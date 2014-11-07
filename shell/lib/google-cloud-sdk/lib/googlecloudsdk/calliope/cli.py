# Copyright 2013 Google Inc. All Rights Reserved.

"""The calliope CLI/API is a framework for building library interfaces."""

import os
import re
import sys
import uuid
import argcomplete

from googlecloudsdk.calliope import actions
from googlecloudsdk.calliope import backend
from googlecloudsdk.calliope import frontend
from googlecloudsdk.core import log
from googlecloudsdk.core import metrics


class CLILoader(object):
  """A class to encapsulate loading the CLI and bootstrapping the REPL."""

  # Splits a path like foo.bar.baz into 2 groups: foo.bar, and baz.  Group 1 is
  # optional.
  PATH_RE = r'(?:([\w\.]+)\.)?([^\.]+)'

  def __init__(self, name, command_root_directory,
               allow_non_existing_modules=False, load_context=None,
               logs_dir=None, version_func=None,
               help_func=None):
    """Initialize Calliope.

    Args:
      name: str, The name of the top level command, used for nice error
        reporting.
      command_root_directory: str, The path to the directory containing the main
        CLI module.
      allow_non_existing_modules: True to allow extra module directories to not
        exist, False to raise an exception if a module does not exist.
      load_context: A function that returns a context dict, or None for a
        default which always returns {}.
      logs_dir: str, The path to the root directory to store logs in, or None
        for no log files.
      version_func: func, A function to call for a top-level -v and
        --version flag. If None, no flags will be available.
      help_func: func([command path]), A function to call for in-depth help
        messages. It is passed the set of subparsers used (not including the
        top-level command). After it is called calliope will exit. This function
        will be called when a top-level 'help' command is run, or when the
        --help option is added on to any command.

    Raises:
      backend.LayoutException: If no command root directory is given.
    """
    self.__name = name
    self.__command_root_directory = command_root_directory
    if not self.__command_root_directory:
      raise backend.LayoutException(
          'You must specify a command root directory.')

    self.__allow_non_existing_modules = allow_non_existing_modules

    self.__config_hooks = backend.ConfigHooks(load_context=load_context)
    self.__logs_dir = logs_dir
    self.__version_func = version_func
    self.__help_func = help_func

    self.__pre_run_hooks = []
    self.__post_run_hooks = []

    self.__top_level_command = None
    self.__modules = []

  def SetTopLevelCommand(self, name):
    """Sets the name of the top level command for single command CLIs.

    If you are making a CLI with no subgroups, use this to set the name of the
    command to use from the command root directory.

    Args:
      name: str, The name of the command to add.  This must correspond to a
        <name>.py file in the command root directory.

    Raises:
      backend.LayoutException: If modules have already been added.
    """
    if self.__modules:
      raise backend.LayoutException(
          'You cannot set a top level command because command modules have '
          'already been added.')
    self.__top_level_command = name

  def AddModule(self, name, path):
    """Adds a module to this CLI tool.

    If you are making a CLI that has subgroups, use this to add in more
    directories of commands.

    Args:
      name: str, The name of the group to create under the main CLI.  If this is
        to be placed under another group, a dotted name can be used.
      path: str, The full path the directory containing the commands for this
        group.

    Raises:
      backend.LayoutException: If a top level command has already been added.
    """
    if self.__top_level_command:
      raise backend.LayoutException(
          'You cannot add a module because a top level command has already '
          'been set.')
    self.__modules.append((name, path))

  def RegisterPreRunHook(self, func,
                         include_commands=None, exclude_commands=None):
    """Register a function to be run before command execution.

    Args:
      func: function, The no args function to run.
      include_commands: str, A regex for the command paths to run.  If not
        provided, the hook will be run for all commands.
      exclude_commands: str, A regex for the command paths to exclude.  If not
        provided, nothing will be excluded.
    """
    hook = frontend.RunHook(func, include_commands, exclude_commands)
    self.__pre_run_hooks.append(hook)

  def RegisterPostRunHook(self, func,
                          include_commands=None, exclude_commands=None):
    """Register a function to be run after command execution.

    Args:
      func: function, The no args function to run.
      include_commands: str, A regex for the command paths to run.  If not
        provided, the hook will be run for all commands.
      exclude_commands: str, A regex for the command paths to exclude.  If not
        provided, nothing will be excluded.
    """
    hook = frontend.RunHook(func, include_commands, exclude_commands)
    self.__post_run_hooks.append(hook)

  def Generate(self):
    """Uses the registered information to generate the CLI tool.

    Returns:
      CLI, The generated CLI tool.
    """
    if self.__top_level_command:
      return self.__LoadCLIFromSingleCommand()
    return self.__LoadCLIFromGroups()

  def __LoadCLIFromSingleCommand(self):
    """Load the CLI from a single command.

    When loaded for a single command, there are no groups and no global
    arguments.  This is use when a calliope command needs to be made a
    standalone command.

    Raises:
      backend.LayoutException: If the top level command file does not exist.

    Returns:
      CLI, The generated CLI tool.
    """
    if not self.__top_level_command:
      raise backend.LayoutException('No top level command registered.')
    file_path = os.path.join(self.__command_root_directory,
                             self.__top_level_command + '.py')
    if not os.path.isfile(file_path):
      raise backend.LayoutException(
          'The given command does not exist: {}'.format(file_path))
    top_command = backend.Command(
        self.__command_root_directory, [self.__top_level_command],
        [self.__name], uuid.uuid4().hex, self.__config_hooks, parser_group=None,
        help_func=self.__help_func)
    parser = top_command.Parser()
    entry_point = frontend.Command(None, top_command)

    return self.__MakeCLI(entry_point, parser, top_command)

  def __LoadCLIFromGroups(self):
    """Load the CLI from a command directory.

    Returns:
      CLI, The generated CLI tool.
    """
    top_group = self.__LoadGroup(self.__command_root_directory, None,
                                 allow_non_existing_modules=False)
    registered_groups = {None: top_group}
    self.__RegisterAllSubGroups(top_group, registered_groups)

    for module_dot_path, module_dir in self.__modules:
      try:
        match = re.match(CLILoader.PATH_RE, module_dot_path)
        root, name = match.group(1, 2)
        parent_group = registered_groups.get(root)
        exception_if_present = None
        if not parent_group:
          exception_if_present = backend.LayoutException(
              'Root [{root}] for command group [{group}] does not exist.'
              .format(root=root, group=name))

        path_list = module_dot_path.split('.')
        group = self.__LoadGroup(
            module_dir, parent_group, module_path=path_list,
            allow_non_existing_modules=self.__allow_non_existing_modules,
            exception_if_present=exception_if_present, top_group=top_group)
        if group:
          self.__RegisterAllSubGroups(group, registered_groups)
          parent_group.AddSubGroup(group)
      except backend.CommandLoadFailure as e:
        log.exception(e)

    parser = top_group.Parser()
    entry_point = frontend.UnboundCommandGroup(None, top_group)
    cli = self.__MakeCLI(entry_point, parser, top_group)

    top_group.MakeShellActions(cli)

    return cli

  def __RegisterAllSubGroups(self, group, registered_groups):
    for g in self.__GetAllGroups(group):
      # pylint: disable=protected-access
      registered_groups['.'.join(g._path[1:])] = g

  def __GetAllGroups(self, starting_group, groups=None):
    if not groups:
      groups = []
    groups.append(starting_group)
    for g in starting_group.groups:
      self.__GetAllGroups(g, groups)
    return groups

  def __LoadGroup(self, module_directory, parent_group, module_path=None,
                  allow_non_existing_modules=False, exception_if_present=None,
                  top_group=None):
    """Loads a single command group from a directory.

    Args:
      module_directory: The path to the location of the module
      parent_group: backend.CommandGroup, The parent command group for this
        command group, or None if this is the top group.
      module_path: An optional name override for the module. If not set, it will
        default to using the name of the directory containing the module.
      allow_non_existing_modules: True to allow this module directory to not
        exist, False to raise an exception if this module does not exist.
      exception_if_present: Exception, An exception to throw if the module
        actually exists, or None.
      top_group: backend.CommandGroup, The top command group for this CLI.

    Raises:
      LayoutException: If the module directory does not exist and
      allow_non_existing is False.

    Returns:
      The backend.CommandGroup object, or None if the module directory does not
      exist and allow_non_existing is True.
    """
    if not os.path.isdir(module_directory):
      if allow_non_existing_modules:
        return None
      raise backend.LayoutException(
          'The given module directory does not exist: {}'.format(
              module_directory))
    elif exception_if_present:
      # pylint: disable=raising-bad-type, This will be an actual exception.
      raise exception_if_present

    module_root, module = os.path.split(module_directory)
    if not module_path:
      module_path = [module]
    # If this is the top level, don't register the name of the module directory
    # itself, it should assume the name of the command.  If this is another
    # module directory, its name gets explicitly registered under the root
    # command.
    is_top = not parent_group
    sub_parser = parent_group.SubParser() if parent_group else None
    path = [self.__name] if is_top else [self.__name] + module_path
    group = backend.CommandGroup(
        module_root, [module], path, uuid.uuid4().hex, sub_parser,
        self.__config_hooks, help_func=self.__help_func,
        parent_group=top_group)

    return group

  def __MakeCLI(self, entry_point, parser, top_element):
    """Generate a CLI object from the given data.

    Args:
      entry_point: The REPL entrypoint for this CLI.
      parser: The argparse parser for the top of this command tree.
      top_element: The top element of the command tree
        (that extends backend.CommandCommon).

    Returns:
      CLI, The generated CLI tool.
    """
    if self.__version_func is not None:
      parser.add_argument(
          '-v', '--version',
          action=actions.FunctionExitAction(self.__version_func),
          help='Print version information.')
    # pylint: disable=protected-access
    top_element._ai.add_argument(
        '--verbosity',
        choices=log.OrderedVerbosityNames(),
        default=None,
        help='Override the default verbosity for this command.  This must be '
        'a standard logging verbosity level: [{values}] (Default: [{default}]).'
        .format(values=', '.join(log.OrderedVerbosityNames()),
                default=log.DEFAULT_VERBOSITY_STRING))
    top_element._ai.add_argument(
        '--user-output-enabled',
        default=None,
        choices=('true', 'false'),
        help='Control whether user intended output is printed to the console.  '
        '(true/false)')

    if '_ARGCOMPLETE' not in os.environ:
      # Don't bother setting up logging if we are just doing a completion.
      log.AddFileLogging(self.__logs_dir)

    return CLI(entry_point, parser, self.__pre_run_hooks, self.__post_run_hooks)


class CLI(object):
  """A generated command line tool."""

  def __init__(self, entry_point, parser, pre_run_hooks, post_run_hooks):
    self.__entry_point = entry_point
    self.__parser = parser
    self.__pre_run_hooks = pre_run_hooks
    self.__post_run_hooks = post_run_hooks
    self.args = []

  def _ArgComplete(self):
    argcomplete.autocomplete(self.__parser, always_complete_options=False)

  def Execute(self, args=None):
    """Execute the CLI tool with the given arguments.

    Args:
      args: The arguments from the command line or None to use sys.argv

    Returns:
      The result of executing the command determined by the command
      implementation.
    """
    self._ArgComplete()

    self.argv = args or sys.argv[1:]
    args = self.__parser.parse_args(self.argv)
    command_path_string = '.'.join(args.command_path)

    # TODO(user): put a real version here
    metrics.Commands(command_path_string, None)
    path = args.command_path[1:]
    kwargs = args.__dict__

    # Dig down into the groups and commands, binding the arguments at each step.
    # If the path is empty, this means that we have an actual command as the
    # entry point and we don't need to dig down, just call it directly.

    # The command_path will be, eg, ['top', 'group1', 'group2', 'command'], and
    # is set by each backend.Command when it's loaded from
    # 'tools/group1/group2/command.py'. It corresponds also to the python object
    # built to mirror the command line, with 'top' corresponding to the
    # entry point returned by the EntryPoint() method. Then, in this case, the
    # object found with self.EntryPoint().group1.group2.command is the runnable
    # command being targetted by this operation. The following code segment
    # does this digging and applies the relevant arguments at each step, taken
    # from the argparse results.

    # pylint: disable=protected-access
    cur = self.EntryPoint()
    while path:
      cur = cur._BindArgs(kwargs=kwargs, cli_mode=True)
      cur = getattr(cur, path[0])
      path = path[1:]

    return cur._Execute(cli_mode=True, pre_run_hooks=self.__pre_run_hooks,
                        post_run_hooks=self.__post_run_hooks, kwargs=kwargs)

  def EntryPoint(self):
    """Get the top entry point into the REPL for interactive mode.

    Returns:
      A REPL command group that allows you to bind args and call commands
      interactively in the same way you would from the command line.
    """
    return self.__entry_point
