# Copyright 2013 Google Inc. All Rights Reserved.

"""Backend stuff for the calliope.cli module.

Not to be used by mortals.

"""

import argparse
import imp
import os
import re
import sys


from googlecloudsdk.calliope import actions
from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import markdown
from googlecloudsdk.calliope import shell
from googlecloudsdk.calliope import usage_text
from googlecloudsdk.core import exceptions as core_exceptions
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.core.util import resource_printer


class ArgumentException(Exception):
  """ArgumentException is for problems with the provided arguments."""
  pass


class MissingArgumentException(ArgumentException):
  """An exception for when required arguments are not provided."""

  def __init__(self, command_path, missing_arguments):
    """Creates a new MissingArgumentException.

    Args:
      command_path: A list representing the command or group that had the
          required arguments
      missing_arguments: A list of the arguments that were not provided
    """
    message = ('The following required arguments were not provided for command '
               '[{0}]: [{1}]'.format('.'.join(command_path),
                                     ', '.join(missing_arguments)))
    super(MissingArgumentException, self).__init__(message)


class UnexpectedArgumentException(ArgumentException):
  """An exception for when required arguments are not provided."""

  def __init__(self, command_path, unexpected_arguments):
    """Creates a new UnexpectedArgumentException.

    Args:
      command_path: A list representing the command or group that was given the
          unexpected arguments
      unexpected_arguments: A list of the arguments that were not valid
    """
    message = ('The following arguments were unexpected for command '
               '[{0}]: [{1}]'.format('.'.join(command_path),
                                     ', '.join(unexpected_arguments)))
    super(UnexpectedArgumentException, self).__init__(message)


class LayoutException(Exception):
  """LayoutException is for problems with module directory structure."""
  pass


class CommandLoadFailure(Exception):
  """An exception for when a command or group module cannot be imported."""

  def __init__(self, command, root_exception):
    self.command = command
    self.root_exception = root_exception
    super(CommandLoadFailure, self).__init__(
        'Problem loading {command}: {issue}.'.format(
            command=command, issue=str(root_exception)))


class ArgumentInterceptor(object):
  """ArgumentInterceptor intercepts calls to argparse parsers.

  The argparse module provides no public way to access a complete list of
  all arguments, and we need to know these so we can do validation of arguments
  when this library is used in the python interpreter mode. Argparse itself does
  the validation when it is run from the command line.

  Attributes:
    parser: argparse.Parser, The parser whose methods are being intercepted.
    allow_positional: bool, Whether or not to allow positional arguments.
    defaults: {str:obj}, A dict of {dest: default} for all the arguments added.
    required: [str], A list of the dests for all required arguments.
    dests: [str], A list of the dests for all arguments.
    positional_args: [argparse.Action], A list of the positional arguments.
    flag_args: [argparse.Action], A list of the flag arguments.

  Raises:
    ArgumentException: if a positional argument is made when allow_positional
        is false.
  """

  class ParserData(object):

    def __init__(self):
      self.defaults = {}
      self.required = []
      self.dests = []
      self.mutex_groups = {}
      self.positional_args = []
      self.flag_args = []
      self.ancestor_flag_args = []

  def __init__(self, parser, allow_positional, data=None, mutex_group_id=None):
    self.parser = parser
    self.allow_positional = allow_positional
    self.data = data or ArgumentInterceptor.ParserData()
    self.mutex_group_id = mutex_group_id

  @property
  def defaults(self):
    return self.data.defaults

  @property
  def required(self):
    return self.data.required

  @property
  def dests(self):
    return self.data.dests

  @property
  def mutex_groups(self):
    return self.data.mutex_groups

  @property
  def positional_args(self):
    return self.data.positional_args

  @property
  def flag_args(self):
    return self.data.flag_args

  @property
  def ancestor_flag_args(self):
    return self.data.ancestor_flag_args

  # pylint: disable=g-bad-name
  def add_argument(self, *args, **kwargs):
    """add_argument intercepts calls to the parser to track arguments."""
    # TODO(user): do not allow short-options without long-options.

    # we will choose the first option as the name
    name = args[0]

    positional = not name.startswith('-')
    if positional and not self.allow_positional:
      # TODO(user): More informative error message here about which group
      # the problem is in.
      raise ArgumentException('Illegal positional argument: ' + name)

    if positional and '-' in name:
      raise ArgumentException(
          "Positional arguments cannot contain a '-': " + name)

    dest = kwargs.get('dest')
    if not dest:
      # this is exactly what happens in argparse
      dest = name.lstrip(self.parser.prefix_chars).replace('-', '_')
    default = kwargs.get('default')
    required = kwargs.get('required')

    self.defaults[dest] = default
    if required:
      self.required.append(dest)
    self.dests.append(dest)
    if self.mutex_group_id:
      self.mutex_groups[dest] = self.mutex_group_id

    if positional and 'metavar' not in kwargs:
      kwargs['metavar'] = name.upper()

    added_argument = self.parser.add_argument(*args, **kwargs)

    if positional:
      self.positional_args.append(added_argument)
    else:
      self.flag_args.append(added_argument)

    return added_argument

  # pylint: disable=redefined-builtin
  def register(self, registry_name, value, object):
    return self.parser.register(registry_name, value, object)

  def set_defaults(self, **kwargs):
    return self.parser.set_defaults(**kwargs)

  def get_default(self, dest):
    return self.parser.get_default(dest)

  def add_argument_group(self, *args, **kwargs):
    new_parser = self.parser.add_argument_group(*args, **kwargs)
    return ArgumentInterceptor(parser=new_parser,
                               allow_positional=self.allow_positional,
                               data=self.data)

  def add_mutually_exclusive_group(self, **kwargs):
    new_parser = self.parser.add_mutually_exclusive_group(**kwargs)
    return ArgumentInterceptor(parser=new_parser,
                               allow_positional=self.allow_positional,
                               data=self.data,
                               mutex_group_id=id(new_parser))

  def AddFlagActionFromAncestors(self, action):
    """Add a flag action to this parser, but segregate it from the others.

    Segregating the action allows automatically generated help text to ignore
    this flag.

    Args:
      action: argparse.Action, The action for the flag being added.

    """
    # pylint:disable=protected-access, simply no other way to do this.
    self.parser._add_action(action)
    # explicitly do this second, in case ._add_action() fails.
    self.data.ancestor_flag_args.append(action)


class ConfigHooks(object):
  """This class holds function hooks for context and config loading/saving."""

  def __init__(
      self,
      load_context=None,
      context_filters=None,
      group_class=None):
    """Create a new object with the given hooks.

    Args:
      load_context: a function returns the context to be sent to commands.
      context_filters: a list of functions that take (contex, args),
          that will be called in order before a command is run. They are
          described in the README under the heading GROUP SPECIFICATION.
      group_class: base.Group, The class that this config hooks object is for.
    """
    self.load_context = load_context if load_context else lambda: {}
    self.context_filters = context_filters if context_filters else []
    self.group_class = group_class

  def OverrideWithBase(self, group_base):
    """Get a new ConfigHooks object with overridden functions based on module.

    If module defines any of the function, they will be used instead of what
    is in this object.  Anything that is not defined will use the existing
    behavior.

    Args:
      group_base: The base.Group class corresponding to the group.

    Returns:
      A new ConfigHooks object updated with any newly found hooks
    """

    def ContextFilter(context, args):
      group = group_base()
      group.Filter(context, args)
      return group
    # We want the new_context_filters to be a completely new list, if there is
    # a change.
    new_context_filters = self.context_filters + [ContextFilter]
    return ConfigHooks(load_context=self.load_context,
                       context_filters=new_context_filters,
                       group_class=group_base)


class CommandCommon(object):
  """A base class for CommandGroup and Command.

  It is responsible for extracting arguments from the modules and does argument
  validation, since this is always the same for groups and commands.
  """

  def __init__(self, module_dir, module_path, path, construction_id,
               config_hooks, help_func, parser_group, allow_positional_args,
               parent_group):
    """Create a new CommandCommon.

    Args:
      module_dir: str, The path to the tools directory that this command or
          group lives within. Used to find the command or group's source file.
      module_path: [str], The command group names that brought us down to this
          command group or command from the top module directory.
      path: [str], Similar to module_path, but is the path to this command or
          group with respect to the CLI itself.  This path should be used for
          things like error reporting when a specific element in the tree needs
          to be referenced.
      construction_id: str, A unique identifier for the CLILoader that is
          being constructed.
      config_hooks: a ConfigHooks object to use for loading context.
      help_func: func([command path]), A function to call with --help.
      parser_group: argparse.Parser, The parser that this command or group will
          live in.
      allow_positional_args: bool, True if this command can have positional
          arguments.
      parent_group: CommandGroup, The parent of this command or group. None if
          at the root.
    """
    module = self._GetModuleFromPath(module_dir, module_path, path,
                                     construction_id)

    self._help_func = help_func
    self._config_hooks = config_hooks
    self._parent_group = parent_group

    # pylint:disable=protected-access, The base module is effectively an
    # extension of calliope, and we want to leave _Common private so people
    # don't extend it directly.
    common_type = base._Common.FromModule(module)

    self.name = path[-1]
    # For the purposes of argparse and the help, we should use dashes.
    self.cli_name = self.name.replace('_', '-')
    path[-1] = self.cli_name
    self._module_path = module_path
    self._path = path
    self._construction_id = construction_id

    self._common_type = common_type
    self._common_type.group_class = config_hooks.group_class

    (self.short_help, self.long_help) = usage_text.ExtractHelpStrings(
        self._common_type.__doc__)

    self.detailed_help = getattr(self._common_type, 'detailed_help', {})

    self._AssignParser(
        parser_group=parser_group,
        help_func=help_func,
        allow_positional_args=allow_positional_args)

  def _AssignParser(self, parser_group, help_func, allow_positional_args):
    """Assign a parser group to model this Command or CommandGroup.

    Args:
      parser_group: argparse._ArgumentGroup, the group that will model this
          command or group's arguments.
      help_func: func([str]), The long help function that is used for --help.
      allow_positional_args: bool, Whether to allow positional args for this
          group or not.

    """
    if not parser_group:
      # This is the root of the command tree, so we create the first parser.
      self._parser = argparse.ArgumentParser(description=self.long_help,
                                             add_help=False,
                                             prog='.'.join(self._path))
    else:
      # This is a normal sub group, so just add a new subparser to the existing
      # one.
      self._parser = parser_group.add_parser(
          self.cli_name,
          help=self.short_help,
          description=self.long_help,
          add_help=False,
          prog='.'.join(self._path))

    # pylint:disable=protected-access
    self._parser._check_value = usage_text.CheckValueAndSuggest
    self._parser.error = usage_text.PrintParserError(self._parser)

    self._sub_parser = None

    self._ai = ArgumentInterceptor(
        parser=self._parser,
        allow_positional=allow_positional_args)

    self._short_help_action = actions.ShortHelpAction(self, self._ai)

    self._ai.add_argument(
        '-h', action=self._short_help_action,
        help='Print a summary help and exit.')

    if help_func:
      def LongHelp():
        help_func(
            self._path,
            default=usage_text.ShortHelpText(self, self._ai))
      long_help_action = actions.FunctionExitAction(LongHelp)
    else:
      long_help_action = self._short_help_action

    self._ai.add_argument(
        '--help', action=long_help_action,
        help='Display detailed help.')

    def Markdown(command):
      """Returns an action that lists the markdown help for command."""

      class Action(argparse.Action):

        def __call__(self, parser, namespace, values, option_string=None):
          markdown.Markdown(command, sys.stdout.write)
          sys.exit(0)

      return Action

    self._ai.add_argument(
        '--markdown', action=Markdown(self),
        nargs=0,
        help=argparse.SUPPRESS)

    if not parser_group:
      format_flag = self._ai.add_argument(
          '--format', help='Format for printed output.',
          choices=resource_printer.SUPPORTED_FORMATS)
      format_flag.detailed_help = """\
          Specify a format for printed output. By default, a command-specific
          human-friendly output format is used. Setting this flag to one of
          the available options will serialize the result of the command in
          the chosen format and print it to stdout. Supported formats are:
          `{0}`.""".format('`, `'.join(resource_printer.SUPPORTED_FORMATS))

    self._parser.parse_args = usage_text.ErrFuncParseArgs(self._parser)
    self._AcquireArgs()

  def GetPath(self):
    return self._path

  def GetDocString(self):
    if self.long_help:
      return self.long_help
    if self.short_help:
      return self.short_help
    return 'The {name} command.'.format(name=self.name)

  def GetShortHelp(self):
    return usage_text.ShortHelpText(self, self._ai)

  def GetSubCommandHelps(self):
    return {}

  def GetSubGroupHelps(self):
    return {}

  def _GetModuleFromPath(self, module_dir, module_path, path, construction_id):
    """Import the module and dig into it to return the namespace we are after.

    Import the module relative to the top level directory.  Then return the
    actual module corresponding to the last bit of the path.

    Args:
      module_dir: str, The path to the tools directory that this command or
        group lives within.
      module_path: [str], The command group names that brought us down to this
        command group or command from the top module directory.
      path: [str], The same as module_path but with the groups named as they
        will be in the CLI.
      construction_id: str, A unique identifier for the CLILoader that is
        being constructed.

    Returns:
      The imported module.
    """
    src_dir = os.path.join(module_dir, *module_path[:-1])
    f = None
    m = imp.find_module(module_path[-1], [src_dir])
    f, file_path, items = m
    # Make sure this module name never collides with any real module name.
    # Use the CLI naming path, so values are always unique.
    name = '__calliope__command__.{construction_id}.{name}'.format(
        construction_id=construction_id,
        name='.'.join(path).replace('-', '_'))
    try:
      module = imp.load_module(name, f, file_path, items)
    # pylint:disable=broad-except, We really do want to catch everything here,
    # because if any exceptions make it through for any single command or group
    # file, the whole CLI will not work. Instead, just log whatever it is.
    except Exception as e:
      _, _, exc_traceback = sys.exc_info()
      raise CommandLoadFailure('.'.join(path), e), None, exc_traceback
    finally:
      if f:
        f.close()
    return module

  def _AcquireArgs(self):
    """Call the function to register the arguments for this module."""
    args_func = self._common_type.Args
    if not args_func:
      return
    args_func(self._ai)

    if self._parent_group:
      # Add parent flags to children, if they aren't represented already
      for flag in self._parent_group.GetAllAvailableFlags():
        if flag.option_strings in [['-h'], ['--help'], ['-h', '--help'],
                                   ['--markdown']]:
          # Each command or group gets its own unique help flags.
          continue
        if flag.required:
          # It is not easy to replicate required flags to subgroups and
          # subcommands, since then there would be two+ identical required
          # flags, and we'd want only one of them to be necessary.
          continue
        try:
          self._ai.AddFlagActionFromAncestors(flag)
        except argparse.ArgumentError:
          raise ArgumentException(
              'repeated flag in {command}: {flag}'.format(
                  command='.'.join(self._path),
                  flag=flag.option_strings))

  def GetAllAvailableFlags(self):
    return self._ai.flag_args + self._ai.ancestor_flag_args

  def _GetSubPathsForNames(self, names):
    """Gets a list of (module path, path) for the given list of sub names.

    Args:
      names: The names of the sub groups or commands the paths are for

    Returns:
      A list of tuples of the new (module_path, path) for the given names.
      These terms are that as used by the constructor of CommandGroup and
      Command.
    """
    return [(self._module_path + [name], self._path + [name]) for name in names]

  def Parser(self):
    """Return the argparse parser this group is using.

    Returns:
      The argparse parser this group is using
    """
    return self._parser

  def SubParser(self):
    """Gets or creates the argparse sub parser for this group.

    Returns:
      The argparse subparser that children of this group should register with.
          If a sub parser has not been allocated, it is created now.
    """
    if not self._sub_parser:
      self._sub_parser = self._parser.add_subparsers()
    return self._sub_parser

  def CreateNewArgs(self, kwargs, current_args, cli_mode):
    """Make a new argument dictionary from default, existing, and new args.

    Args:
      kwargs: The keyword args the user provided for this level
      current_args: The arguments that have previously been collected at other
          levels
      cli_mode: True if we are doing arg parsing for cli mode.

    Returns:
      A new argument dictionary
    """
    if cli_mode:
      # We are binding one big dictionary of arguments.  Filter out all the
      # arguments that don't belong to this level.
      filtered_kwargs = {}
      for key, value in kwargs.iteritems():
        if key in self._ai.dests:
          filtered_kwargs[key] = value
      kwargs = filtered_kwargs

    # Make sure the args provided at this level are OK.
    self._ValidateArgs(kwargs, cli_mode)
    # Start with the defaults arguments for this level.
    new_args = dict(self._ai.defaults)
    # Add in anything that was already collected above us in the tree.
    new_args.update(current_args)
    # Add in the args from this invocation.
    new_args.update(kwargs)
    return new_args

  def _ValidateArgs(self, args, cli_mode):
    """Make sure the given arguments are correct for this level.

    Ensures that any required args are provided as well as that no unexpected
    arguments were provided.

    Args:
      args:  A dictionary of the arguments that were provided
      cli_mode: True if we are doing arg parsing for cli mode.

    Raises:
      ArgumentException: If mutually exclusive arguments were both given.
      MissingArgumentException: If there are missing required arguments.
      UnexpectedArgumentException: If there are unexpected arguments.
    """
    missed_args = []
    for required in self._ai.required:
      if required not in args:
        missed_args.append(required)
    if missed_args:
      raise MissingArgumentException(self._path, missed_args)

    unexpected_args = []
    for dest in args:
      if dest not in self._ai.dests:
        unexpected_args.append(dest)
    if unexpected_args:
      raise UnexpectedArgumentException(self._path, unexpected_args)

    if not cli_mode:
      # We only need to do mutex group detections when binding args manually.
      # Argparse will take care of this when on the CLI.
      found_groups = {}
      group_ids = self._ai.mutex_groups
      for dest in sorted(args):
        group_id = group_ids.get(dest)
        if group_id:
          found = found_groups.get(group_id)
          if found:
            raise ArgumentException('Argument {0} is not allowed with {1}'
                                    .format(dest, found))
          found_groups[group_id] = dest


class CommandGroup(CommandCommon):
  """A class to encapsulate a group of commands."""

  def __init__(self, module_dir, module_path, path, construction_id,
               parser_group, config_hooks, help_func, parent_group=None):
    """Create a new command group.

    Args:
      module_dir: always the root of the whole command tree
      module_path: a list of command group names that brought us down to this
          command group from the top module directory
      path: similar to module_path, but is the path to this command group
          with respect to the CLI itself.  This path should be used for things
          like error reporting when a specific element in the tree needs to be
          referenced
      construction_id: str, A unique identifier for the CLILoader that is
          being constructed.
      parser_group: the current argparse parser, or None if this is the root
          command group.  The root command group will allocate the initial
          top level argparse parser.
      config_hooks: a ConfigHooks object to use for loading context
      help_func: func([command path]), A function to call with --help.
      parent_group: CommandGroup, The parent of this group. None if at the
          root.

    Raises:
      LayoutException: if the module has no sub groups or commands
    """

    super(CommandGroup, self).__init__(
        module_dir=module_dir,
        module_path=module_path,
        path=path,
        construction_id=construction_id,
        config_hooks=config_hooks,
        help_func=help_func,
        allow_positional_args=False,
        parser_group=parser_group,
        parent_group=parent_group)

    self._module_dir = module_dir

    self._LoadSubGroups()

    self._parser.usage = usage_text.GenerateUsage(self, self._ai)
    self._parser.error = usage_text.PrintShortHelpError(self._parser, self)

  def _LoadSubGroups(self):
    """Load all of this group's subgroups and commands."""
    self._config_hooks = self._config_hooks.OverrideWithBase(self._common_type)

    # find sub groups and commands
    self.groups = []
    self.commands = []
    (group_names, command_names) = self._FindSubGroups()
    self.all_sub_names = set(group_names + command_names)
    if not group_names and not command_names:
      raise LayoutException('Group %s has no subgroups or commands'
                            % '.'.join(self._path))

    # recursively create the tree of command groups and commands
    sub_parser = self.SubParser()
    for (new_module_path, new_path) in self._GetSubPathsForNames(group_names):
      self.groups.append(
          CommandGroup(self._module_dir, new_module_path, new_path,
                       self._construction_id, sub_parser, self._config_hooks,
                       help_func=self._help_func,
                       parent_group=self))

    for (new_module_path, new_path) in self._GetSubPathsForNames(command_names):
      cmd = Command(self._module_dir, new_module_path, new_path,
                    self._construction_id, self._config_hooks, sub_parser,
                    self._help_func, parent_group=self)
      self.commands.append(cmd)

  def MakeShellActions(self, loader):
    group_names = [group.name.replace('_', '-') for group in self.groups]
    command_names = [command.name.replace('_', '-')
                     for command in self.commands]
    self._ai.add_argument(
        '--shell',
        action=shell.ShellAction(group_names + command_names, loader),
        nargs='?',
        help=argparse.SUPPRESS)
    for group in self.groups:
      group.MakeShellActions(loader)

  def GetSubCommandHelps(self):
    return dict((item.cli_name, item.short_help or '')
                for item in self.commands)

  def GetSubGroupHelps(self):
    return dict((item.cli_name, item.short_help or '')
                for item in self.groups)

  def GetHelpFunc(self):
    return self._help_func

  def AddSubGroup(self, group):
    """Merges another command group under this one.

    If we load command groups for alternate locations, this method is used to
    make those extra sub groups fall under this main group in the CLI.

    Args:
      group: Any other CommandGroup object that should be added to the CLI
    """
    self.groups.append(group)
    self.all_sub_names.add(group.name)
    self._parser.usage = usage_text.GenerateUsage(self, self._ai)

  def IsValidSubName(self, name):
    """See if the given name is a name of a registered sub group or command.

    Args:
      name: The name to check

    Returns:
      True if the given name is a registered sub group or command of this
      command group.
    """
    return name in self.all_sub_names

  def _FindSubGroups(self):
    """Final all the sub groups and commands under this group.

    Returns:
      A tuple containing two lists. The first is a list of strings for each
      command group, and the second is a list of strings for each command.

    Raises:
      LayoutException: if there is a command or group with an illegal name.
    """
    location = os.path.join(self._module_dir, *self._module_path)
    items = os.listdir(location)
    groups = []
    commands = []
    items.sort()
    for item in items:
      name, ext = os.path.splitext(item)
      itempath = os.path.join(location, item)

      if ext == '.py':
        if name == '__init__':
          continue
      elif not os.path.isdir(itempath):
        continue

      if re.search('[A-Z]', name):
        raise LayoutException('Commands and groups cannot have capital letters:'
                              ' %s.' % name)

      if not os.path.isdir(itempath):
        commands.append(name)
      else:
        init_path = os.path.join(itempath, '__init__.py')
        if os.path.exists(init_path):
          groups.append(item)
    return groups, commands


class Command(CommandCommon):
  """A class that encapsulates the configuration for a single command."""

  def __init__(self, module_dir, module_path, path, construction_id,
               config_hooks, parser_group, help_func, parent_group=None):
    """Create a new command.

    Args:
      module_dir: str, The root of the command tree.
      module_path: a list of command group names that brought us down to this
          command from the top module directory
      path: similar to module_path, but is the path to this command with respect
          to the CLI itself.  This path should be used for things like error
          reporting when a specific element in the tree needs to be referenced
      construction_id: str, A unique identifier for the CLILoader that is
          being constructed.
      config_hooks: a ConfigHooks object to use for loading context
      parser_group: argparse.Parser, The parser to be used for this command.
      help_func: func([str]), Detailed help function.
      parent_group: CommandGroup, The parent of this command.
    """
    super(Command, self).__init__(
        module_dir=module_dir,
        module_path=module_path,
        path=path,
        construction_id=construction_id,
        config_hooks=config_hooks,
        help_func=help_func,
        allow_positional_args=True,
        parser_group=parser_group,
        parent_group=parent_group)

    self._parser.set_defaults(
        cmd_func=self.Run, command_path=self._path,
        err_func=usage_text.PrintShortHelpError(self._parser, self))

    self._parser.usage = usage_text.GenerateUsage(self, self._ai)

  def Run(self, args, command=None, cli_mode=False, pre_run_hooks=None,
          post_run_hooks=None):
    """Run this command with the given arguments.

    Args:
      args: The arguments for this command as a namespace.
      command: The bound Command object that is used to run this Command.
      cli_mode: bool, True if running from the command line, False if running
        interactively.
      pre_run_hooks: [_RunHook], Things to run before the command.
      post_run_hooks: [_RunHook], Things to run after the command.

    Returns:
      The object returned by the module's Run() function.

    Raises:
      exceptions.Error: if thrown by the Run() function.
    """
    command_path_string = '.'.join(self._path)

    properties.VALUES.PushArgs(args)
    # Enable user output for CLI mode only if it is not explicitly set in the
    # properties (or given in the provided arguments that were just pushed into
    # the properties object).
    user_output_enabled = properties.VALUES.core.user_output_enabled.GetBool()
    set_user_output_property = cli_mode and user_output_enabled is None
    if set_user_output_property:
      properties.VALUES.core.user_output_enabled.Set(True)
    # Now that we have pushed the args, reload the settings so the flags will
    # take effect.  These will use the values from the properties.
    old_user_output_enabled = log.SetUserOutputEnabled(None)
    old_verbosity = log.SetVerbosity(None)

    try:
      if cli_mode and pre_run_hooks:
        for hook in pre_run_hooks:
          hook.Run(command_path_string)

      tool_context = self._config_hooks.load_context()
      last_group = None
      for context_filter in self._config_hooks.context_filters:
        last_group = context_filter(tool_context, args)

      command_instance = self._common_type(
          context=tool_context,
          entry_point=command.EntryPoint(),
          command=command,
          group=last_group)

      def OutputFormatter(obj):
        command_instance.Display(args, obj)
      output_formatter = OutputFormatter

      def Format(obj):
        if not obj:
          return
        resource_printer.Print(obj, args.format or 'yaml', out=log.out)
      command_instance.format = Format
      if args.format:
        output_formatter = command_instance.format

      log.debug('Running %s with %s.', command_path_string, args)
      result = command_instance.Run(args)
      if cli_mode:
        output_formatter(result)

      if cli_mode and post_run_hooks:
        for hook in post_run_hooks:
          hook.Run(command_path_string)

      return result

    except core_exceptions.Error as exc:
      msg = '({0}) {1}'.format(command_path_string, str(exc))
      log.debug(msg, exc_info=sys.exc_info())
      if cli_mode:
        log.error(msg)
        self._Exit(exc)
      else:
        raise
    except Exception as exc:
      # Make sure any uncaught exceptions still make it into the log file.
      log.file_only_logger.debug(str(exc), exc_info=sys.exc_info())
      raise
    finally:
      if set_user_output_property:
        properties.VALUES.core.user_output_enabled.Set(None)
      log.SetUserOutputEnabled(old_user_output_enabled)
      log.SetVerbosity(old_verbosity)
      properties.VALUES.PopArgs()

  def _Exit(self, unused_exc):
    """This method exists so we can mock this out during testing to not exit."""
    sys.exit(1)
