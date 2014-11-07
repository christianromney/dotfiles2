# Copyright 2013 Google Inc. All Rights Reserved.

"""The calliope CLI/API is a framework for building library interfaces."""

import pprint
import re


class _Args(object):
  """A helper class to convert a dictionary into an object with properties."""

  def __init__(self, args):
    self.__dict__.update(args)

  def __str__(self):
    return '_Args(%s)' % pprint.pformat(self.__dict__)

  def __iter__(self):
    for key, value in sorted(self.__dict__.iteritems()):
      yield key, value


class UnboundCommandGroup(object):
  """A class to represent an unbound command group in the REPL.

  Unbound refers to the fact that no arguments have been bound to this command
  group yet.  This object can be called with a set of arguments to set them.
  You can also access any sub group or command of this group as a property if
  this group does not require any arguments at this level.
  """

  def __init__(self, parent_group, group):
    """Create a new UnboundCommandGroup.

    Args:
      parent_group: The BoundCommandGroup this is a descendant of or None if
          this is the root command.
      group: The backend.CommandGroup that this object is representing
    """
    self._parent_group = parent_group
    self._group = group

    # We change the .__doc__ so that when calliope is used in interpreter mode,
    # the user can inspect .__doc__ and get the help messages provided by the
    # tool creator.
    self.__doc__ = self._group.GetDocString()

  def ParentGroup(self):
    """Gives you the bound command group this group is a descendant of.

    Returns:
      The BoundCommandGroup above this one in the tree or None if we are the top
    """
    return self._parent_group

  def GetShortHelp(self):
    return self._group.GetShortHelp()

  def __call__(self, **kwargs):
    return self._BindArgs(kwargs=kwargs, cli_mode=False)

  def _BindArgs(self, kwargs, cli_mode):
    """Bind arguments to this command group.

    This is called with the kwargs to bind to this command group.  It validates
    that the group has registered the provided args and that any required args
    are provided.

    Args:
      kwargs: The args to bind to this command group.
      cli_mode: True if we are doing arg parsing for cli mode.

    Returns:
      A new BoundCommandGroup with the given arguments
    """
    # pylint: disable=protected-access, We don't want to expose the member or an
    # accessor since this is a user facing class.  These three classes all work
    # as a single unit.
    current_args = self._parent_group._args if self._parent_group else {}
    # Compute the new argument bindings for what was just provided.
    new_args = self._group.CreateNewArgs(
        kwargs=kwargs,
        current_args=current_args,
        cli_mode=cli_mode)

    bound_group = BoundCommandGroup(self, self._group, self._parent_group,
                                    new_args, kwargs)
    return bound_group

  def __getattr__(self, name):
    """Access sub groups or commands without using () notation.

    Accessing a sub group or command without using the above call, implicitly
    executes the binding with no arguments.  If the context has required
    arguments, this will fail.

    Args:
      name: the name of the attribute to get

    Returns:
      A new UnboundCommandGroup or Command created by binding this command group
      with no arguments.

    Raises:
      AttributeError: if the given name is not a valid sub group or command
    """
    # Map dashes in the CLI to underscores in the API.
    name = name.replace('-', '_')
    if self._group.IsValidSubName(name):
      # Bind zero arguments to this group and then get the name we actually
      # asked for
      return getattr(self._BindArgs(kwargs={}, cli_mode=False), name)
    raise AttributeError(name)

  def Name(self):
    return self._group.name

  def HelpFunc(self):
    return self._group.GetHelpFunc()

  def __repr__(self):
    s = ''
    if self._parent_group:
      s += '%s.' % repr(self._parent_group)
    s += self.Name()
    return s


class BoundCommandGroup(object):
  """A class to represent a bound command group in the REPL.

  Bound refers to the fact that arguments have already been provided for this
  command group.  You can access sub groups or commands of this group as
  properties.
  """

  def __init__(self, unbound_group, group, parent_group, args, new_args):
    """Create a new BoundCommandGroup.

    Args:
      unbound_group: the UnboundCommandGroup that this BoundCommandGroup was
          created from.
      group: The backend.CommandGroup equivalent for this group.
      parent_group: The BoundCommandGroup this is a descendant of
      args: All the default and provided arguments from above and including
          this group.
      new_args: The args used to bind this command group, not including those
          from its parent groups.
    """
    self._unbound_group = unbound_group
    self._group = group
    self._parent_group = parent_group
    self._args = args
    self._new_args = new_args
    # Create attributes for each sub group or command that can come next.
    for group in self._group.groups:
      setattr(self, group.name, UnboundCommandGroup(self, group))
    for command in self._group.commands:
      setattr(self, command.name, Command(self, command))

    self.__doc__ = self._group.GetDocString()

  def __getattr__(self, name):
    # Map dashes in the CLI to underscores in the API.
    fixed_name = name.replace('-', '_')
    if name == fixed_name:
      raise AttributeError
    return getattr(self, fixed_name)

  def UnboundGroup(self):
    return self._unbound_group

  def ParentGroup(self):
    """Gives you the bound command group this group is a descendant of.

    Returns:
      The BoundCommandGroup above this one in the tree or None if we are the top
    """
    return self._parent_group

  def GetShortHelp(self):
    return self._group.GetShortHelp()

  def __repr__(self):
    s = ''
    if self._parent_group:
      s += '%s.' % repr(self._parent_group)
    s += self._group.name

    # There are some things in the args which are set by default, like cmd_func
    # and command_path, which should not appear in the repr.
    # pylint:disable=protected-access
    valid_args = self._group._ai.dests
    args = ', '.join(['{0}={1}'.format(arg, repr(val))
                      for arg, val in self._new_args.iteritems()
                      if arg in valid_args])
    if args:
      s += '(%s)' % args
    return s


class Command(object):
  """A class representing a command that can be called in the REPL.

  At this point, contexts about this command have already been created and bound
  to any required arguments for those command groups.  This object can be called
  to actually invoke the underlying command.
  """

  def __init__(self, parent_group, command):
    """Create a new Command.

    Args:
      parent_group: The BoundCommandGroup this is a descendant of
      command: The backend.Command object to actually invoke
    """
    self._parent_group = parent_group
    self._command = command

    # We change the .__doc__ so that when calliope is used in interpreter mode,
    # the user can inspect .__doc__ and get the help messages provided by the
    # tool creator.
    self.__doc__ = self._command.GetDocString()

  def ParentGroup(self):
    """Gives you the bound command group this group is a descendant of.

    Returns:
      The BoundCommandGroup above this one in the tree or None if we are the top
    """
    return self._parent_group

  def __call__(self, **kwargs):
    return self._Execute(cli_mode=False, pre_run_hooks=None,
                         post_run_hooks=None, kwargs=kwargs)

  def GetShortHelp(self):
    return self._command.GetShortHelp()

  def EntryPoint(self):
    """Get the entry point that owns this command."""

    cur = self
    while cur.ParentGroup():
      cur = cur.ParentGroup()
    if type(cur) is BoundCommandGroup:
      cur = cur.UnboundGroup()
    return cur

  def _Execute(self, cli_mode, pre_run_hooks, post_run_hooks, kwargs):
    """Invoke the underlying command with the given arguments.

    Args:
      cli_mode: If true, run in CLI mode without checking kwargs for validity.
      pre_run_hooks: [RunHook], Things to run before the command.
      post_run_hooks: [RunHook], Things to run after the command.
      kwargs: The arguments with which to invoke the command.

    Returns:
      The result of executing the command determined by the command
      implementation.
    """
    # pylint: disable=protected-access, We don't want to expose the member or an
    # accessor since this is a user facing class.  These three classes all work
    # as a single unit.
    parent_args = self._parent_group._args if self._parent_group else {}
    new_args = self._command.CreateNewArgs(
        kwargs=kwargs,
        current_args=parent_args,
        cli_mode=cli_mode)  # we ignore unknown when in cli mode
    arg_namespace = _Args(new_args)
    return self._command.Run(
        args=arg_namespace, command=self, cli_mode=cli_mode,
        pre_run_hooks=pre_run_hooks, post_run_hooks=post_run_hooks)

  def __repr__(self):
    s = ''
    if self._parent_group:
      s += '%s.' % repr(self._parent_group)
    s += self._command.name
    return s


class RunHook(object):
  """Encapsulates a function to be run before or after command execution."""

  def __init__(self, func, include_commands=None, exclude_commands=None):
    """Constructs the hook.

    Args:
      func: function, The no args function to run.
      include_commands: str, A regex for the command paths to run.  If not
        provided, the hook will be run for all commands.
      exclude_commands: str, A regex for the command paths to exclude.  If not
        provided, nothing will be excluded.
    """
    self.__func = func
    self.__include_commands = include_commands if include_commands else '.*'
    self.__exclude_commands = exclude_commands

  def Run(self, command_path):
    """Runs this hook if the filters match the given command.

    Args:
      command_path: str, The calliope command path for the command that was run.

    Returns:
      bool, True if the hook was run, False if it did not match.
    """
    if not re.match(self.__include_commands, command_path):
      return False
    if self.__exclude_commands and re.match(self.__exclude_commands,
                                            command_path):
      return False
    self.__func()
    return True

