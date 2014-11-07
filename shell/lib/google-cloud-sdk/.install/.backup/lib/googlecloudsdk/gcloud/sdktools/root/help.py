# Copyright 2013 Google Inc. All Rights Reserved.

"""A calliope command that calls a help function."""

from googlecloudsdk import calliope
from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions as c_exc
from googlecloudsdk.core import cli


class Help(base.Command):
  """Prints detailed help messages for the specified commands.

  This command prints a detailed help message for the commands specified
  after the ``help`' operand.
  """

  @staticmethod
  def Args(parser):
    command_arg = parser.add_argument(
        'command',
        nargs='*',
        help='The commands to get help for.')
    command_arg.detailed_help = """\
        A sequence of group and command names with no flags.
        """

  @c_exc.RaiseToolExceptionInsteadOf(cli.NoHelpFoundError)
  def Run(self, args):
    help_func = self.entry_point.HelpFunc()

    def ShowShortHelp():
      """Print short help text."""
      try:
        cur = self.entry_point
        for command_segment in args.command:
          cur = getattr(cur, command_segment)
          if type(cur) not in [calliope.cli.frontend.UnboundCommandGroup,
                               calliope.cli.frontend.Command]:
            # They indexed into something weird, abort.
            raise AttributeError()
      except AttributeError:
        raise c_exc.ToolException(
            'Unknown command: {command}'.format(command='.'.join(args.command)))
      print cur.GetShortHelp()

    if not help_func:
      ShowShortHelp()
    else:
      try:
        help_func([self.entry_point.Name()] + (args.command or []))
      except cli.NoHelpFoundError:
        ShowShortHelp()
