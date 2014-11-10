# Copyright 2014 Google Inc. All Rights Reserved.

"""instance-groups instances subcommands."""

from googlecloudsdk.calliope import base


class Changes(base.Group):
  """Manage instances in an instance-group."""

  @staticmethod
  def Args(parser):
    """Args is called by calliope to gather arguments for this command.

    Args:
      parser: An argparse parser that you can use to add arguments that go
          on the command line after this command. Positional arguments are
          allowed.
    """
    parser.add_argument(
        '--group', required=True, help='Instance group name.')
