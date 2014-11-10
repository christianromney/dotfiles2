# Copyright 2014 Google Inc. All Rights Reserved.

"""Deployment Manager deployments sub-group."""

from googlecloudsdk.calliope import base


class Deployments(base.Group):
  """Commands for Deployment Manager deployments."""

  @staticmethod
  def Args(parser):
    """Args is called by calliope to gather arguments for this command.

    Args:
      parser: An argparse parser that you can use to add arguments that go
          on the command line after this command. Positional arguments are
          allowed.
    """
    parser.add_argument(
        '--region', required=True, help='Name of the region')
