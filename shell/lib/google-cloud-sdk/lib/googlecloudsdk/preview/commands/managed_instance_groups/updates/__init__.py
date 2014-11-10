# Copyright 2014 Google Inc. All Rights Reserved.

"""The command-group for the Managed Instance Group Updater service CLI."""

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import cli
from googlecloudsdk.core import properties
from googlecloudsdk.core import resolvers
from googlecloudsdk.core.credentials import store


class ManagedInstanceGroupsUpdater(base.Group):
  """Managed Instance Group Updater client."""

  @staticmethod
  def Args(parser):
    """Args is called by calliope to gather arguments for this command.

    Args:
      parser: An argparse parser that you can use to add arguments that go
          on the command line after this command. Positional arguments are
          allowed.
    """
    parser.add_argument('--group', help='Managed instance group name.')

  @exceptions.RaiseToolExceptionInsteadOf(store.Error)
  def Filter(self, context, args):
    """Context() is a filter function that can update the context.

    Args:
      context: The current context.
      args: The argparse namespace that was specified on the CLI or API.

    Returns:
      The updated context.
    """
    if args.group is None:
      raise exceptions.ToolException('argument --group is required')

    resources = context['updater_resources']
    resources.SetParamDefault(
        api='replicapoolupdater', collection=None, param='instanceGroupManager',
        resolver=resolvers.FromArgument('--group', args.group))
    return context
