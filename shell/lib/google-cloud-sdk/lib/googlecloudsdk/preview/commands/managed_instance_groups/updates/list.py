# Copyright 2014 Google Inc. All Rights Reserved.

"""managed-instance-groups updates list command."""

from googlecloudapis.apitools.base import py as apitools_base
from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core.util import list_printer
from googlecloudsdk.preview.lib import util


class List(base.Command):
  """Lists all managed instance group updates in a given project."""

  @staticmethod
  def Args(parser):
    """Args is called by calliope to gather arguments for this command.

    Args:
      parser: An argparse parser that you can use to add arguments that go
          on the command line after this command. Positional arguments are
          allowed.
    """
    parser.add_argument('--limit', type=int,
                        help='The maximum number of results to list.')

  def Run(self, args):
    """Run 'managed-instance-groups updates list'.

    Args:
      args: argparse.Namespace, The arguments that this command was invoked
          with.

    Returns:
      List of all the updates.

    Raises:
      HttpException: An http error response was received while executing api
          request.
      ToolException: An error other than http error occured while executing
          the command.
    """
    client = self.context['updater_api'].updates
    messages = self.context['updater_messages']
    resources = self.context['updater_resources']

    group_ref = resources.Parse(
        args.group,
        collection='replicapool.instanceGroupManagers')
    request = messages.ReplicapoolupdaterUpdatesListRequest(
        project=group_ref.project,
        zone=group_ref.zone,
        instanceGroupManager=group_ref.instanceGroupManager)
    limit = util.SanitizeLimitFlag(args.limit)

    try:
      return apitools_base.YieldFromList(client, request, limit)
    except apitools_base.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))

  def Display(self, unused_args, result):
    list_printer.PrintResourceList(
        'replicapoolupdater.updates', result)
