# Copyright 2014 Google Inc. All Rights Reserved.

"""managed-instance-groups updates describe command."""

from googlecloudapis.apitools.base import py as apitools_base
from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.preview.lib import util


class Describe(base.Command):
  """Gets information about a single managed instance group update."""

  @staticmethod
  def Args(parser):
    """Args is called by calliope to gather arguments for this command.

    Args:
      parser: An argparse parser that you can use to add arguments that go
          on the command line after this command. Positional arguments are
          allowed.
    """
    parser.add_argument(
        'handle',
        help='Handle for a managed instance group update.')

  def Run(self, args):
    """Run 'managed-instance-groups updates describe'.

    Args:
      args: argparse.Namespace, The arguments that this command was invoked
          with.

    Returns:
      Update, representation of the update if the Get call was
      successful.

    Raises:
      HttpException: An http error response was received while executing api
          request.
      ToolException: An error other than http error occured while executing
          the command.
    """
    client = self.context['updater_api'].updates
    messages = self.context['updater_messages']
    resources = self.context['updater_resources']

    ref = resources.Parse(
        args.handle,
        collection='replicapoolupdater.updates')
    request = messages.ReplicapoolupdaterUpdatesGetRequest(
        project=ref.project,
        zone=ref.zone,
        instanceGroupManager=ref.instanceGroupManager,
        updateHandle=ref.updateHandle)

    try:
      return client.Get(request)
    except apitools_base.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))

  def Display(self, args, result):
    self.format(result)


Describe.detailed_help = {
    'brief': 'Gets information about a single managed instance group update.',
    'DESCRIPTION': """\
        This command gets information about a single managed instance group update.

        By default, this information is displayed in yaml format.
        You can also specify json or text formats.
        """,
}
