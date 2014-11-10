# Copyright 2014 Google Inc. All Rights Reserved.

"""managed-instance-groups updates rollback command."""

from googlecloudapis.apitools.base import py as apitools_base
from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import log
from googlecloudsdk.preview.lib import util
from googlecloudsdk.preview.lib.updater import util as updater_util


class Rollback(base.Command):
  """Rolls back an existing update."""

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
    # TODO(user): Support --async which does not wait for state transition.

  def Run(self, args):
    """Run 'managed-instance-groups updates rollback'.

    Args:
      args: argparse.Namespace, The arguments that this command was invoked
          with.

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
    request = messages.ReplicapoolupdaterUpdatesRollbackRequest(
        project=ref.project,
        zone=ref.zone,
        instanceGroupManager=ref.instanceGroupManager,
        updateHandle=ref.updateHandle)

    try:
      client.Rollback(request)
      update = updater_util.WaitForUpdateState(
          client, ref, ['ROLLING_BACK', 'ROLLED_BACK'], 'ROLLED_BACK',
          'Rolling back the update')
      if update.state in ['ROLLING_BACK', 'ROLLED_BACK']:
        log.status.write('Initiated rollback of [{0}].\n'.format(ref))
      else:
        raise exceptions.ToolException(
            'could not initiate rollback of [{0}]'.format(ref))

    except apitools_base.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))

Rollback.detailed_help = {
    'brief': 'Rolls back an existing update.',
    'DESCRIPTION': """\
        Rolls back the update in state ROLLING_FORWARD, ROLLING_BACK or PAUSED \
        (fails if the update is in any other state).
        No-op if invoked in state ROLLED_BACK.

        Rollback sets the template that the Managed Instance Group \
        used before the update was started and gradually restarts back to the \
        old template all instances that are running in the new template.
        """,
}
