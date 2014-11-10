# Copyright 2014 Google Inc. All Rights Reserved.

"""managed-instance-groups start-rolling-update command."""

from googlecloudapis.apitools.base import py as apitools_base
from googlecloudsdk.calliope import arg_parsers
from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import log
from googlecloudsdk.preview.lib import util
from googlecloudsdk.preview.lib.updater import util as updater_util


class StartRollingUpdate(base.Command):
  """Starts a new rolling update."""

  @staticmethod
  def Args(parser):
    """Args is called by calliope to gather arguments for this command.

    Args:
      parser: An argparse parser that you can use to add arguments that go
          on the command line after this command. Positional arguments are
          allowed.
    """
    parser.add_argument('group', help='Managed instance group name.')
    parser.add_argument(
        '--template', required=True,
        help='Name of the Compute Engine instance template resource.')
    parser.add_argument(
        '--num-canary-instances', type=int,
        help='Number of instances updated as a part of canary phase.')
    parser.add_argument(
        '--max-num-concurrent-instances', type=int,
        help='Maximum number of instances that can be updated simultaneously.')
    sleep_after_instance_restart = parser.add_argument(
        '--sleep-after-instance-restart', type=arg_parsers.Duration(),
        help='Time to sleep after instance is restarted.')
    sleep_after_instance_restart.detailed_help = """\
        Time period after the instance has been restarted but before marking
        the update of this instance as done. Valid units for this flag are ``s''
        for seconds, ``m'' for minutes, ``h'' for hours and ``d'' for days. If
        no unit is specified, seconds is assumed.
        """
    # TODO(user): Support --async which does not wait for state transition.

  def Run(self, args):
    """Run 'managed-instance-groups start-rolling-update'.

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

    group_ref = resources.Parse(
        args.group,
        collection='replicapool.instanceGroupManagers')
    request = messages.ReplicapoolupdaterUpdatesInsertRequest(
        project=group_ref.project,
        zone=group_ref.zone,
        instanceGroupManager=group_ref.instanceGroupManager,
        update=self._PrepareUpdate(group_ref, args))

    try:
      response = client.Insert(request)
      ref = resources.Create(
          collection='replicapoolupdater.updates',
          instanceGroupManager=args.group,
          updateHandle=response.updateHandle)
      update = updater_util.WaitForUpdateState(
          client, ref, ['ROLLING_FORWARD', 'ROLLED_OUT'], 'ROLLED_OUT',
          'Starting a new update')
      if update.state in ['ROLLING_FORWARD', 'ROLLED_OUT']:
        log.status.write('Started [{0}].\n'.format(ref))
      else:
        raise exceptions.ToolException('could not start [{0}]'.format(ref))

    except apitools_base.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))

  def _PrepareUpdate(self, group_ref, args):
    """Creates an update object based on user-provided flags.

    Args:
      group_ref: Reference to the InstanceGroupManager resource.
      args: argparse.Namespace, The arguments that this command was invoked
          with.

    Returns:
      Update, an update object prepared to be used by Insert method.
    """
    messages = self.context['updater_messages']
    resources = self.context['updater_resources']

    template_ref = resources.Create(
        collection='compute.instanceTemplates',
        project=group_ref.project, instanceTemplate=args.template)

    policy = messages.UpdatePolicy()
    if args.num_canary_instances:
      policy.canary = messages.UpdatePolicyCanary(
          numInstances=args.num_canary_instances)
    if args.max_num_concurrent_instances:
      policy.maxNumConcurrentInstances = args.max_num_concurrent_instances
    if args.sleep_after_instance_restart:
      policy.sleepAfterInstanceRestartSec = args.sleep_after_instance_restart

    return messages.Update(
        instanceTemplate=template_ref.SelfLink(),
        policy=policy)


StartRollingUpdate.detailed_help = {
    'brief': 'Starts a new rolling update.',
    'DESCRIPTION': """\
        A rolling update causes the service to gradually recreate your \
        existing instances with the new template. You can increase the number \
        of instances updated simultaneously with the \
        --max-num-concurrent-instances flag.

        If you are not sure you want to apply an update to all existing \
        instances, you can use the --num-canary-instances flag and the update \
        will automatically be paused after updating the number of instances \
        specified. Afterwards, you can decide whether to cancel or continue \
        the update.

        In case you notice your managed instance group misbehaving due to the \
        new template, you can roll back the update. This will stop the update \
        from being applied to more instances, and instances already created \
        with the new template will be recreated with the last template applied \
        before the current update.
        """,
}
