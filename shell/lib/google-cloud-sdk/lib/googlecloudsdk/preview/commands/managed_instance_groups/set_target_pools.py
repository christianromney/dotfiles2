# Copyright 2014 Google Inc. All Rights Reserved.

"""managed-instance-groups set-target-pools command."""

from apiclient import errors

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib import util


class SetTargetPools(base.Command):
  """Sets the target-pools for an existing managed instance group."""

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
        '--target-pool',
        nargs='+',
        required=True,
        help='Names of the Compute Engine target pool resources to use.')

  def Run(self, args):
    """Run 'managed-instance-groups set-target-pools'.

    Args:
      args: argparse.Namespace, The arguments that this command was invoked
          with.

    Returns:
      An object representing the service response obtained by the Get
      API if the Get call was successful.

    Raises:
      HttpException: A http error response was received while executing api
          request.
      ToolException: An error other than http error occured while executing
          the command.

    Returns:
      response: the response returned by the service, expected to be a
          zonal operation resource
    """
    client = self.context['managedInstanceGroupsClient']
    project = properties.VALUES.core.project.Get(required=True)

    get_request = client.instanceGroupManagers().get(
        project=project,
        zone=args.zone,
        instanceGroupManager=args.group)

    fingerprint = None
    try:
      group = get_request.execute()
      fingerprint = group['fingerprint']
    except errors.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))
    except errors.Error as error:
      raise exceptions.ToolException(error)

    # TODO(user): Replace this with an actual call to the GCE Zones.Get() API
    # Truncate last two letters of zone to get region
    # e.g. us-central1 for us-central1-a
    region = args.zone[0:-2]

    target_pool_url = ('https://www.googleapis.com/compute/v1/'
                       'projects/{0}/regions/{1}/targetPools/{2}')

    target_pools = []
    for target_pool in args.target_pool:
      target_pools.append(target_pool_url.format(project, region, target_pool))

    request_body = {
        'fingerprint': fingerprint,
        'targetPools': target_pools
    }

    request = client.instanceGroupManagers().setTargetPools(
        project=project,
        zone=args.zone,
        instanceGroupManager=args.group,
        body=request_body)

    try:
      response = request.execute()
      log.Print('Target pools set for managed instance group {0}.'.format(
          args.group))
      return response
    except errors.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))
    except errors.Error as error:
      raise exceptions.ToolException(error)

SetTargetPools.detailed_help = {
    'brief': 'Sets the target pools for an existing managed instance group.',
    'DESCRIPTION': """\
        This command sets the target pools for an existing managed
        instance group.

        The new target pools won't apply to existing instances in the group
        unless they are recreated using the 'recreate-instances' command.
        But any new instances created in the managed instance group will be
        added to all of the provided target pools for load balancing purposes.
        """,
}
