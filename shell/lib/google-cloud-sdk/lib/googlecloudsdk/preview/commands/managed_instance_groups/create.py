# Copyright 2014 Google Inc. All Rights Reserved.

"""managed-instance-groups create command."""

from apiclient import errors

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib import util


class Create(base.Command):
  """Insert (Create) a managed instance group."""

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
        '--template',
        required=True,
        help=('Name of the Compute Engine instance template resource to be '
              'used.'))
    parser.add_argument(
        '--base-instance-name',
        required=True,
        help='The base name to use for the Compute Engine instances that will '
        'be created in the managed instance group.')
    parser.add_argument(
        '--size',
        required=True,
        help='Initial number of instances in the managed instance group.')
    parser.add_argument(
        '--description',
        help='Managed instance group description.')
    parser.add_argument(
        '--target-pool',
        nargs='*',
        help='Compute Engine Target Pools to add the instances to. '
        'Target Pools must be specified by name, not by URL. Example: '
        '--target-pools "target-pool-1 target-pool-2"')

  def Run(self, args):
    """Run 'managed-instance-groups create'.

    Args:
      args: argparse.Namespace, The arguments that this command was invoked
          with.

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

    # TODO(user): Replace this with an actual call to the GCE Zones.Get() API
    # Truncate last two letters of zone to get region
    # e.g. us-central1 for us-central1-a
    region = args.zone[0:-2]
    target_pool_url = ('https://www.googleapis.com/compute/v1/'
                       'projects/{0}/regions/{1}/targetPools/{2}')

    # TODO(user): Replace this with an actual call to the GCE
    # InstanceTemplates.Get() API
    template_url = ('https://www.googleapis.com/compute/v1/'
                    'projects/{0}/global/instanceTemplates/{1}')

    new_group = {
        'name': args.group,
        'description': args.description,
        'instanceTemplate': template_url.format(project, args.template),
        'baseInstanceName': args.base_instance_name
    }

    if args.target_pool:
      target_pools = []
      for target_pool in args.target_pool:
        target_pools.append(
            target_pool_url.format(project, region, target_pool))
      new_group['targetPools'] = target_pools

    request = client.instanceGroupManagers().insert(
        project=project,
        zone=args.zone,
        size=args.size,
        body=new_group)

    try:
      response = request.execute()
      log.Print(('Managed instance group {0} is being created. '
                 'Operation: {1}').format(args.group, response['name']))
      return response
    except errors.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))
    except errors.Error as error:
      raise exceptions.ToolException(error)
