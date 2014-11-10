# Copyright 2014 Google Inc. All Rights Reserved.

"""managed-instance-groups recreate-instances command."""

from apiclient import errors

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib import util


class RecreateInstances(base.Command):
  """Recreates one or more instances in a managed instance group."""

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
        '--instance',
        nargs='+',
        required=True,
        help='Names of the instances to recreate.')

  def Run(self, args):
    """Run 'managed-instance-groups recreate-instances'.

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

    request_body = {}
    instances = []
    for instance in args.instance:
      instances.append(instance)
    request_body['instances'] = instances

    request = client.instanceGroupManagers().recreateInstances(
        project=project,
        zone=args.zone,
        instanceGroupManager=args.group,
        body=request_body)

    try:
      response = request.execute()
      log.Print(('Instances {0} are being recreated in group {1}. '
                 'Operation: {2}').format(args.instance,
                                          args.group,
                                          response['name']))
      return response
    except errors.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))
    except errors.Error as error:
      raise exceptions.ToolException(error)

RecreateInstances.detailed_help = {
    'brief': 'Recreates one or more instances in a managed instance group.',
    'DESCRIPTION': """\
        This command recreates one or more instances in a managed instance
        group. The underlying virtual machine instances are deleted, and
        recreated based on the latest instance template configured for the
        managed instance group.
        """,
}
