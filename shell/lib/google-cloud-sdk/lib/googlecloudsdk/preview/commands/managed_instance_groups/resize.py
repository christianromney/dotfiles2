# Copyright 2014 Google Inc. All Rights Reserved.

"""managed-instance-groups resize command."""

from apiclient import errors

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib import util


class Resize(base.Command):
  """Resizes a managed-instance-group to a provided size."""

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
        '--new-size',
        required=True,
        help='New size for the managed instance group, must be >= 0.')

  def Run(self, args):
    """Run 'managed-instance-group resize'.

    Args:
      args: argparse.Namespace, The arguments that this command was invoked
          with.

    Returns:
      An object representing the service response obtained by the Resize
      API if the Resize call was successful.

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

    request = client.instanceGroupManagers().resize(
        project=project,
        zone=args.zone,
        instanceGroupManager=args.group,
        size=args.new_size)

    try:
      response = request.execute()
      log.Print(('Managed instance group {0} is being resized. '
                 'Operation: {1}').format(args.group, response['name']))
      return response
    except errors.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))
    except errors.Error as error:
      raise exceptions.ToolException(error)

Resize.detailed_help = {
    'brief': 'Resizes a managed instance group to a provided size.',
    'DESCRIPTION': """\
        This command resizes a managed instance group to a provided size.

        If you resize down, the Instance Group Manager service deletes
        instances from the group until the group reaches the desired size.
        To understand what order the instances will be deleted in, please
        see the API documentation.

        If you resize up, the service adds instances to the group using the
        most current instance template until the group reaches the desired
        size.
        """,
}
