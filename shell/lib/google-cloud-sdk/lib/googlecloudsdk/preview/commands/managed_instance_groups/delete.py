# Copyright 2014 Google Inc. All Rights Reserved.

"""managed-instance-groups delete command."""

from apiclient import errors

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib import util


class Delete(base.Command):
  """Deletes a managed instance group and its corresponding instances."""

  @staticmethod
  def Args(parser):
    """Args is called by calliope to gather arguments for this command.

    Args:
      parser: An argparse parser that you can use to add arguments that go
          on the command line after this command. Positional arguments are
          allowed.
    """
    parser.add_argument('group', help='Managed instance group name.')

  def Run(self, args):
    """Run 'managed-instance-groups delete'.

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

    request = client.instanceGroupManagers().delete(
        project=project,
        zone=args.zone,
        instanceGroupManager=args.group)

    try:
      response = request.execute()
      log.Print(('Managed instance group {0} is being deleted. '
                 'Operation: {1}').format(args.group, response['name']))
      return response
    except errors.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))
    except errors.Error as error:
      raise exceptions.ToolException(error)

Delete.detailed_help = {
    'brief': ('Deletes a managed instance group and all of the '
              'instances in the group.'),
    'DESCRIPTION': """\
        This command deletes a managed instance group and all of the instances
        in the group.

        The underlying virtual machine instances are also deleted.
        If you wish to keep one or more of the virtual machines, use the
        abandon-instances command first before deleting the managed instance
        group.
        """,
}
