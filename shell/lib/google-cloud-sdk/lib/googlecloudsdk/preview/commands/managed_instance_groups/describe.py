# Copyright 2014 Google Inc. All Rights Reserved.

"""managed-instance-groups describe command."""

from apiclient import errors

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib import util


class Describe(base.Command):
  """Gets information about a single managed instance group."""

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
    """Run 'managed-instance-groups describe'.

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

    request = client.instanceGroupManagers().get(
        project=project,
        zone=args.zone,
        instanceGroupManager=args.group)

    try:
      response = request.execute()
      util.PrettyPrint(response, args.format or 'yaml')
      return response
    except errors.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))
    except errors.Error as error:
      raise exceptions.ToolException(error)

Describe.detailed_help = {
    'brief': 'Gets information about a single managed instance group.',
    'DESCRIPTION': """\
        This command gets information about a single managed instance group.

        By default, this information is displayed in yaml format.
        You can also specify json or text formats.
        """,
}
