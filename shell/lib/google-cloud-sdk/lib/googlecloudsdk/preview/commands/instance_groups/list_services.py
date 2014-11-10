# Copyright 2014 Google Inc. All Rights Reserved.

"""instance-groups add-service command."""

from apiclient import errors

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib import util


class ListServices(base.Command):
  """Lists the services for a resource or an instance group."""

  @staticmethod
  def Args(parser):
    """Args is called by calliope to gather arguments for this command.

    Args:
      parser: An argparse parser that you can use to add arguments that go
          on the command line after this command. Positional arguments are
          allowed.
    """
    parser.add_argument('name', help='Instance group name.')
    parser.add_argument(
        '--instance',
        help='The name of the instance resource to list the services for. If '
        'not specified, the services configured for the instance group will '
        'be returned.')

  def Run(self, args):
    """Run 'instance-groups list-services'.

    Args:
      args: argparse.Namespace, The arguments that this command was invoked
          with.

    Returns:
      the API response.

    Raises:
      HttpException: A http error response was received while executing api
          request.
      ToolException: An error other than http error occured while executing
          the command.
    """
    client = self.context['instanceGroupsClient']
    project = properties.VALUES.core.project.Get(required=True)
    resource_url = util.GenerateInstanceUrl(project, args.zone, args.instance)

    get_request = client.getService(
        project=project,
        zone=args.zone,
        resourceView=args.name,
        resourceName=resource_url)

    try:
      response = get_request.execute()
      util.PrettyPrint(response)
      return response
    except errors.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))
    except errors.Error as error:
      raise exceptions.ToolException(error)

ListServices.detailed_help = {
    'brief': 'Lists the services for an instance group an instance resource.',
    'DESCRIPTION': """\
        This command lists the services (name and port tuples) for one or all
        instances in an instance groupp.

        Please see the API documentation for more details.
        """,
}
