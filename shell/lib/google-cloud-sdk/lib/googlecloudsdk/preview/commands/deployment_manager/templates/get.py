# Copyright 2014 Google Inc. All Rights Reserved.

"""templates get command."""

from apiclient import errors

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib import util


class Get(base.Command):
  """Gets a specific template and displays it in yaml format."""

  @staticmethod
  def Args(parser):
    """Args is called by calliope to gather arguments for this command.

    Args:
      parser: An argparse parser that you can use to add arguments that go
          on the command line after this command. Positional arguments are
          allowed.
    """
    parser.add_argument('template', help='Template name.')

  def Run(self, args):
    """Run 'templates get'.

    Args:
      args: argparse.Namespace, The arguments that this command was invoked
          with.

    Raises:
      HttpException: A http error response was received while executing api
          request.
      ToolException: An error other than http error occured while executing
          the command.

    Returns:
      The get API's response
    """
    client = self.context['manager']
    project = properties.VALUES.core.project.Get(required=True)

    request = client.templates().get(
        projectId=project, templateName=args.template)

    try:
      response = request.execute()
      util.PrettyPrint(response, args.format or 'yaml')
      return response
    except errors.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))
    except errors.Error as error:
      # pylint:disable=nonstandard-exception, ToolException is an Exception.
      raise exceptions.ToolException.FromCurrent()

Get.detailed_help = {
    'brief': 'Gets a specific template and displays it in yaml format.',
    'DESCRIPTION': """\
        This command gets a specific template and displays it in yaml format..

        You can also specify other formats such as json or text.
        """,
}
