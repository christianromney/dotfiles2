# Copyright 2014 Google Inc. All Rights Reserved.

"""templates delete command."""

from apiclient import errors

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib import util


class Delete(base.Command):
  """Delete a template."""

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
    """Run 'templates delete'.

    Args:
      args: argparse.Namespace, The arguments that this command was invoked
          with.

    Raises:
      HttpException: A http error response was received while executing api
          request.
      ToolException: An error other than http error occured while executing
          the command.
    """
    client = self.context['manager']
    project = properties.VALUES.core.project.Get(required=True)

    request = client.templates().delete(
        projectId=project, templateName=args.template)

    try:
      request.execute()
      log.Print('Template {0} deleted.'.format(args.template))
    except errors.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))
    except errors.Error as error:
      # pylint:disable=nonstandard-exception, ToolException is an Exception.
      raise exceptions.ToolException.FromCurrent()

Delete.detailed_help = {
    'brief': 'Delete a template resource.',
    'DESCRIPTION': """\
        This command deletes a template resource.

        Deleting a template does not delete any resources in deployments created
        based on that template.
        """,
}
