# Copyright 2014 Google Inc. All Rights Reserved.

"""templates create command."""

from apiclient import errors

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib import template_util
from googlecloudsdk.preview.lib import util


class Create(base.Command):
  """Insert (Create) a template resource."""

  @staticmethod
  def Args(parser):
    """Args is called by calliope to gather arguments for this command.

    Args:
      parser: An argument parser that you can use to add arguments to the
          command line. Positional arguments are allowed.
    """
    parser.add_argument('template', help='Template name.')
    parser.add_argument(
        '--template-file',
        required=True,
        help='Path to YAML or JSON file containing the template.')

  def Run(self, args):
    """Run 'templates create'.

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

    new_template = template_util.ParseTemplate(
        args.template_file, args.template)

    request = client.templates().insert(
        projectId=project, body=new_template)

    try:
      request.execute()
      log.Print('Template {0} inserted.'.format(args.template))
    except errors.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))
    except errors.Error as error:
      # pylint:disable=nonstandard-exception, ToolException is an Exception.
      raise exceptions.ToolException.FromCurrent()

Create.detailed_help = {
    'brief': 'Insert (Create) a template resource.',
    'DESCRIPTION': """\
        This command inserts (creates) a template resource.

        You must provide a relative path to a local template file, either in
        YAML or JSON format, to create a template resource.

        If you want to use actions in your template, you can use a combination
        of script files and commands to specify your actions. In some cases,
        you might want to group a number of related commands into a script file
        for organization and readability reasons.

        To include a local script file, use the %file prefix in front of your
        script file's path. For example, if you specify the following in your
        template:

          actions:
            install-apache:
              commands: [
                "apt-get update",
                "apt-get -y install apache2",
                "%file:/path/to/my/website-setup-script.sh"
              ]

        the third command would copy the contents of the local file
        '/path/to/my/website-setup-script.sh' to a file on your Compute Engine
        virtual machines, and execute that file. For more information, please
        see https://developers.google.com/deployment-manager/yaml#includes

        Please note that each command or %file entry in your template must be
        less than 32KB in size, and the aggregate size of all of your commands
        must be less than 512KB.
        """,
}
