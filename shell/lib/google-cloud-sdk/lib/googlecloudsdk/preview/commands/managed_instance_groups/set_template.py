# Copyright 2014 Google Inc. All Rights Reserved.

"""managed-instance-groups set-template command."""

from apiclient import errors

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib import util


class SetTemplate(base.Command):
  """Sets the template for an existing managed instance group."""

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
        '--template', required=True,
        help='Name of the Compute Engine instance template resource.')

  def Run(self, args):
    """Run 'managed-instance-groups set-template'.

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

    # TODO(user): Use resources.Create(...).
    template_url = ('https://www.googleapis.com/compute/v1/'
                    'projects/{0}/global/instanceTemplates/{1}')

    request_body = {
        'instanceTemplate': template_url.format(project, args.template)
    }
    request = client.instanceGroupManagers().setInstanceTemplate(
        project=project,
        zone=args.zone,
        instanceGroupManager=args.group,
        body=request_body)

    try:
      response = request.execute()
      log.Print(
          'Instance template {0} set for managed instance group {1}.'.format(
              args.template, args.group))
      return response
    except errors.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))
    except errors.Error as error:
      raise exceptions.ToolException(error)

SetTemplate.detailed_help = {
    'brief': 'Sets the template for an existing managed instance group.',
    'DESCRIPTION': """\
        This command updates the instance template for an existing managed
        instance group.

        The new template won't apply to existing instances in the group
        unless they are recreated using the 'recreate-instances' command.
        But the new template does apply to all new instances added to the
        managed instance group.
        """,
}
