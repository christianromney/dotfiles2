# Copyright 2014 Google Inc. All Rights Reserved.

"""Instance groups delete command."""

from apiclient import errors

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib import util


class Delete(base.Command):
  """Delete an instance group."""

  @staticmethod
  def Args(parser):
    """Args is called by calliope to gather arguments for this command.

    Args:
      parser: An argparse parser that you can use to add arguments that go
          on the command line after this command. Positional arguments are
          allowed.
    """
    parser.add_argument(
        'name', nargs='+', help='One or more instance group names.')

  def Run(self, args):
    """Run 'instance-groups delete'.

    Args:
      args: argparse.Namespace, The arguments that this command was invoked
          with.

    Raises:
      HttpException: A http error response was received while executing api
          request.
      ToolException: An error other than http error occured while executing
          the command.
    """
    client = self.context['instanceGroupsClient']
    project = properties.VALUES.core.project.Get(required=True)

    for group_name in args.name:
      request = client.delete(
          project=project, zone=args.zone, resourceView=group_name)

      try:
        request.execute()
        log.Print('Instance group {0} deleted.'.format(group_name))
      except errors.HttpError as error:
        raise exceptions.HttpException(util.GetError(error))
      except errors.Error as error:
        raise exceptions.ToolException(error)
