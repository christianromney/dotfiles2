# Copyright 2014 Google Inc. All Rights Reserved.

"""'resourceviews resources remove' command."""

from apiclient import errors

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib import util


class Remove(base.Command):
  """Removes instancess from an instance group by name."""

  @staticmethod
  def Args(parser):
    """Args is called by calliope to gather arguments for this command.

    Args:
      parser: An argparse parser that you can use to add arguments that go
          on the command line after this command. Positional arguments are
          allowed.
    """
    parser.add_argument(
        'instances',
        nargs='+',
        help=('Instances to remove from the instance group, specified as '
              'names or URLs. If specified as URLs the --urls flag must be '
              'provided.'))
    parser.add_argument(
        '--urls',
        dest='urls',
        default=False,
        action='store_true',
        help=('Use this flag to specify that the provided instance '
              'identifiers are URLs, and not instance names.'))

  def Run(self, args):
    """Run 'instance-groups instances remove'.

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

    instance_urls = []
    if args.urls:
      instance_urls = args.instances
    else:
      for instance in args.instances:
        instance_urls.append(
            'https://www.googleapis.com/compute/v1/projects/' +
            project + '/zones/' + args.zone + '/instances/' +
            instance)

    request_body = {'resources': instance_urls}

    request = client.removeResources(
        project=project,
        zone=args.zone,
        resourceView=args.group,
        body=request_body)

    try:
      request.execute()
      log.Print('Instances removed from instance group {0}.'.format(
          args.group))
    except errors.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))
    except errors.Error as error:
      raise exceptions.ToolException(error)

Remove.detailed_help = {
    'brief': 'Removes resources from an instance group by instance name.',
    'DESCRIPTION': """\
        This command removes resource from an instance group by name.
        The instance name will be converted to fully-qualified URLs before
        it is removed.

        This does not delete the actual instance resource but removes it
        from the instance group.
        """,
}
