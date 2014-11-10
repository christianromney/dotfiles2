# Copyright 2014 Google Inc. All Rights Reserved.

"""Instance groups create command."""

import argparse
from apiclient import errors

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib import util


class Create(base.Command):
  """Insert (create) an instance group."""

  @staticmethod
  def Args(parser):
    """Args is called by calliope to gather arguments for this command.

    Args:
      parser: An argparse parser that you can use to add arguments that go
          on the command line after this command. Positional arguments are
          allowed.
    """
    parser.add_argument('name', help='Instance group name.')
    parser.add_argument('--description', help='Description for the group.')
    parser.add_argument(
        '--network',
        help='Name of the Compute Engine network associated with the group.')

  def Run(self, args):
    """Run 'instance-groups create'.

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

    new_instance_group = {
        'name': args.name,
        'description': args.description,
    }
    compute_api_version = 'v1'

    if args.network:
      network_url = ('https://www.googleapis.com/compute/{0}/'
                     'projects/{1}/global/networks/{2}')
      new_instance_group['network'] = network_url.format(
          compute_api_version, project, args.network)

    request = client.insert(
        project=project, zone=args.zone, body=new_instance_group)

    try:
      request.execute()
      log.Print('Instance group {0} created.'.format(args.name))
    except errors.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))
    except errors.Error as error:
      raise exceptions.ToolException(error)
