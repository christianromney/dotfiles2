# Copyright 2014 Google Inc. All Rights Reserved.

"""The command-group for the Managed Instance Group service CLI."""

import apiclient.discovery as discovery

from googlecloudapis.replicapoolupdater import v1beta1 as updater_v1beta1
from googlecloudapis.replicapoolupdater.v1beta1 import replicapoolupdater_v1beta1_messages
from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import cli
from googlecloudsdk.core import properties
from googlecloudsdk.core import resolvers
from googlecloudsdk.core import resources
from googlecloudsdk.core.credentials import store


class InstanceGroupManagers(base.Group):
  """Managed Instance Groups client."""

  @staticmethod
  def Args(parser):
    """Args is called by calliope to gather arguments for this command.

    Args:
      parser: An argparse parser that you can use to add arguments that go
          on the command line after this command. Positional arguments are
          allowed.
    """
    parser.add_argument(
        '--api-version', default='v1beta2', help='Optional API version.')
    parser.add_argument(
        '--zone', required=True, help='Managed instance group\'s zone name.')

  @exceptions.RaiseToolExceptionInsteadOf(store.Error)
  def Filter(self, context, args):
    """Context() is a filter function that can update the context.

    Args:
      context: The current context.
      args: The argparse namespace that was specified on the CLI or API.

    Returns:
      The updated context.
    """

    # Create the Managed Instance Group service client
    api_server = properties.VALUES.core.api_host.Get()
    api_version = args.api_version

    discovery_url = ('{server}/discovery/v1/apis/replicapool/{version}/rest'
                     .format(server=api_server.rstrip('/'),
                             version=api_version))
    http = cli.Http()
    client = discovery.build(
        'replicapool', api_version, http=http,
        discoveryServiceUrl=discovery_url)
    context['managedInstanceGroupsClient'] = client

    # Create the Managed Instance Group Updater service client
    context['updater_api'] = updater_v1beta1.ReplicapoolupdaterV1beta1(
        credentials=cli.Credentials(),
        http=cli.Http())
    context['updater_messages'] = replicapoolupdater_v1beta1_messages
    resources.SetParamDefault(
        api='replicapool', collection=None, param='project',
        resolver=resolvers.FromProperty(properties.VALUES.core.project))
    resources.SetParamDefault(
        api='replicapool', collection=None, param='zone',
        resolver=resolvers.FromProperty(properties.VALUES.compute.zone))
    resources.SetParamDefault(
        api='replicapoolupdater', collection=None, param='project',
        resolver=resolvers.FromProperty(properties.VALUES.core.project))
    resources.SetParamDefault(
        api='replicapoolupdater', collection=None, param='zone',
        resolver=resolvers.FromProperty(properties.VALUES.compute.zone))
    context['updater_resources'] = resources

    return context
