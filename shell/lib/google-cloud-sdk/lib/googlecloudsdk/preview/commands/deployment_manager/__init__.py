# Copyright 2014 Google Inc. All Rights Reserved.

"""The command group for the DeploymentManager CLI."""

import apiclient.discovery as discovery

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import cli
from googlecloudsdk.core import properties
from googlecloudsdk.core.credentials import store


class DeploymentManager(base.Group):
  """Manage Deployments of cloud resources."""

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

  @exceptions.RaiseToolExceptionInsteadOf(store.Error)
  def Filter(self, context, args):
    """Context() is a filter function that can update the context.

    Args:
      context: The current context.
      args: The argparse namespace that was specified on the CLI or API.

    Returns:
      The updated context.
    Raises:
      ToolException: When no project is specified.
    """

    # Create the DeploymentManager service client
    api_server = properties.VALUES.core.api_host.Get()
    api_version = args.api_version

    discovery_url = ('{server}/discovery/v1/apis/manager/{version}/rest'
                     .format(server=api_server.rstrip('/'),
                             version=api_version))
    http = cli.Http()
    client = discovery.build(
        'manager', api_version, http=http, discoveryServiceUrl=discovery_url)
    context['manager'] = client

    return context
