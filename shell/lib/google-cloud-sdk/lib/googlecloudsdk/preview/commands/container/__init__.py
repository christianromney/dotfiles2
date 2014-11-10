# Copyright 2014 Google Inc. All Rights Reserved.

"""The main command group for cloud container."""

import argparse

from googlecloudapis.container import v1beta1 as container_v1beta1
from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import cli
from googlecloudsdk.core import properties
from googlecloudsdk.core import resolvers
from googlecloudsdk.core import resources as cloud_resources


class Container(base.Group):
  """Deploy and manage clusters of machines for running containers."""

  @staticmethod
  def Args(parser):
    """Add arguments to the parser.

    Args:
      parser: argparse.ArgumentParser, This is a standard argparser parser with
        which you can register arguments.  See the public argparse documentation
        for its capabilities.
    """
    parser.add_argument(
        '--api-version', default='v1beta1', help=argparse.SUPPRESS)

    parser.add_argument(
        '--zone', '-z',
        help='The zone (e.g. us-central1-a) for the cluster')

  def Filter(self, context, args):
    """Modify the context that will be given to this group's commands when run.

    Args:
      context: {str:object}, A set of key-value pairs that can be used for
          common initialization among commands.
      args: argparse.Namespace: The same namespace given to the corresponding
          .Run() invocation.

    Returns:
      The refined command context.
    """

    if not args.zone:
      raise exceptions.RequiredArgumentException(
          '--zone', 'The compute zone for the cluster')
    cloud_resources.SetParamDefault(
        api='compute', collection=None, param='project',
        resolver=resolvers.FromProperty(properties.VALUES.core.project))

    context['container-api-v1beta1'] = container_v1beta1.ContainerV1beta1(
        credentials=cli.Credentials(),
        url=properties.VALUES.core.api_host.Get() + '/container/v1beta1/')
    context['container_messages-v1beta1'] = container_v1beta1
    context['container_registry-v1beta1'] = cloud_resources.REGISTRY

    context['container-api'] = context['container-api-' + args.api_version]
    context['container_messages'] = context['container_messages-' +
                                            args.api_version]
    context['container_registry'] = context['container_registry-' +
                                            args.api_version]
    return context
