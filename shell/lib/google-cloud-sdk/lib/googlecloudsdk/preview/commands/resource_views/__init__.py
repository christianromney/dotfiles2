# Copyright 2014 Google Inc. All Rights Reserved.

"""The super-group for the Resource Views CLI."""

import apiclient.discovery as discovery

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import cli
from googlecloudsdk.core import properties
from googlecloudsdk.core.credentials import store


class ResourceViews(base.Group):
  """Manage Cloud Resource Views."""

  @staticmethod
  def Args(parser):
    """Args is called by calliope to gather arguments for this command.

    Args:
      parser: An argparse parser that you can use to add arguments that go
          on the command line after this command. Positional arguments are
          allowed.
    """
    parser.add_argument(
        '--api-version', default='v1beta1', help='Optional API version.')
    parser.add_argument(
        '--zone',
        required=False,
        help='Resource view zone name.')
    parser.add_argument(
        '--region',
        required=False,
        help='Resource view region name.')
    # TODO(user): Debug and add a mutually exclusive argument group so that
    # zone and region are not set together, but exactly one of them is set.

  @exceptions.RaiseToolExceptionInsteadOf(store.Error)
  def Filter(self, context, args):
    """Context() is a filter function that can update the context.

    Args:
      context: The current context.
      args: The argparse namespace that was specified on the CLI or API.
    Raises:
      ToolException: if the zone or region flags are provided together or
        are not provided at all.
    Returns:
      The updated context.
    """

    api_server = properties.VALUES.core.api_host.Get()
    api_version = args.api_version

    discovery_url = ('{server}/discovery/v1/apis/resourceviews/{version}/rest'
                     .format(server=api_server.rstrip('/'),
                             version=api_version))
    http = cli.Http()
    client = discovery.build(
        'resourceviews', api_version, http=http,
        discoveryServiceUrl=discovery_url)

    zone_views_client = client.zoneViews()

    # TODO(user): Remove when v1beta1 is deprecated.
    # Alias the API names so that we can continue to support v1beta1
    context['regionViewsClient'] = None
    if 'v1beta1' in api_version:
      region_views_client = client.regionViews()
      context['regionViewsClient'] = region_views_client
      if args.region and args.zone:
        raise exceptions.ToolException(
            '--zone and --region flags must not be set together!')
      if not (args.region or args.zone):
        raise exceptions.ToolException(
            'either --zone or --region must be set!')
    else:
      if not args.zone:
        raise exceptions.ToolException(
            '--zone is required and must be provided for all v1beta2 commands')

    context['zoneViewsClient'] = zone_views_client

    return context
