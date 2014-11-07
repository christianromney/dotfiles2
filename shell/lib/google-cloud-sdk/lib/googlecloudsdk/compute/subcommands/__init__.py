# Copyright 2014 Google Inc. All Rights Reserved.
"""The super-group for the compute CLI."""
import argparse
import urlparse

from googlecloudapis.compute.v1 import compute_v1_client
from googlecloudsdk.calliope import base
from googlecloudsdk.compute.lib import constants
from googlecloudsdk.core import cli
from googlecloudsdk.core import exceptions
from googlecloudsdk.core import properties
from googlecloudsdk.core import resolvers
from googlecloudsdk.core import resources


class Compute(base.Group):
  """Read and manipulate Google Compute Engine resources."""


  def Filter(self, context, args):
    http = cli.Http()
    core_values = properties.VALUES.core
    compute_values = properties.VALUES.compute
    context['http'] = http
    context['project'] = core_values.project.Get(required=True)

    for api, param, prop in (
        ('compute', 'project', core_values.project),
        ('resourceviews', 'projectName', core_values.project),
        ('compute', 'zone', compute_values.zone),
        ('resourceviews', 'zone', compute_values.zone),
        ('compute', 'region', compute_values.region),
        ('resourceviews', 'region', compute_values.region)):
      resources.SetParamDefault(
          api=api,
          collection=None,
          param=param,
          resolver=resolvers.FromProperty(prop))

    api_host = properties.VALUES.core.api_host.Get()
    v1_version_component = 'compute/v1/'

    compute_url = urlparse.urljoin(api_host, v1_version_component)
    context['batch-url'] = urlparse.urljoin(api_host, 'batch')

    v1_compute = compute_v1_client.ComputeV1(
        url=compute_url,
        get_credentials=False,
        http=http)
    context['compute'] = v1_compute
    context['resources'] = resources.REGISTRY.CloneAndSwitchAPIs(v1_compute)



Compute.detailed_help = {
    'brief': 'Read and manipulate Google Compute Engine resources',
}
