# Copyright 2013 Google Inc. All Rights Reserved.

"""The super-group for the sql CLI.

The fact that this is a directory with
an __init__.py in it makes it a command group. The methods written below will
all be called by calliope (though they are all optional).
"""
import argparse
import os
import re

import apiclient.discovery as discovery


from googlecloudapis.sqladmin import v1beta1 as sql_v1beta1
from googlecloudapis.sqladmin import v1beta3 as sql_v1beta3
from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import cli
from googlecloudsdk.core import config
from googlecloudsdk.core import properties
from googlecloudsdk.core import resolvers
from googlecloudsdk.core import resources as cloud_resources
from googlecloudsdk.core.credentials import store as c_store
from googlecloudsdk.sql import util as util


class SQL(base.Group):
  """Manage Cloud SQL databases."""

  @exceptions.RaiseToolExceptionInsteadOf(c_store.Error)
  def Filter(self, context, args):
    """Context() is a filter function that can update the context.

    Args:
      context: The current context.
      args: The argparse namespace that was specified on the CLI or API.

    Returns:
      The updated context.
    """

    cloud_resources.SetParamDefault(
        api='sql', collection=None, param='project',
        resolver=resolvers.FromProperty(properties.VALUES.core.project))
    credentials = cli.Credentials()

    context['sql_client-v1beta3'] = sql_v1beta3.SqladminV1beta3(
        credentials=credentials)
    context['sql_messages-v1beta3'] = sql_v1beta3
    context['registry-v1beta3'] = cloud_resources.REGISTRY

    context['sql_client-v1beta1'] = sql_v1beta1.SqladminV1beta1(
        credentials=credentials)
    context['sql_messages-v1beta1'] = sql_v1beta1
    context['registry-v1beta1'] = cloud_resources.REGISTRY.CloneAndSwitchAPIs(
        context['sql_client-v1beta1'])

    return context
