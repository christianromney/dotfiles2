# Copyright 2014 Google Inc. All Rights Reserved.

"""The command group for the Autoscaler CLI."""

import argparse
import apiclient.discovery as discovery

from googlecloudapis.autoscaler import v1beta2 as autoscaler_v1beta2
from googlecloudapis.autoscaler.v1beta2 import autoscaler_v1beta2_messages as messages_v2
from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import cli
from googlecloudsdk.core.credentials import store


class Autoscaler(base.Group):
  """Manage autoscalers of cloud resources."""

  @staticmethod
  def Args(parser):
    parser.add_argument('--zone', required=True, help='Autoscaler Zone name')

  @exceptions.RaiseToolExceptionInsteadOf(store.Error)
  def Filter(self, context, args):
    api = autoscaler_v1beta2.AutoscalerV1beta2(
        get_credentials=False, http=cli.Http())
    context['autoscaler_messages_module'] = messages_v2


    context['autoscaler'] = api.AutoscalersService(api)
    # TODO(user): Add 'autoscaler_resources', which will be an instance of
    # resources.Registry from the core.resources module (so it can be used to
    # construct resources in commands (and switched easily)).
    return context
