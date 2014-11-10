# Copyright 2014 Google Inc. All Rights Reserved.
"""Command for updating autoscalers."""

from googlecloudapis.apitools.base.py import exceptions
from googlecloudsdk.calliope import exceptions as calliope_exceptions
from googlecloudsdk.compute.lib import base_classes
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib.autoscaler import util


class UpdateAutoscaler(base_classes.BaseCommand):  # BaseAsyncMutator
  """Update Autoscaler instances."""

  @staticmethod
  def Args(parser):
    util.AddAutoscalerArgs(parser)

  def Run(self, args):
    try:
      messages = self.context['autoscaler_messages_module']
      request = messages.AutoscalerAutoscalersUpdateRequest()
      request.project = properties.VALUES.core.project.Get(required=True)
      request.zone = args.zone
      request.autoscaler = args.name
      # TODO(user): Whenever interpreting args as Cloud resources, use the
      # resources module (self.context['autoscaler_resources'])
      request.autoscalerResource = util.PrepareAutoscaler(
          args, messages)
      result = self.context['autoscaler'].Update(request)
      # TODO(user): Handle more complex operations (handle errors each time
      # operation is pulled).
      if result.error:
        raise exceptions.Error(util.GetErrorMessage(result.error))
      log.status.write('Updated [{0}].\n'.format(args.name))

    except exceptions.HttpError as error:
      raise calliope_exceptions.HttpException(util.GetErrorMessage(error))
