# Copyright 2014 Google Inc. All Rights Reserved.
"""Command for deleting autoscalers."""

from googlecloudapis.apitools.base.py import exceptions
from googlecloudsdk.calliope import exceptions as calliope_exceptions
from googlecloudsdk.compute.lib import base_classes
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib.autoscaler import util


class DeleteAutoscaler(base_classes.BaseCommand):  # BaseAsyncMutator
  """Delete Autoscaler instances."""

  @staticmethod
  def Args(parser):
    parser.add_argument(
        'names', help='Autoscalers names.', nargs='+')

  def Run(self, args):
    try:
      request = (self.context['autoscaler_messages_module'].
                 AutoscalerAutoscalersDeleteRequest())
      request.project = properties.VALUES.core.project.Get(required=True)
      # TODO(user): Whenever interpreting args as cloud resources, use the
      # resources module (self.context['autoscaler_resources']).
      # TODO(user): Handle more complex operations.
      request.zone = args.zone
      # TODO(user): Handle operations.
      for name in args.names:
        request.autoscaler = name
        self.context['autoscaler'].Delete(request)
        log.DeletedResource(name)

    except exceptions.HttpError as error:
      raise calliope_exceptions.HttpException(util.GetErrorMessage(error))
