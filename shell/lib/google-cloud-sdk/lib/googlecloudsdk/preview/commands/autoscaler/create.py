# Copyright 2014 Google Inc. All Rights Reserved.
"""Command for creating autoscalers."""

from googlecloudapis.apitools.base.py import exceptions
from googlecloudsdk.calliope import exceptions as calliope_exceptions
from googlecloudsdk.compute.lib import base_classes
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib.autoscaler import util


class CreateAutoscaler(base_classes.BaseCommand):
  """Create Autoscaler instances."""

  @staticmethod
  def Args(parser):
    util.AddAutoscalerArgs(parser)

  def Run(self, args):

    try:
      messages = self.context['autoscaler_messages_module']
      request = messages.AutoscalerAutoscalersInsertRequest()
      request.project = properties.VALUES.core.project.Get(required=True)
      request.zone = args.zone
      request.autoscaler = util.PrepareAutoscaler(args, messages)
      operation = self.context['autoscaler'].Insert(request)

      # TODO(user): Handle more complex operations. See go/tkqnv an
      # example of how to nicely work with an operations API to have the command
      # block until it's done. In a nutshell, add an --async flag. When async is
      # not set, poll the operation a la https://go/ouout. If async is set,
      # return the operation like it is done here.
      if operation.error:
        raise exceptions.Error(operation.error)
    except exceptions.HttpError as error:
      raise calliope_exceptions.HttpException(util.GetErrorMessage(error))

  def Display(self, unused_args, result):
    # TODO(user): The output of create commands should be the same as the
    # list command, but for the one element. See go/dxtgn.
    print'Success.'
