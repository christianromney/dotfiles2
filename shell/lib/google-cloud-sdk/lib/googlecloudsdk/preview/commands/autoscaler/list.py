# Copyright 2014 Google Inc. All Rights Reserved.
"""Command for listing autoscalers."""

from googlecloudapis.apitools.base import py as apitools_base
from googlecloudapis.apitools.base.py import exceptions
from googlecloudsdk.calliope import exceptions as calliope_exceptions
from googlecloudsdk.compute.lib import base_classes
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib.autoscaler import util


class ListAutoscalers(base_classes.BaseCommand):
  """List Autoscaler instances."""

  # TODO(user): List must comply wih go/yuyix
  def Run(self, args):
    try:
      request = (self.context['autoscaler_messages_module'].
                 AutoscalerAutoscalersListRequest())
      request.project = properties.VALUES.core.project.Get(required=True)
      # TODO(user): Whenever interpreting args as cloud resources, use the
      # resources module (self.context['autoscaler_resources']). Note that zone
      # is a Cloud resource.
      request.zone = args.zone
      autoscalers = apitools_base.YieldFromList(
          self.context['autoscaler'], request)
      result = [autoscaler.name for autoscaler in autoscalers]
      return result

    except exceptions.HttpError as error:
      raise calliope_exceptions.HttpException(util.GetErrorMessage(error))

  # TODO(user): Use apitools_base.YieldFromList: go/xsxsl, example:
  # go/mztly.
  # TODO(user): Use .cloud.core.util.list_printer to .Display().
  def Display(self, unused_args, result):
    if result:
      for autoscaler in result:
        print autoscaler
    else:
      print 'No autoscalers'
