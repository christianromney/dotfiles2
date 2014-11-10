# Copyright 2014 Google Inc. All Rights Reserved.
"""Command for getting autoscalers."""

# TODO(user): Rename get command to describe to be consistent with compute.

from googlecloudapis.apitools.base.py import exceptions
from googlecloudsdk.calliope import exceptions as calliope_exceptions
from googlecloudsdk.compute.lib import base_classes
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib.autoscaler import util


def UtilizationTargetTypeForItem(item):
  if hasattr(item, 'utilizationTargetType') and item.utilizationTargetType:
    return item.utilizationTargetType
  return ''


class GetAutoscaler(base_classes.BaseCommand):
  """Get Autoscaler instances."""

  @staticmethod
  def Args(parser):
    parser.add_argument('name', help='Autoscaler name.')

  def Run(self, args):
    request = (self.context['autoscaler_messages_module'].
               AutoscalerAutoscalersGetRequest())
    request.project = properties.VALUES.core.project.Get(required=True)
    request.zone = args.zone
    request.autoscaler = args.name
    # TODO(user): Simply return autoscaler. In .Display pass Autoscaler
    # directly into the formatter.
    try:
      autoscaler = self.context['autoscaler'].Get(request)

      as_dict = {
          'description': autoscaler.description,
          'name': autoscaler.name,
          'selfLink': autoscaler.selfLink,
          'target': autoscaler.target
          }
      if autoscaler.autoscalingPolicy:
        as_dict['autoscalingPolicy'] = {
            'coolDownPeriodSec':
                autoscaler.autoscalingPolicy.coolDownPeriodSec,
            'maxNumReplicas': autoscaler.autoscalingPolicy.maxNumReplicas,
            'minNumReplicas': autoscaler.autoscalingPolicy.minNumReplicas
            }
        if autoscaler.autoscalingPolicy.cpuUtilization:
          as_dict['autoscalingPolicy']['cpuUtilization'] = {
              'utilizationTarget':
              autoscaler.autoscalingPolicy.cpuUtilization.utilizationTarget
          }
        # autoscalingPolicy has cpuUtilization field both in v1beta2 and v1beta3
        # of autoscaler API so it's enough to check if it's not null to know if
        # user set it. However loadBalancingUtilization and
        # customMetricUtilizations field are not present in v1beta2 of API so
        # trying to access their value when using v1beta2 API causes error.
        # So we need to check if those fields are present before checking if
        # they were set. We're checking if field is present by using hasattr not
        # by checking API version used because we want to print them when they
        # are supported in v1beta2 immediately after the support is added,
        # without waiting for gcloud release.
        if (hasattr(
            autoscaler.autoscalingPolicy, 'loadBalancingUtilization') and
            autoscaler.autoscalingPolicy.loadBalancingUtilization):
          as_dict['autoscalingPolicy']['loadBalancingUtilization'] = {
              'utilizationTarget': (
                  autoscaler.autoscalingPolicy.loadBalancingUtilization.
                  utilizationTarget)
          }
        if (hasattr(
            autoscaler.autoscalingPolicy, 'customMetricUtilizations') and
            autoscaler.autoscalingPolicy.customMetricUtilizations):
          as_dict['autoscalingPolicy']['customMetricUtilizations'] = [{
              'metric': item.metric,
              'utilizationTarget': item.utilizationTarget,
              'utilizationTargetType': UtilizationTargetTypeForItem(item)
          } for item in (autoscaler.autoscalingPolicy.customMetricUtilizations)
                                                                     ]
      return as_dict

    except exceptions.HttpError as error:
      raise calliope_exceptions.HttpException(util.GetErrorMessage(error))

  def Display(self, unused_args, result):
    print self.format(result)
