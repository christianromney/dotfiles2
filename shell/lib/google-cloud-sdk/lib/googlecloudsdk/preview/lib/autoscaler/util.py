# Copyright 2014 Google Inc. All Rights Reserved.
"""Common utility functions for Autoscaler processing."""

import json

from googlecloudapis.apitools.base.py import exceptions
from googlecloudsdk.calliope import arg_parsers
from googlecloudsdk.calliope import exceptions

# TODO(user): Use generated list of possible enum values.
ALLOWED_UTILIZATION_TARGET_TYPES = ['GAUGE', 'DELTA_PER_SECOND',
                                    'DELTA_PER_MINUTE']


def AddAutoscalerArgs(parser):
  """Adds commandline arguments to parser."""
  parser.add_argument('name', help='Autoscaler name.')
  parser.add_argument('--target', help='Resource to be scaled.')
  parser.add_argument('--cool-down-period', type=arg_parsers.Duration(),
                      help='Number of seconds Autoscaler will wait between '
                      'resizing collection.')
  parser.add_argument('--description', help='Notes about Autoscaler.')
  parser.add_argument('--min-num-replicas', type=int,
                      help='Minimum number of replicas Autoscaler will set.')
  parser.add_argument('--max-num-replicas', type=int,
                      help='Maximum number of replicas Autoscaler will set.')
  parser.add_argument('--target-cpu-utilization', type=float,
                      help='CPU utilization level Autoscaler will aim to '
                      'maintain (0.0 to 1.0).')
  parser.add_argument('--custom-metric', type=str, help='A Google Cloud '
                      'Monitoring instance metric (see '
                      'https://developers.google.com/cloud-monitoring/metrics'
                      ').')
  parser.add_argument('--target-custom-metric-utilization', type=float,
                      help='Custom metric level Autoscaler will aim to '
                      'maintain (greater than 0.0).')
  parser.add_argument('--custom-metric-utilization-target-type', type=str,
                      help='Type of target you specified, you can choose from '
                      'the following: ' +
                      ', '.join(ALLOWED_UTILIZATION_TARGET_TYPES) + '.')
  parser.add_argument('--target-load-balancer-utilization', type=float,
                      help='Load balancer utilization level Autoscaler will '
                      'aim to maintain (greater than 0.0).')


def GetErrorMessage(error):
  content_obj = json.loads(error.content)
  return content_obj.get('error', {}).get('message', '')


def PrepareAutoscaler(args, messages):
  """Validates args, returns prepared body for create/update/patch request."""
  if args.min_num_replicas and args.min_num_replicas < 0:
    raise exceptions.ToolException('min num replicas can\'t be negative.')

  if args.max_num_replicas and args.max_num_replicas < 0:
    raise exceptions.ToolException('max num replicas can\'t be negative.')

  if args.min_num_replicas and args.max_num_replicas:
    if args.min_num_replicas > args.max_num_replicas:
      raise exceptions.ToolException(
          'max num replicas can\'t be less than min num replicas.')

  if not args.max_num_replicas:
    raise exceptions.ToolException('you need to provide max num replicas.')

  if not args.target:
    raise exceptions.ToolException('you need to provide target.')

  specified_configs = 0
  if args.target_cpu_utilization:
    specified_configs += 1
    if args.target_cpu_utilization > 1.:
      raise exceptions.ToolException(
          'target cpu utilization can\'t be grater than 1.')
    if args.target_cpu_utilization < 0.:
      raise exceptions.ToolException(
          'target cpu utilization can\'t be lesser than 0.')

  if (args.custom_metric or args.target_custom_metric_utilization or
      args.custom_metric_utilization_target_type):
    if (args.custom_metric and args.target_custom_metric_utilization and
        args.custom_metric_utilization_target_type):
      specified_configs += 1
      if args.target_custom_metric_utilization <= 0.:
        raise exceptions.ToolException(
            'target custom metric utilization can\'t be lesser than 0.')
      if (args.custom_metric_utilization_target_type not in
          ALLOWED_UTILIZATION_TARGET_TYPES):
        raise exceptions.ToolException(
            'Unexpected value for --custom-metric-utilization-target-type: '
            '\'' + args.custom_metric_utilization_target_type + '\', expected '
            'one of: ' + ', '.join(ALLOWED_UTILIZATION_TARGET_TYPES) + '.')
    else:
      raise exceptions.ToolException(
          'you need to provide all three: --custom-metric, '
          '--target-custom-metric-utilization and '
          '--custom-metric-utilization-target-type.')

  if args.target_load_balancer_utilization:
    specified_configs += 1
    if args.target_load_balancer_utilization <= 0:
      raise exceptions.ToolException(
          'target load balancer utilization can\'t be lesser than 0.')

  if specified_configs > 1:
    raise exceptions.ToolException(
        'only one utilization target can be specified.')

  result = messages.Autoscaler()
  if args.description:
    result.description = args.description
  if args.name:
    result.name = args.name
  if args.target:
    result.target = args.target
  result.autoscalingPolicy = messages.AutoscalingPolicy()
  if args.cool_down_period:
    result.autoscalingPolicy.coolDownPeriodSec = args.cool_down_period
  if args.max_num_replicas:
    result.autoscalingPolicy.maxNumReplicas = args.max_num_replicas
  if args.min_num_replicas:
    result.autoscalingPolicy.minNumReplicas = args.min_num_replicas
  if args.custom_metric:
    result.autoscalingPolicy.customMetricUtilizations = [
        messages.AutoscalingPolicyCustomMetricUtilization()]
    result.autoscalingPolicy.customMetricUtilizations[0].utilizationTarget = (
        args.target_custom_metric_utilization)
    result.autoscalingPolicy.customMetricUtilizations[0].metric = (
        args.custom_metric)
    (result.autoscalingPolicy.customMetricUtilizations[0].
     utilizationTargetType) = args.custom_metric_utilization_target_type
  if args.target_cpu_utilization:
    result.autoscalingPolicy.cpuUtilization = (
        messages.AutoscalingPolicyCpuUtilization())
    result.autoscalingPolicy.cpuUtilization.utilizationTarget = (
        args.target_cpu_utilization)
  if args.target_load_balancer_utilization:
    result.autoscalingPolicy.loadBalancingUtilization = (
        messages.AutoscalingPolicyLoadBalancingUtilization())
    result.autoscalingPolicy.loadBalancingUtilization.utilizationTarget = (
        args.target_load_balancer_utilization)

  return result
