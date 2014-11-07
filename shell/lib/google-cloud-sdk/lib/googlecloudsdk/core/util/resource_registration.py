# Copyright 2013 Google Inc. All Rights Reserved.

"""One-line documentation for resource_registration module.

A detailed description of resource_registration.
"""

from googlecloudsdk.core import resources


def RegisterReleasedAPIs():
  """Register all official versions of released Cloud APIs.

  """
  # pylint:disable=g-import-not-at-top
  from googlecloudapis.compute import v1 as compute_v1
  from googlecloudapis.developerprojects import v1 as projects_v1
  from googlecloudapis.dns import v1beta1 as dns_v1beta1
  from googlecloudapis.manager import v1beta2 as manager_v1beta2
  from googlecloudapis.replicapool import v1beta1 as replicapool_v1beta1
  from googlecloudapis.resourceviews import v1beta1 as resourceviews_v1beta1
  from googlecloudapis.sqladmin import v1beta3 as sqladmin_v1beta3
  resources.RegisterAPI(compute_v1.ComputeV1(get_credentials=False))
  resources.RegisterAPI(projects_v1.DeveloperprojectsV1(get_credentials=False))
  resources.RegisterAPI(dns_v1beta1.DnsV1beta1(get_credentials=False))
  resources.RegisterAPI(manager_v1beta2.ManagerV1beta2(get_credentials=False))
  resources.RegisterAPI(
      replicapool_v1beta1.ReplicapoolV1beta1(get_credentials=False))
  resources.RegisterAPI(
      resourceviews_v1beta1.ResourceviewsV1beta1(get_credentials=False))
  resources.RegisterAPI(sqladmin_v1beta3.SqladminV1beta3(get_credentials=False))
  from googlecloudapis.autoscaler import v1beta2 as autoscaler_v1beta2
  resources.RegisterAPI(
      autoscaler_v1beta2.AutoscalerV1beta2(get_credentials=False))


def RegisterUnreleasedAPIs():
  pass
