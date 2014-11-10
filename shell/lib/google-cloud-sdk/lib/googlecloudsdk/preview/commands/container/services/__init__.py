# Copyright 2014 Google Inc. All Rights Reserved.

"""The main command group for cloud container clusters services."""
from googlecloudsdk.preview.lib.container import util as c_util


class Services(c_util.BaseKubecfgGroup):
  """Manage Kubernetes Services on a running cluster."""
  pass
