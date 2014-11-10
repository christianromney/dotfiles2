# Copyright 2014 Google Inc. All Rights Reserved.

"""The main command group for cloud container clusters pods."""
from googlecloudsdk.preview.lib.container import util as c_util


class Pods(c_util.BaseKubecfgGroup):
  """Manage Kubernetes pods on a running cluster."""
  pass
