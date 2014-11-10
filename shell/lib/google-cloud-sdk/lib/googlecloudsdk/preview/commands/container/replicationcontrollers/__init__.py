# Copyright 2014 Google Inc. All Rights Reserved.

"""The main command group for cloud container clusters replication controllers.
"""
from googlecloudsdk.preview.lib.container import util as c_util


class ReplicationControllers(c_util.BaseKubecfgGroup):
  """Manage Kubernetes ReplicationControllers on a running cluster."""
  pass
