# Copyright 2014 Google Inc. All Rights Reserved.

"""This is the list pods command."""
from googlecloudsdk.preview.lib.container import util as c_util


class List(c_util.BaseKubecfgCommand):
  """List pods running on a container cluster."""

  @staticmethod
  def Args(parser):
    """Register flags for this command."""
    c_util.BaseKubecfgCommand.Args(parser)

  def Run(self, args):
    """This is what gets called when the user runs this command.

    Args:
      args: an argparse namespace. All the arguments that were provided to this
        command invocation.

    Returns:
      Some value that we want to have printed later.
    """
    cluster_config = super(List, self).Run(args)
    return self.CallKubecfg(cluster_config, ['list', 'pods'])

