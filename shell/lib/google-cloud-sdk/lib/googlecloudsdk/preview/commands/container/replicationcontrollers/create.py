# Copyright 2014 Google Inc. All Rights Reserved.

"""This is the create replicationController command."""
from googlecloudsdk.preview.lib.container import util as c_util


class Create(c_util.BaseKubecfgCommand):
  """Create a replicationController running in a cluster."""

  @staticmethod
  def Args(parser):
    """Register flags for this command."""
    c_util.BaseKubecfgCommand.Args(parser)
    parser.add_argument('--config-file',
                        required=True,
                        help='Path to the pod config')

  def Run(self, args):
    """This is what gets called when the user runs this command.

    Args:
      args: an argparse namespace. All the arguments that were provided to this
        command invocation.

    Returns:
      Some value that we want to have printed later.
    """
    cluster_config = super(Create, self).Run(args)
    return self.CallKubecfg(
        cluster_config,
        ['-c', args.config_file, 'create', 'replicationControllers'])

