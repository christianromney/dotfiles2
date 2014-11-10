# Copyright 2014 Google Inc. All Rights Reserved.

"""This is the create pod command."""
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.preview.lib.container import util as c_util


def _ValidateArgs(args):
  """Validate the passed in arguments.

  Args:
    args: The args to validate

  Throws:
    exceptions.ToolException if any arg is missing or invalid.
  """
  if not args.name:
    raise exceptions.InvalidArgumentException('--name', 'Missing name')
  if not args.image:
    raise exceptions.InvalidArgumentException('--image', 'Missing image')
  if not args.port:
    raise exceptions.InvalidArgumentException('--port', 'Missing port')
  try:
    port_num = int(args.port)
  except ValueError:
    raise exceptions.InvalidArgumentException('--port', 'Not a number')
  if port_num <= 0:
    raise exceptions.InvalidArgumentException('--port', 'Not a positive number')


class Create(c_util.BaseKubecfgCommand):
  """Create a pod running in a cluster."""

  @staticmethod
  def Args(parser):
    """Register flags for this command."""
    c_util.BaseKubecfgCommand.Args(parser)
    parser.add_argument('--config-file',
                        help='Path to the pod config')
    parser.add_argument('--image', help='The Docker image to use')
    parser.add_argument('--port', help='A port to expose')
    parser.add_argument('name', help='The name of the pod')

  def Run(self, args):
    """This is what gets called when the user runs this command.

    Args:
      args: an argparse namespace. All the arguments that were provided to this
        command invocation.

    Returns:
      Some value that we want to have printed later.
    """
    cluster_config = super(Create, self).Run(args)
    if args.config_file:
      config_file = args.config_file
    else:
      _ValidateArgs(args)
      pod = c_util.MakePod(args.name, args.image, int(args.port))
      config_file = c_util.WriteTemporaryConfigFile(pod)

    return self.CallKubecfg(
        cluster_config,
        ['-c', config_file, 'create', 'pods'])
