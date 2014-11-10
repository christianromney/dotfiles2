# Copyright 2014 Google Inc. All Rights Reserved.

"""This is the describe cluster command."""
from googlecloudsdk.calliope import base
from googlecloudsdk.core import properties

from googlecloudsdk.preview.lib.container import util as c_util


class Describe(base.Command):
  """Describe an existing cluster for running containers."""

  @staticmethod
  def Args(parser):
    """Register flags for this command."""
    parser.add_argument('name', help='The name of this cluster.')

  def Run(self, args):
    """This is what gets called when the user runs this command.

    Args:
      args: an argparse namespace. All the arguments that were provided to this
        command invocation.

    Returns:
      Some value that we want to have printed later.
    """
    resources = self.context['container_registry']
    zone_id = resources.Parse(args.zone, collection='compute.zones').zone
    project_id = properties.VALUES.core.project.Get(required=True)
    return c_util.DescribeCluster(args.name, zone_id, project_id, self.context)

  def Display(self, args, result):
    """This method is called to print the result of the Run() method.

    Args:
      args: The arguments that command was run with.
      result: The value returned from the Run() method.
    """
    self.format(result)

