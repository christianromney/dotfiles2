# Copyright 2014 Google Inc. All Rights Reserved.

"""Hidden command for cluster healthchecking."""
from googlecloudsdk.calliope import base
from googlecloudsdk.core import log
from googlecloudsdk.core import properties

from googlecloudsdk.preview.lib.container import util as c_util


@base.Hidden
class HealthCheck(base.Command):
  """Healthcheck a running cluster."""

  @staticmethod
  def Args(parser):
    """Register flags for this command."""
    parser.add_argument('name', help='The name of this cluster.')
    parser.add_argument(
        '--save-config-files',
        action='store_true',
        help='Pull ssl certificate files from master and cache locally with '
        'endpoint/auth data.')

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

    cluster = c_util.DescribeCluster(args.name, zone_id, project_id,
                                     self.context)

    healthy = c_util.HealthCheckCluster(cluster.endpoint,
                                        cluster.masterAuth.user,
                                        cluster.masterAuth.password)
    if args.save_config_files:
      c_util.ContainerConfig.Persist(cluster, project_id)
      c_util.FetchCertFiles(self.entry_point, cluster.name, zone_id, project_id)
    return (cluster, healthy)

  def Display(self, args, result):
    """This method is called to print the result of the Run() method.

    Args:
      args: The arguments that command was run with.
      result: The value returned from the Run() method.
    """
    cluster, healthy = result
    log.out.Print('healthy:', healthy)
    log.out.Print(cluster)
