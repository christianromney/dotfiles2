# Copyright 2014 Google Inc. All Rights Reserved.

"""This is the create cluster command."""
import random
import string

from googlecloudapis.apitools.base import py as apitools_base
from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import properties

from googlecloudsdk.preview.lib import util
from googlecloudsdk.preview.lib.container import util as c_util


class Create(base.Command):
  """Create a cluster for running containers."""

  @staticmethod
  def Args(parser):
    """Register flags for this command."""
    parser.add_argument('name', help='The name of this cluster.')
    parser.add_argument(
        '--num-nodes',
        type=int,
        help='The number of nodes in the cluster.',
        default=3)
    parser.add_argument(
        '--machine-type', '-m',
        help='The type of machine to use for workers. Defaults to '
        'server-specified')
    parser.add_argument(
        '--source-image',
        help='The source image to use for workers. Defaults to '
        'server-specified')
    parser.add_argument(
        '--user', '-u',
        help='The user name to use for cluster auth.',
        default='admin')
    parser.add_argument(
        '--password',
        help='The password to use for cluster auth.')
    parser.add_argument(
        '--cluster-api-version',
        help='The kubernetes release version to launch the cluster with. '
        'Defaults to server-specified.')
    parser.add_argument(
        '--no-healthcheck',
        help='Do not wait for the cluster api to initialize after creation. '
        'If set, return cluster description as soon as it is created.',
        action='store_true')

  @exceptions.RaiseToolExceptionInsteadOf(c_util.Error)
  def Run(self, args):
    """This is what gets called when the user runs this command.

    Args:
      args: an argparse namespace. All the arguments that were provided to this
        command invocation.

    Returns:
      Some value that we want to have printed later.
    """
    client = self.context['container-api']
    messages = self.context['container_messages']
    project_id = properties.VALUES.core.project.Get(required=True)
    resources = self.context['container_registry']
    zone_id = resources.Parse(args.zone, collection='compute.zones').zone

    if args.password:
      password = args.password
    else:
      password = ''.join(random.SystemRandom().choice(
          string.ascii_letters + string.digits) for _ in range(16))

    node_config = messages.NodeConfig()
    if args.machine_type:
      node_config.machineType = args.machine_type
    if args.source_image:
      node_config.sourceImage = args.source_image

    create_cluster_req = messages.CreateClusterRequest(
        cluster=messages.Cluster(
            name=args.name,
            numNodes=args.num_nodes,
            nodeConfig=node_config,
            masterAuth=messages.MasterAuth(user=args.user,
                                           password=password)))
    if args.cluster_api_version:
      create_cluster_req.cluster.clusterApiVersion = args.cluster_api_version

    req = messages.ContainerProjectsZonesClustersCreateRequest(
        createClusterRequest=create_cluster_req,
        projectId=project_id,
        zoneId=zone_id)

    cluster = None
    try:
      operation = client.projects_zones_clusters.Create(req)

      operation = c_util.WaitForOperation(
          operation, project_id, zone_id, self.context,
          'Waiting for cluster creation')

      c_util.CheckOperationSucceeded(operation, messages)
      # Get Cluster
      cluster = c_util.DescribeCluster(args.name, zone_id, project_id,
                                       self.context)
      c_util.CheckClusterIsValid(cluster, messages)
    except apitools_base.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))

    if not args.no_healthcheck:
      healthy = c_util.HealthCheckCluster(cluster.endpoint,
                                          cluster.masterAuth.user,
                                          cluster.masterAuth.password)
      if healthy:
        c_util.ContainerConfig.Persist(cluster, project_id)
        c_util.FetchCertFiles(self.entry_point, args.name, zone_id, project_id)

    return cluster

  def Display(self, args, result):
    """This method is called to print the result of the Run() method.

    Args:
      args: The arguments that command was run with.
      result: The value returned from the Run() method.
    """
    self.format(result)

