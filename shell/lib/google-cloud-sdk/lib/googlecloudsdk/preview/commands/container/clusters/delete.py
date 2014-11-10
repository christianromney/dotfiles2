# Copyright 2014 Google Inc. All Rights Reserved.

"""This is the delete cluster command."""
from googlecloudapis.apitools.base import py as apitools_base
from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import properties

from googlecloudsdk.preview.lib import util
from googlecloudsdk.preview.lib.container import util as c_util


class Delete(base.Command):
  """Delete an existing cluster for running containers."""

  @staticmethod
  def Args(parser):
    """Register flags for this command."""
    parser.add_argument('name', help='The name of this cluster.')

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

    req = messages.ContainerProjectsZonesClustersDeleteRequest(
        clusterId=args.name, projectId=project_id, zoneId=zone_id)

    try:
      operation = client.projects_zones_clusters.Delete(req)

      # Return the operation when it succeeds or times out
      operation = c_util.WaitForOperation(
          operation, project_id, zone_id, self.context,
          'Waiting for cluster deletion')
      c_util.CheckOperationSucceeded(operation, messages)
    except apitools_base.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))

    # Purge cached config files for the cluster
    c_util.ContainerConfig.Purge(args.name, zone_id, project_id)

    return operation

  def Display(self, args, result):
    """This method is called to print the result of the Run() method.

    Args:
      args: The arguments that command was run with.
      result: The value returned from the Run() method.
    """
    self.format(result)

