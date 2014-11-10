# Copyright 2014 Google Inc. All Rights Reserved.

"""This is the list clusters command."""
from googlecloudapis.apitools.base import py as apitools_base
from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import properties

from googlecloudsdk.preview.lib import util


class List(base.Command):
  """List existing clusters for running containers."""

  @staticmethod
  def Args(parser):
    """Register flags for this command."""
    pass

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

    req = messages.ContainerProjectsZonesClustersListRequest(
        projectId=project_id, zoneId=zone_id)
    try:
      return client.projects_zones_clusters.List(req)
    except apitools_base.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))

  def Display(self, args, result):
    """This method is called to print the result of the Run() method.

    Args:
      args: The arguments that command was run with.
      result: The value returned from the Run() method.
    """
    self.format(result)
