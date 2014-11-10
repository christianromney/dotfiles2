# Copyright 2014 Google Inc. All Rights Reserved.

"""Instance groups list command."""

from apiclient import errors

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib import util


class List(base.Command):
  """List all instance groups for a given project."""

  @staticmethod
  def Args(parser):
    """Args is called by calliope to gather arguments for this command.

    Args:
      parser: An argparse parser that you can use to add arguments that go
          on the command line after this command. Positional arguments are
          allowed.
    """
    parser.add_argument('--limit', type=int,
                        help='The maximum number of results to list.')

  def Run(self, args):
    """Run 'instance-groups list'.

    Args:
      args: argparse.Namespace, The arguments that this command was invoked
          with.

    Returns:
      A list object representing the instance groups obtained by the List
      operation if the List API call was successful.

    Raises:
      HttpException: A http error response was received while executing api
          request.
      ToolException: An error other than http error occured while executing
          the command.
    """
    limit = util.SanitizeLimitFlag(args.limit)

    request = self.BuildRequest(args)
    results = []
    try:
      response = request.execute()
      self.AppendResults(results, response)
      while response and 'nextPageToken' in response and len(results) < limit:
        request = self.BuildRequest(args, response['nextPageToken'])
        response = request.execute()
        self.AppendResults(results, response)

      if len(results) > limit:
        results = results[0:limit]

      return results
    except errors.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))
    except errors.Error as error:
      raise exceptions.ToolException(error)

  def BuildRequest(self, args, page_token=None):
    client = self.context['instanceGroupsClient']
    project = properties.VALUES.core.project.Get(required=True)
    return client.list(
        project=project, zone=args.zone, pageToken=page_token)

  def AppendResults(self, results, response):
    # TODO(user): refactor this to a common library when we move to apitools
    if results is None or response is None:
      raise ValueError('Unexpected input! ' + results + ' ' + response)

    if response and 'items' in response:
      results.extend(response['items'])

  def Display(self, unused_args, results):
    """Display prints information about what just happened to stdout.

    Args:
      unused_args: The same as the args in Run.
      results: The results of the Run() method.
    Raises:
      ValueError: if result is None or not a list
    """
    if not isinstance(results, list):
      raise ValueError('results must be a list')

    for instance_group in results:
      log.Print(instance_group['selfLink'])
