# Copyright 2014 Google Inc. All Rights Reserved.

"""templates list command."""

from apiclient import errors

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib import util


class List(base.Command):
  """Lists all template resources for a given project."""

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
    """Run 'templates list'.

    Args:
      args: argparse.Namespace, The arguments that this command was invoked
          with.

    Returns:
      An object representing the service response obtained by the API if the
      call was successful.

    Raises:
      HttpException: A http error response was received while executing api
          request.
      ToolException: An error other than http error occured while executing
          the command.
    """
    client = self.context['manager']
    project = properties.VALUES.core.project.Get(required=True)

    limit = util.SanitizeLimitFlag(args.limit)

    request = client.templates().list(projectId=project)
    results = []
    try:
      response = request.execute()
      self.AppendResults(results, response)
      while response and 'nextPageToken' in response and len(results) < limit:
        request = client.templates().list(
            projectId=project, pageToken=response['nextPageToken'])
        response = request.execute()
        self.AppendResults(results, response)

      if len(results) > limit:
        results = results[0:limit]

      return results
    except errors.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))
    except errors.Error as error:
      # pylint:disable=nonstandard-exception, ToolException is an Exception.
      raise exceptions.ToolException.FromCurrent()

  def AppendResults(self, results, response):
    # TODO(user): refactor this to a common library when we move to apitools
    if results is None or response is None:
      raise ValueError('Unexpected input! ' + results + ' ' + response)
    if response and 'resources' in response:
      results.extend(response['resources'])

  def Display(self, unused_args, result):
    """Display prints information about what just happened to stdout.

    Args:
      unused_args: The same as the args in Run.

      result: a list of dicts, where each dict has a key 'name' and a value
          which is a Template name

    Raises:
      ValueError: if result is None or not a list
    """
    if not isinstance(result, list):
      raise ValueError('result must be a list')

    if not result:
      log.Print('No Templates were found in your project!')
      return

    for template in result:
      log.Print(template['name'])
