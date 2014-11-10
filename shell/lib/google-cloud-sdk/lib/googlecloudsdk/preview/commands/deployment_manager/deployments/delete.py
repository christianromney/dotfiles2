# Copyright 2014 Google Inc. All Rights Reserved.

"""deployments delete command."""

import time

from apiclient import errors

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib import deployment_util
from googlecloudsdk.preview.lib import util


class Delete(base.Command):
  """Delete a deployment."""

  FINAL_DELETE_STATES = ['DELETE_FAILED', 'DELETED']

  @staticmethod
  def Args(parser):
    """Args is called by calliope to gather arguments for this command.

    Args:
      parser: An argparse parser that you can use to add arguments that go
          on the command line after this command. Positional arguments are
          allowed.
    """
    parser.add_argument('deployment', help='Deployment name.')
    parser.add_argument(
        '--no-poll-for-completion',
        help='Do not poll continuously for deployment completion. If set, '
        'this command only makes one GET request to the API. (default=False)',
        dest='no_polling',
        default=False,
        action='store_true')
    parser.add_argument(
        '--polling-period',
        help='Number of seconds to sleep between consecutive polls. '
        'Only effective when --poll-for-completion is provided.',
        default=10)

  def Run(self, args):
    """Run 'deployments delete'.

    Args:
      args: argparse.Namespace, The arguments that this command was invoked
          with.

    Raises:
      HttpException: A http error response was received while executing api
          request.
      ToolException: An error other than http error occured while executing
          the command.
    """
    if args.polling_period < 1 and not args.no_polling:
      raise exceptions.InvalidArgumentException(
          '--polling-period', 'Polling period must be >= 1 second.')

    client = self.context['manager']
    project = properties.VALUES.core.project.Get(required=True)

    request = client.deployments().delete(
        projectId=project, region=args.region,
        deploymentName=args.deployment)

    try:
      request.execute()
      log.Print('Delete API successfully called for deployment {0}.'.format(
          args.deployment))

      if args.no_polling:
        log.Print('Deployment {0} is being deleted...'.format(args.deployment))
        return

      log.Print('Polling for deployment {0} status:'.format(args.deployment))
      get_request = client.deployments().get(
          projectId=project, region=args.region,
          deploymentName=args.deployment)
      # TODO(user): Add support for a --max-polls or similar flag
      while True:
        response = get_request.execute()
        if deployment_util.DeploymentIsComplete(
            response, Delete.FINAL_DELETE_STATES):
          util.PrettyPrint(response)
          return
        log.Print('Next poll in {0} (s)'.format(args.polling_period))
        time.sleep(args.polling_period)
    except errors.HttpError as error:
      # TODO(user): Change HttpError to some other lib so we don't have to
      # resort to infer the HTTP error code from the string content
      if '"code": 404' in error.content:
        log.Print('Deployment {0} no longer exists.'.format(
            args.deployment))
        return
      else:
        raise exceptions.HttpException(util.GetError(error))
    except errors.Error as error:
      # pylint:disable=nonstandard-exception, ToolException is an Exception.
      raise exceptions.ToolException.FromCurrent()
