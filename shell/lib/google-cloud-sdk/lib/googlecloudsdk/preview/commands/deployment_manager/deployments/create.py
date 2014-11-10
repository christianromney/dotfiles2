# Copyright 2014 Google Inc. All Rights Reserved.

"""deployments create command."""

import time

from apiclient import errors

from googlecloudsdk.calliope import arg_parsers
from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.preview.lib import deployment_util
from googlecloudsdk.preview.lib import util


class Create(base.Command):
  """Insert (Create) a deployment using a specified template."""

  FINAL_DEPLOY_STATES = ['DEPLOYED', 'DEPLOYMENT_FAILED']

  @staticmethod
  def Args(parser):
    """Args is called by calliope to gather arguments for this command.

    Args:
      parser: An argparse parser that you can use to add arguments that go
          on the command line after this command. Positional arguments are
          allowed.
    """
    parser.add_argument('deployment', help='Deployment name.')
    parser.add_argument('--description', help='Deployment description.',
                        default=None)
    parser.add_argument(
        '--overrides', nargs='*',
        action=arg_parsers.AssociativeList(),
        help='Optional "Path=Value Path=Value ..." template overrides. '
        'Overrides must be a list of space delimited key=value pairs. '
        'This overrides existing template settings for this deployment '
        'but does not change the template itself. Example: '
        '--overrides $.modules.foo.replicaPoolModule.numReplicas=20 '
        ' $.modules.bar.replicaPoolModule.numReplicas=5 ...')
    parser.add_argument('--template', required=True,
                        help='Name of the template to use. The template must '
                        'exist in the system before creating the deployment.')
    parser.add_argument(
        '--no-poll-for-completion',
        help='Do not poll continuously for deployment completion. If set, '
        'only one Get API call is made. (default=False)',
        dest='no_polling',
        default=False,
        action='store_true')
    parser.add_argument(
        '--polling-period',
        help='Number of seconds to sleep between consecutive polls. '
        'Only effective when --poll-for-completion is provided.',
        default=10)

  def Run(self, args):
    """Run 'deployments create'.

    Args:
      args: argparse.Namespace, The arguments that this command was invoked
          with.

    Raises:
      HttpException: A http error response was received while executing api
          request.
      ToolException: An error other than http error occured while executing
          the command.
    """
    client = self.context['manager']
    project = properties.VALUES.core.project.Get(required=True)

    new_deployment = {}
    new_deployment['name'] = args.deployment
    new_deployment['templateName'] = args.template

    if args.polling_period < 1 and not args.no_polling:
      raise exceptions.InvalidArgumentException(
          '--polling-period', 'Polling period must be >= 1 second.')

    if args.description:
      new_deployment['description'] = args.description
    new_deployment['overrides'] = []

    if args.overrides:
      for path, value in args.overrides.iteritems():
        override = {}
        override['path'] = path
        override['value'] = value
        new_deployment['overrides'].append(override)

    request = client.deployments().insert(
        projectId=project, region=args.region, body=new_deployment)

    try:
      request.execute()
      log.Print('Insert API successfully called for deployment {0}.'.format(
          args.deployment))

      if args.no_polling:
        log.Print('Deployment {0} is being created...'.format(
            args.deployment))
        return

      log.Print('Polling for deployment {0} status:'.format(args.deployment))
      get_request = client.deployments().get(
          projectId=project, region=args.region,
          deploymentName=args.deployment)
      # TODO(user): Add support for a --max-polls or similar flag
      while True:
        response = get_request.execute()
        if deployment_util.DeploymentIsComplete(
            response, Create.FINAL_DEPLOY_STATES):
          log.Print('Deployment {0} created.'.format(args.deployment))
          return
        log.Print('Next poll in {0} (s)'.format(args.polling_period))
        time.sleep(args.polling_period)

    except errors.HttpError as error:
      raise exceptions.HttpException(util.GetError(error))
    except errors.Error as error:
      # pylint:disable=nonstandard-exception, ToolException is an Exception.
      raise exceptions.ToolException.FromCurrent()
