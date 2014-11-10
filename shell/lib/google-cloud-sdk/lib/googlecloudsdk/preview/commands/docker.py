# Copyright 2014 Google Inc. All Rights Reserved.

"""Provides the docker CLI access to the Google Container Registry.

Sets docker up to authenticate with the Google Container Registry,
and passes all flags after -- to the docker CLI.
"""

import base64
import json
import os
import subprocess
import sys
import textwrap

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions as c_exc
from googlecloudsdk.core import log
from googlecloudsdk.core.credentials import store as c_store

# NOTE: Other tools like the python docker library (used by gcloud app)
# also relies on .dockercfg (in addition to the docker CLI client)
DOCKERCFG = os.path.join(os.path.expanduser('~'), '.dockercfg')

USERNAME = '_token'

DEFAULT_REGISTRY = 'container.cloud.google.com'


def ReadDockerConfig():
  with open(DOCKERCFG, 'r') as reader:
    return reader.read()


def WriteDockerConfig(contents):
  with open(DOCKERCFG, 'w') as writer:
    writer.write(contents)


@base.Hidden
class Init(base.Command):
  """Provides the docker CLI access to the Google Container Registry."""

  detailed_help = {
      'DESCRIPTION': textwrap.dedent("""\
          The docker sub-command of gcloud wraps docker commands, so that
          gcloud can inject the appropriate fresh authentication token into
          requests that interact with the docker registry.  As commands are
          simply passed through to docker, see this for a full reference of
          command-line options that can be supplied after the --
             http://docs.docker.com/reference/commandline/cli/
      """),
      'EXAMPLES': textwrap.dedent("""\
          Pull the image '{registry}/my-org/my-image' from the docker registry:
            gcloud preview docker -- pull {registry}/my-org/my-image

          Push the image '{registry}/foo/bar:baz' to our private docker registry.
            gcloud preview docker -- push {registry}/foo/bar:baz

          Configure authentication, then simply use docker:
            gcloud preview docker --authorize_only
            docker push {registry}/foo/bar:baz
      """.format(registry=DEFAULT_REGISTRY)),
  }

  @staticmethod
  def Args(parser):
    parser.add_argument(
        '--server', '-s',
        help='The address of the Google Cloud Registry',
        required=False,
        default='https://{registry}'.format(registry=DEFAULT_REGISTRY))

    # TODO(user): This should evolve into something that launches an
    # auth daemon process, or utilizes a more permanent credential.
    parser.add_argument(
        '--authorize_only', '-a',
        help='Configure docker authorization only, do not launch the '
        'docker command-line',
        action='store_true')

    parser.add_argument(
        '--docker-host',
        help='The URL to connect to Docker Daemon. Format: tcp://host:port or '
        'unix:///path/to/socket.')

    parser.add_argument(
        'extra_args', nargs='*', default=[],
        help='Arguments to pass to docker')

  def _UpdateDockerCredentials(self, server):
    """Updates the docker config to have fresh credentials."""
    # Loading credentials will ensure that we're logged in.
    # And prompt/abort to 'run gcloud auth login' otherwise.
    cred = c_store.Load()

    # Ensure our credential has a valid access token
    # NOTE: Load should have refreshed the token as needed.
    if not cred.access_token:
      raise c_exc.ToolException('No access token could be obtained '
                                'from the current credentials.')

    # Update the docker configuration file passing the access token
    # as a password, and a benign value as the username.
    self._UpdateDockerConfig(server, USERNAME, cred.access_token)

  def _UpdateDockerConfig(self, server, username, password):
    """Register the username/password for the given server in '.dockercfg'."""

    # NOTE: using "docker login" doesn't work as they're quite strict on what
    # is allowed in username/password.
    try:
      dockercfg_contents = json.loads(ReadDockerConfig())
    except IOError:
      # If the file doesn't exist, start with an empty map.
      dockercfg_contents = {}

    # Add the entry for our server.
    auth = base64.b64encode(username + ':' + password)
    dockercfg_contents[server] = {'auth': auth, 'email': 'not@val.id'}

    # TODO(user): atomic replace?
    # Be nice and pretty-print.
    WriteDockerConfig(json.dumps(dockercfg_contents, indent=2))

  def Run(self, args):
    """Executes the given docker command, after refreshing our credentials.

    Args:
      args: argparse.Namespace, An object that contains the values for
         the arguments specified in the .Args() method.
    """

    self._UpdateDockerCredentials(args.server)
    if args.authorize_only:
      # NOTE: We don't know at this point how long the access token we have
      # placed in the docker configuration will last.  More information needs
      # to be exposed from all credential kinds in order for us to have an
      # accurate awareness of lifetime here.
      log.err.Print('Short-lived access for {server} configured.'.format(
          server=args.server))
      return

    # TODO(user): reconcile with the 'gcloud app' docker stuff,
    # which should be using a gcloud config property.
    extra_args = (args.extra_args if not args.docker_host else
                  ['-H', args.docker_host] + args.extra_args)

    subprocess.check_call(['docker'] + extra_args,
                          stdin=sys.stdin,
                          stdout=sys.stdout,
                          stderr=sys.stderr)
    return
