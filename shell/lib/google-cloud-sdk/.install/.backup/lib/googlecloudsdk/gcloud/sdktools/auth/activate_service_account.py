# Copyright 2013 Google Inc. All Rights Reserved.

"""A simple auth command to bootstrap authentication with oauth2."""

import getpass
import os

from oauth2client import client

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions as c_exc
from googlecloudsdk.core import config
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.core.credentials import store as c_store


class ActivateServiceAccount(base.Command):
  """Get credentials via the private key for a service account.

  Get credentials for a service account, using a .p12 file for the private key.
  If --project is set, set the default project.
  """

  @staticmethod
  def Args(parser):
    """Set args for serviceauth."""
    parser.add_argument('account', help='The email for the service account.')
    parser.add_argument('--key-file',
                        help=('Path to the service accounts private key.'),
                        required=True)
    group = parser.add_mutually_exclusive_group()
    group.add_argument('--password-file',
                       help=('Path to a file containing the password for the '
                             'service account private key.'))
    group.add_argument('--prompt-for-password', action='store_true',
                       help=('Prompt for the password for the service account '
                             'private key.'))

  def Run(self, args):
    """Create service account credentials."""

    try:
      private_key = open(args.key_file).read()
    except IOError as e:
      raise c_exc.BadFileException(e)

    password = None
    if args.password_file:
      try:
        password = open(args.password_file).read().strip()
      except IOError as e:
        raise c_exc.UnknownArgumentException('--password-file', e)
    if args.prompt_for_password:
      password = getpass.getpass('Password: ')

    if not client.HAS_CRYPTO:
      if not os.environ.get('CLOUDSDK_PYTHON_SITEPACKAGES'):
        raise c_exc.ToolException(
            ('PyOpenSSL is not available. If you have already installed '
             'PyOpenSSL, you will need to enable site packages by '
             'setting the environment variable CLOUDSDK_PYTHON_SITEPACKAGES to '
             '1. If that does not work, See '
             'https://developers.google.com/cloud/sdk/crypto for details.'))
      else:
        raise c_exc.ToolException(
            ('PyOpenSSL is not available. See '
             'https://developers.google.com/cloud/sdk/crypto for details.'))

    if password:
      cred = client.SignedJwtAssertionCredentials(
          service_account_name=args.account,
          private_key=private_key,
          scope=config.CLOUDSDK_SCOPES,
          private_key_password=password,
          user_agent=config.CLOUDSDK_USER_AGENT)
    else:
      cred = client.SignedJwtAssertionCredentials(
          service_account_name=args.account,
          private_key=private_key,
          scope=config.CLOUDSDK_SCOPES,
          user_agent=config.CLOUDSDK_USER_AGENT)

    c_store.Store(cred, args.account)

    properties.PersistProperty(properties.VALUES.core.account, args.account)

    project = args.project
    if project:
      properties.PersistProperty(properties.VALUES.core.project, project)

    return cred

  def Display(self, args, result):
    if result:
      log.Print('Activated service account credentials for %s.' % args.account)
