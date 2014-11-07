# Copyright 2013 Google Inc. All Rights Reserved.

"""Module that allows adding git p2d credentials to a netrc file."""

import os
import re

from googlecloudsdk.calliope import exceptions as c_exc
from googlecloudsdk.core.credentials import store as c_store
from googlecloudsdk.core.util import files
from googlecloudsdk.core.util import platforms


_MACHINE_SUB_RE = re.compile(r"""
machine\s+code.google.com\s.*?
(?=(macdef|machine|default|\Z))
""", re.VERBOSE | re.DOTALL)

_NEW_MACHINE_FORMAT = """machine code.google.com
        login {login}
        password {password}

"""


def _ScrapeAndReplaceNetRC(netrc_data, new_machine):
  """Find a code.google.com entry and replace it with the new_machine.

  Args:
    netrc_data: str, The complete contents of the old netrc file.
    new_machine: str, The entry to add at the top of the processed file.

  Returns:
    str, The contents that can be written back to the netrc file.
  """

  filtered_netrc_data = _MACHINE_SUB_RE.sub('', netrc_data)
  return new_machine+filtered_netrc_data


def ActivateGitP2D(account, creds, netrc_path=None):
  """Modify the user's netrc file so that they can use git push-to-deploy.

  Args:
    account: str, The account that is being activated.
    creds: oauth2client.client.Credentials, The credentials that will be
        inspected for a refresh token.
    netrc_path: str, Path to an alternative netrc file.
  """

  if not creds.refresh_token:
    raise c_store.Error('Active credentials have no refresh token.')

  new_machine = _NEW_MACHINE_FORMAT.format(login=account,
                                           password=creds.refresh_token)

  if not netrc_path:
    if (platforms.OperatingSystem.Current() ==
        platforms.OperatingSystem.WINDOWS):
      # Yes, the right place on windows is "%HOME%\_netrc".
      netrc_name = '_netrc'
    else:
      netrc_name = '.netrc'
    try:
      netrc_path = os.path.join(os.environ['HOME'], netrc_name)
    except KeyError:
      raise c_exc.BadFileException(
          'Cannot find %s file ($HOME is not set).' % netrc_name)

  if not os.path.exists(netrc_path):
    netrc_data = ''
  else:
    with open(netrc_path) as netrc_file:
      netrc_data = netrc_file.read()
  new_data = _ScrapeAndReplaceNetRC(netrc_data, new_machine)

  with files.OpenForWritingPrivate(netrc_path) as netrc_file:
    netrc_file.write(new_data)
