# Copyright 2013 Google Inc. All Rights Reserved.

"""A module to make it easy to set up and run CLIs in the Cloud SDK."""

import os.path
import subprocess
import sys

import httplib2
from oauth2client import client


from googlecloudsdk.calliope import cli as calliope
from googlecloudsdk.core import config
from googlecloudsdk.core import exceptions
from googlecloudsdk.core import log
from googlecloudsdk.core.credentials import store as c_store


__all__ = ['CLILoader', 'GoogleCloudSDKPackageRoot', 'Credentials', 'Http']


class Error(exceptions.Error):
  """Exceptions for the cli module."""


class CannotRefreshAuthTokenError(Error):
  """An exception raised when the auth tokens fail to refresh."""

  def __init__(self, msg):
    auth_command = '$ gcloud auth login'
    message = ('There was a problem refreshing your current auth tokens: '
               '{0}.  Please run\n  {1}.'.format(msg, auth_command))
    super(CannotRefreshAuthTokenError, self).__init__(message)


class NoHelpFoundError(Error):
  """Raised when a help file cannot be located."""


def GetHelp(help_dir):
  """Returns a function that can display long help.

  Long help is displayed using the man utility if it's available on
  the user's platform. If man is not available, a plain-text version
  of help is written to standard out.

  Args:
    help_dir: str, The path to the directory containing help documents.

  Returns:
    func([str]), A function that can display help if help_dir exists,
    otherwise None.
  """
  def Help(path, default=None):
    """Displays help for the given subcommand.

    This function first attempts to display help using the man utility.
    If man is unavailable, a plain-text version of the help is printed
    to standard out.

    Args:
      path: A path representing the subcommand for which help is being
          requested (e.g., ['my-prog', 'my-subcommand'] if help is being
          requested for "my-prog my-subcommand").
      default: str, Text to print out if no help files can be found.

    Raises:
      HelpNotFound: If man is not available and no help exists for the
          given subcommand. Note that if man is available and no help exists,
          error reporting is deferred to man.
    """
    base = '_'.join(path)
    try:
      exit_code = subprocess.call(
          ['man',
           '-M', os.path.join(help_dir, 'man'),  # Sets the man search path.
           base,
          ])
      if exit_code == 0:
        return
      else:
        log.debug('man process returned with exit code %s', exit_code)
    except OSError as e:
      log.debug('There was a problem launching man: %s', e)

    log.debug('Falling back to plain-text help.')

    text_help_file_path = os.path.join(help_dir, 'text.long', base)
    try:
      with open(text_help_file_path) as f:
        sys.stdout.write(f.read())
    except IOError:
      if default:
        print default
      else:
        raise NoHelpFoundError(
            'No manual entry for command: {0}'.format(' '.join(path)))

  if help_dir and os.path.exists(help_dir):
    return Help
  else:
    return None


def CLILoader(name, command_root_directory, allow_non_existing_modules=False,
              version_func=None, help_dir=None):
  """Get a ready-to-go CLI for Cloud SDK tools.

  Args:
    name: str, The name of your CLI. Should probably be the same as the
        executable name.
    command_root_directory: str, The absolute path to the tools root.
    allow_non_existing_modules: bool, If true, module directories that don't
        exist will be ignored rather than cause errors.
    version_func: func, Function to call with -v, --version.
    help_dir: str, The path to the directory containing help documents or None
      if the CLI does not support man pages.

  Returns:
    calliope.CLILoader, An object that will run the tools from the command
        line.
  """
  paths = config.Paths()

  return calliope.CLILoader(
      name=name,
      command_root_directory=command_root_directory,
      load_context=None,
      logs_dir=paths.logs_dir,
      allow_non_existing_modules=allow_non_existing_modules,
      version_func=version_func,
      help_func=GetHelp(help_dir),
  )


def GoogleCloudSDKPackageRoot():
  return config.GoogleCloudSDKPackageRoot()


def Credentials():
  """Get the currently active credentials.

  This function loads account credentials via core.account property
  of core.properties module.

  These credentials will be refreshed before being returned, so it makes sense
  to cache the value returned for short-lived programs.

  Returns:
    An active, valid credentials object.

  Raises:
    c_store.Error: If an error loading the credentials occurs.
  """
  return c_store.Load()


def Http(auth=True, creds=None, timeout=None):
  """Get an httplib2.Http object for working with the Google API.

  Args:
    auth: bool, True if the http object returned should be authorized.
    creds: oauth2client.client.Credentials, If auth is True and creds is not
        None, use those credentials to authorize the httplib2.Http object.
    timeout: double, The timeout in seconds to pass to httplib2.  This is the
        socket level timeout.  If timeout is None, timeout is infinite.

  Returns:
    An authorized httplib2.Http object, or a regular httplib2.Http object if no
    credentials are available.
  """

  # TODO(user): Have retry-once-if-denied logic, to allow client tools to not
  # worry about refreshing credentials.

  http = httplib2.Http(timeout=timeout)
  if auth:
    if not creds:
      creds = Credentials()
    http = creds.authorize(http)
    # Take this out for now because it interferes with apitools ability to
    # refresh the token for batch requests.  b/18192994
    # http = _WrapRequest(http)
  return http


def _WrapRequest(http):
  """Wraps the original http.request method with one that wraps an exception.

  We need to do this because oauth2client does similar wrapping when you
  authorize the http object.  Because of this, a credential refresh error
  can get raised wherever someone makes an http request.  With no common place
  to handle this exception, we do more wrapping here so we can convert it to
  one of our typed exceptions.

  Args:
    http: The original http object.

  Returns:
    http, The same http object but with the request method wrapped.
  """
  orig_request = http.request
  def NewRequest(*args, **kwargs):
    try:
      return orig_request(*args, **kwargs)
    except client.AccessTokenRefreshError as e:
      log.debug('Exception caught during HTTP request: %s', e.message,
                exc_info=True)
      raise CannotRefreshAuthTokenError(e.message)
  http.request = NewRequest
  return http

