"""Assorted utilities shared between parts of apitools."""

import collections
import httplib
import os
import random
import types
import urllib
import urllib2

from googlecloudapis.apitools.base.py import exceptions

__all__ = [
    'DetectGae',
    'DetectGce',
]

_RESERVED_URI_CHARS = r":/?#[]@!$&'()*+,;="


def DetectGae():
  """Determine whether or not we're running on GAE.

  This is based on:
    https://developers.google.com/appengine/docs/python/#The_Environment

  Returns:
    True iff we're running on GAE.
  """
  server_software = os.environ.get('SERVER_SOFTWARE', '')
  return (server_software.startswith('Development/') or
          server_software.startswith('Google App Engine/'))


def DetectGce():
  """Determine whether or not we're running on GCE.

  This is based on:
    https://developers.google.com/compute/docs/instances#dmi

  Returns:
    True iff we're running on a GCE instance.
  """
  try:
    o = urllib2.urlopen('http://metadata.google.internal')
  except urllib2.URLError:
    return False
  return o.getcode() == httplib.OK


def NormalizeScopes(scope_spec):
  """Normalize scope_spec to a set of strings."""
  if isinstance(scope_spec, types.StringTypes):
    return set(scope_spec.split(' '))
  elif isinstance(scope_spec, collections.Iterable):
    return set(scope_spec)
  raise exceptions.TypecheckError(
      'NormalizeScopes expected string or iterable, found %s' % (
          type(scope_spec),))


def Typecheck(arg, arg_type, msg=None):
  if not isinstance(arg, arg_type):
    if msg is None:
      if isinstance(arg_type, tuple):
        msg = 'Type of arg is "%s", not one of %r' % (type(arg), arg_type)
      else:
        msg = 'Type of arg is "%s", not "%s"' % (type(arg), arg_type)
    raise exceptions.TypecheckError(msg)
  return arg


def ExpandRelativePath(method_config, params, relative_path=None):
  """Determine the relative path for request."""
  path = relative_path or method_config.relative_path or ''

  for param in method_config.path_params:
    param_template = '{%s}' % param
    # For more details about "reserved word expansion", see:
    #   http://tools.ietf.org/html/rfc6570#section-3.2.2
    reserved_chars = ''
    reserved_template = '{+%s}' % param
    if reserved_template in path:
      reserved_chars = _RESERVED_URI_CHARS
      path = path.replace(reserved_template, param_template)
    if param_template not in path:
      raise exceptions.InvalidUserInputError(
          'Missing path parameter %s' % param)
    try:
      # TODO(user): Do we want to support some sophisticated
      # mapping here?
      value = params[param]
    except KeyError:
      raise exceptions.InvalidUserInputError(
          'Request missing required parameter %s' % param)
    if value is None:
      raise exceptions.InvalidUserInputError(
          'Request missing required parameter %s' % param)
    try:
      if not isinstance(value, basestring):
        value = str(value)
      path = path.replace(param_template,
                          urllib.quote(value.encode('utf_8'), reserved_chars))
    except TypeError as e:
      raise exceptions.InvalidUserInputError(
          'Error setting required parameter %s to value %s: %s' % (
              param, value, e))
  return path


def CalculateWaitForRetry(retry_attempt, max_wait=60):
  """Calculates amount of time to wait before a retry attempt.

  Wait time grows exponentially with the number of attempts.
  A random amount of jitter is added to spread out retry attempts from different
  clients.

  Args:
    retry_attempt: Retry attempt counter.
    max_wait: Upper bound for wait time.

  Returns:
    Amount of time to wait before retrying request.
  """

  wait_time = 2 ** retry_attempt
  # randrange requires a nonzero interval, so we want to drop it if
  # the range is too small for jitter.
  if retry_attempt:
    max_jitter = (2 ** retry_attempt) / 2
    wait_time += random.randrange(-max_jitter, max_jitter)
  return min(wait_time, max_wait)
