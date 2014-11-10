# Copyright 2014 Google Inc. All Rights Reserved.

"""Common utility functions for the Preview component."""

import json
import sys

from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import log
from googlecloudsdk.core.util import resource_printer


def GetError(error, verbose=False):
  """Returns a ready-to-print string representation from the http response.

  Args:
    error: A string representing the raw json of the Http error response.
    verbose: Whether or not to print verbose messages [default false]

  Returns:
    A ready-to-print string representation of the error.
  """
  data = json.loads(error.content)
  if verbose:
    PrettyPrint(data)
  code = data['error']['code']
  message = data['error']['message']
  return 'ResponseError: code={0}, message={1}'.format(code, message)


def SanitizeLimitFlag(limit):
  """Sanitizes and returns a limit flag value.

  Args:
    limit: the limit flag value to sanitize.
  Returns:
    Sanitized limit flag value.
  Raises:
    ToolException: if the provided limit flag value is not a positive integer
  """
  if limit is None:
    limit = sys.maxint
  else:
    if limit > sys.maxint:
      limit = sys.maxint
    elif limit <= 0:
      raise exceptions.ToolException(
          '--limit must be a positive integer; received: {0}'
          .format(limit))
  return limit


def PrettyPrint(resource, print_format='json'):
  """Prints the given resource."""
  resource_printer.Print(
      resources=[resource],
      print_format=print_format,
      out=log.out)


def GenerateInstanceUrl(project, zone, name):
  if project and zone and name:
    return ('https://www.googleapis.com/compute/v1/projects/{0}/'
            'zones/{1}/instances/{2}').format(project, zone, name)
  return None


def PrintTable(resources, resource_type):
  """Prints a table of the given resources.

  Args:
    resources: a list of resources to print into a table
    resource_type: the type of the resources to print, e.g. 'replica' or
      'replica-pool'

  Raises:
    ValueError: if an unsupported resource_type is provided
  """
  printer = resource_printer.TablePrinter(out=log.out)

  if not resources:
    return

  if resource_type == 'replica':
    header = ['name', 'status', 'templateVersion']
    printer.AddRow(header)
    for resource in resources:
      row = []
      row.append(resource['name'])
      row.append(resource['status']['templateVersion'])
      row.append(resource['status']['state'])
      printer.AddRow(row)
  elif resource_type == 'replica-pool':
    header = ['name', 'currentNumReplicas']
    printer.AddRow(header)
    for resource in resources:
      row = []
      row.append(resource['name'])
      row.append(str(resource['currentNumReplicas']))
      printer.AddRow(row)
  else:
    raise ValueError('Unsupported resource_type: {0}'.format(resource_type))

  printer.Print()
