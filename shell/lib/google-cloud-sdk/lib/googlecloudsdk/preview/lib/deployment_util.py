# Copyright 2014 Google Inc. All Rights Reserved.

"""Common helper method for DeploymentManager Deployments."""

from googlecloudsdk.preview.lib import util


def DeploymentIsComplete(get_response, expected_final_states):
  """Determines whether a Deployment has finished or not.

  This method checks the contents of a GET API call response, and matches the
  Deployment Status with a list of known to be terminal Deployment Status'

  Args:
    get_response: the Get API response
    expected_final_states: a list of expected Statuses to look for

  Returns:
    True if the get_response indicates that the Deployment has finished,
    and False otherwise.

  Raises:
    ValueError: if the get_response input is invalid.
  """
  # isinstance also checks for None
  if not isinstance(get_response, dict) or 'state' not in get_response:
    raise ValueError('DeploymentIsComplete called with invalid get_response')

  modules = get_response['modules']
  module_status = {}
  for module, details in modules.iteritems():
    module_status[module] = details['state']['status']

  util.PrettyPrint(module_status)

  deployment_status = get_response['state']['status']
  if deployment_status in expected_final_states:
    return True

  return False
