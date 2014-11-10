# Copyright 2014 Google Inc. All Rights Reserved.

"""Common utility functions for managing Resource Views 'service' data."""

from googlecloudsdk.calliope import exceptions


def RemoveServiceFromEndpoints(service_name, services):
  """Removes a service from a list of existing services.

  Args:
    service_name: string representing the name of the service to be removed
    services: a list of service dicts containing 'port' and 'name' keys

  Raises:
    ToolException: if the provided services parameter is in malformed

  Returns:
    a new list of service dicts (name and ports)
  """
  new_services = []
  if not isinstance(services, list):
    return new_services

  # TODO(user): Consider throwing an exception if the service is not
  # already configured in the list of endpoints.
  for service in services:
    if not isinstance(service, dict) or 'name' not in service:
      raise exceptions.ToolException(ValueError(
          'Services are expected to be service dicts!'))
    if service['name'] != service_name:
      new_services.append(service)

  return new_services


def AddServiceToEndpoints(service_name, service_port, services):
  """Adds a service_name:service_port to a list of existing services.

  Args:
    service_name: string representing the name to be set for the service
    service_port: integer representing the port to be set for the service
    services: a list of service dicts containing 'port' and 'name' keys

  Raises:
    ToolException: if the provided services parameter is in malformed

  Returns:
    The new list of service dicts including the new service:
      e.g.
      [
        {'name': service_foo, 'port': 12345},
        {'name': service_bar, 'port': 67890},
      ]

    If the service_name had already been present, the port corresponding to
    that service_name would be updated.
  """
  new_service = {'name': service_name, 'port': service_port}
  if not isinstance(services, list):
    return [new_service]

  for service in services:
    if not isinstance(service, dict) or 'name' not in service:
      raise exceptions.ToolException(ValueError(
          'Services are expected to be dicts!'))

    if service['name'] == service_name:
      service['port'] = service_port
      return services

  services.append(new_service)
  return services
