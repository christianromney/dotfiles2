# Copyright 2014 Google Inc. All Rights Reserved.

"""List printer for Cloud Platform resources."""

from googlecloudsdk.core.util import attrpath
from googlecloudsdk.core.util import console_io


def PrintResourceList(collection, items):
  """Print a list of cloud resources.

  Args:
    collection: str, The name of the collection to which the items belong.
    items: iterable, A list or otherwise iterable object that generates the
        rows of the list.
  """
  console_io.PrintExtendedList(items, COLLECTION_COLUMNS[collection])


def _Select(path, transform=None):
  """Get a column fetcher for the given attr path and transform.

  Args:
    path: str, The attr path that keys into the resource.
    transform: func(str)->str, A func that takes something found by the path
        and maps it to some other strip.

  Returns:
    func(obj)->str, A func that takes an object and returns the value
    for a particular column.
  """

  getter = attrpath.Selector(path)

  if transform is None:
    return getter
  def GetAndTransform(obj):
    return transform(getter(obj))
  return GetAndTransform


def _NameOnly(value):
  """Get only the last token from a longer path, usually the name.

  Intended to be a selector transform for URLs.

  Args:
    value: str, The value whose last token will be returned.

  Returns:
    str, The name from value.
  """
  if value:
    return value.split('/')[-1]
  return value


def _CommaList(default=None):
  def Transform(items):
    if not items:
      return default
    return ', '.join(items)
  return Transform


def _DiskSize(value):
  """Returns a human readable string representation of the disk size.

  Args:
    value: str, Disk size represented as number of bytes.

  Returns:
    A human readable string representation of the disk size.
  """
  size = float(value)
  the_unit = 'TB'
  for unit in ['bytes', 'KB', 'MB', 'GB']:
    if size < 1024.0:
      the_unit = unit
      break
    size = float(size) / 1024.0
  if size == int(size):
    return '%d %s' % (size, the_unit)
  else:
    return '%3.1f %s' % (size, the_unit)




def _SelectTime(path):
  return _Select(path, transform=lambda x: x and x.isoformat())


COLLECTION_COLUMNS = {
    # APPENGINE
    'app.module_versions': (
        ('MODULE', _Select('module')),
        ('VERSION', _Select('version')),
        ('IS_DEFAULT', _Select('is_default',
                               transform=lambda x: '*' if x else '-')),
    ),

    # BIGQUERY
    'bigquery.datasets': (
        ('DATASET_ID', _Select('datasetReference.datasetId')),
    ),

    # COMPUTE
    'compute.instances': (
        ('NAME', _Select('name')),
        ('ZONE', _Select('zone', _NameOnly)),
        ('MACHINE_TYPE', _Select('machineType', _NameOnly)),
        ('INTERNAL_IP', _Select('networkInterfaces[0].networkIP')),
        ('EXTERNAL_IP', _Select('networkInterfaces[0].accessConfigs[0].natIP')),
        ('STATUS', _Select('status')),
    ),

    # SQL
    'sql.backupRuns': (
        ('DUE_TIME', _SelectTime('dueTime')),
        ('ERROR', _Select('error.code')),
        ('STATUS', _Select('status')),
    ),
    'sql.flags': (
        ('NAME', _Select('name')),
        ('TYPE', _Select('type')),
        ('ALLOWED_VALUES', _Select('allowedStringValues', _CommaList(''))),
    ),
    'sql.instances': (
        ('NAME', _Select('instance')),
        ('REGION', _Select('region')),
        ('TIER', _Select('settings.tier')),
        ('ADDRESS', _Select('ipAddresses[0].ipAddress')),
        ('STATUS', _Select('state')),
    ),
    'sql.operations': (
        ('OPERATION', _Select('operation')),
        ('TYPE', _Select('operationType')),
        ('START', _SelectTime('startTime')),
        ('END', _SelectTime('endTime')),
        ('ERROR', _Select('error[0].code')),
        ('STATUS', _Select('state')),
    ),
    'sql.sslCerts': (
        ('NAME', _Select('commonName')),
        ('SHA1_FINGERPRINT', _Select('sha1Fingerprint')),
        ('EXPIRATION', _Select('expirationTime')),
    ),
    'sql.tiers': (
        ('TIER', _Select('tier')),
        ('AVAILABLE_REGIONS', _Select('region', _CommaList(''))),
        ('RAM', _Select('RAM', _DiskSize)),
        ('DISK', _Select('DiskQuota', _DiskSize)),
    ),

    # projects
    'developerprojects.projects': (
        ('PROJECT_ID', _Select('projectId')),
        ('TITLE', _Select('title')),
        ('PROJECT_NUMBER', _Select('projectNumber')),
    ),

    # Managed Instance Groups
    'replicapoolupdater.updates': (
        ('HANDLE', _Select('handle')),
        ('TEMPLATE_NAME', _Select('instanceTemplate', _NameOnly)),
        ('STATE', _Select('state')),
        ('DETAILS', _Select('details')),
    ),
}
