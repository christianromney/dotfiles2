# Copyright 2014 Google Inc. All Rights Reserved.

"""Common utilities for the containers tool."""
import atexit
import collections
import json
import os
import subprocess
import tempfile
import time
import httplib2

from googlecloudapis.apitools.base import py as apitools_base
from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.core import config
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.core.util import console_io
from googlecloudsdk.core.util import files as file_utils
from googlecloudsdk.core.util import platforms

from googlecloudsdk.preview.lib import util


class Error(Exception):
  """Class for errors raised by container commands."""


class BadClusterException(Error):
  """BadClusterException is for an unusable cluster."""

  def __init__(self, cluster, message):
    super(BadClusterException, self).__init__(
        'Cluster [{0}] has error {1}'.format(
            cluster, message))


def CheckOperationSucceeded(operation, messages):
  if operation.status != messages.Operation.StatusValueValuesEnum.done:
    raise Error(
        'Operation [{0}] is still running'.format(operation))
  if operation.errorMessage:
    raise Error('Operation [{0}] finished with error: {1}'.format(
        operation, operation.errorMessage))


def CheckClusterIsValid(cluster, messages):
  if cluster.status == messages.Cluster.StatusValueValuesEnum.error:
    raise BadClusterException(cluster, cluster.statusMessage)
  if not cluster.endpoint:
    raise BadClusterException(cluster, 'missing endpoint')
  if not cluster.masterAuth or (not cluster.masterAuth.user or
                                not cluster.masterAuth.password):
    raise BadClusterException(cluster, 'missing masterAuth data')


def WriteTemporaryConfigFile(obj):
  """Write an object out as a temporary file in JSON.

  Args:
    obj: The object to output.

  Returns:
    The name of the temporary file.  This file is deleted at exit using atexit.
  """
  f = tempfile.NamedTemporaryFile(delete=False)
  f.write(json.dumps(obj))
  f.close()
  atexit.register(os.unlink, f.name)
  return f.name


def MakePod(name, image, port):
  """Make a pod API object given some parameters.

  Args:
    name: The name of the pod.
    image: The container image to use.
    port: The port to use for the container.

  Returns:
    A pod object suitable for json serialization.
  """
  pod = {
      'id': name,
      'kind': 'Pod',
      'apiVersion': 'v1beta1',
      'desiredState': {
          'manifest': {
              'version': 'v1beta1',
              'id': name,
              'containers': [{
                  'name': name,
                  'image': image,
                  'ports': [{
                      'containerPort': port,
                      'hostPort': port
                      }]
                  }]
              }
          },
      'labels': {
          'name': name,
          }
      }
  return pod


def WaitForOperation(operation, project_id, zone_id, context, message,
                     timeout=600, poll_period=5):
  """Poll container Operation until its status is done or timeout reached.

  Args:
    operation: Operation message of the operation to be polled.
    project_id: str, project which owns this operation.
    zone_id: str, compute zone for this operation.
    context: dict, container Command context.
    message: str, message to display to user while polling.
    timeout: number, seconds to poll with retries before timing out.
    poll_period: number, delay in seconds between requests.

  Returns:
    Operation, the return value of the last successful operations.get
    request, or the input operation param, if no request succeeded.
  """
  client = context['container-api']
  messages = context['container_messages']

  req = messages.ContainerProjectsZonesOperationsGetRequest(
      operationId=operation.name, projectId=project_id, zoneId=zone_id)
  with console_io.ProgressTracker(message, autotick=True):
    start_time = time.time()
    while timeout > (time.time() - start_time):
      try:
        operation = client.projects_zones_operations.Get(req)
        if operation.status == messages.Operation.StatusValueValuesEnum.done:
          # Success!
          log.info('Operation %s succeeded after %.3f seconds',
                   operation, (time.time() - start_time))
          return operation
      except apitools_base.HttpError as error:
        log.debug('GetOperation failed: %s', error)
        # Keep trying until we timeout in case error is transient.
      time.sleep(poll_period)
  log.err.Print('Timed out waiting for operation %s' % operation)
  return operation


def HealthCheckCluster(endpoint, user, password, timeout=600, poll_period=5):
  """Poll the Kubernetes api until cluster is healthy.

  Args:
    endpoint: str, external IP of the Kubernetes master.
    user: str, basic auth user to authenticate to Kubernetes api.
    password: str, basic auth password to authenticate to Kubernetes api.
    timeout: number, seconds to poll before timing out.
    poll_period: number, delay in seconds between requests.

  Returns:
    True if Kubernetes api reports cluster is healthy.
  """
  http = httplib2.Http(disable_ssl_certificate_validation=True)
  http.add_credentials(user, password)
  http.force_exception_to_status_code = True

  healthcheck_endpoint = 'https://%s/api/v1beta1/validate' % endpoint
  healthy = False
  with console_io.ProgressTracker('Waiting for cluster api initialization',
                                  autotick=True):
    # Poll the api until it responds
    start_time = time.time()
    while timeout > (time.time() - start_time) and not healthy:
      (response, content) = http.request(healthcheck_endpoint)
      healthy = response.status == 200
      if healthy:
        content_obj = json.loads(content)
        for component in content_obj:
          if component.get('health', None) != 'healthy':
            # We can talk to master but some components are unhealthy.
            # It may be a temporary issue, so keep polling until timeout
            # to give the cluster a chance to fix itself.
            healthy = False
            break
        log.info('Cluster api reached healthy after %.3f seconds',
                 (time.time() - start_time))
      else:
        time.sleep(poll_period)
  if not healthy:
    log.err.Print('Timed out waiting for cluster initialization. '
                  'Cluster api may not be available.\n')
  return healthy


def DescribeCluster(cluster_name, zone_id, project_id, context):
  """Describe a running cluster.

  Args:
    cluster_name: str, name of the cluster to describe.
    zone_id: compute zone in which the cluster is running.
    project_id: project in which the cluster is running.
    context: container Command context.
  Returns:
    Cluster message.
  """
  client = context['container-api']
  messages = context['container_messages']
  req = messages.ContainerProjectsZonesClustersGetRequest(
      clusterId=cluster_name, projectId=project_id, zoneId=zone_id)
  try:
    return client.projects_zones_clusters.Get(req)
  except apitools_base.HttpError as error:
    raise exceptions.HttpException(util.GetError(error))

KMASTER_NAME_FORMAT = 'k8s-{cluster_name}-master'
# These are determined by the version of kubernetes the cluster is running.
# This needs kept up to date when validating new cluster api versions.
KMASTER_CERT_DIRECTORY = '/usr/share/nginx'
KMASTER_USER = 'root'  # for /usr/share/...
KMASTER_CLIENT_KEY = 'kubecfg.key'
KMASTER_CLIENT_CERT = 'kubecfg.crt'
KMASTER_CERT_AUTHORITY = 'ca.crt'
KMASTER_CERT_FILES = [KMASTER_CLIENT_KEY, KMASTER_CLIENT_CERT,
                      KMASTER_CERT_AUTHORITY]


def FetchCertFiles(entry_point, cluster_name, zone_id, project_id):
  """Call into gcloud.compute.copy_files to copy certs from cluster.

  Copies cert files from Kubernetes master into local config directory
  for the provided cluster.

  Args:
    entry_point: CommandGroup, top-level group for a Command.
    cluster_name: str, the name of the cluster to fetch cert files from.
    zone_id: str, compute zone in which the cluster is running.
    project_id: str, project that owns this cluster.
  """
  instance_name = KMASTER_NAME_FORMAT.format(cluster_name=cluster_name)

  def _RemoteFilePart(cert_file):
    path = os.path.join(KMASTER_CERT_DIRECTORY, cert_file)
    return'{user}@{instance_name}:{filepath}'.format(
        user=KMASTER_USER, instance_name=instance_name, filepath=path)

  config_dir = ContainerConfig.GetConfigDir(cluster_name, zone_id, project_id)

  copy_files_args = {
      'sources': map(_RemoteFilePart, KMASTER_CERT_FILES),
      'destination': config_dir,
      'zone': zone_id,
  }

  log.out.Print('Using gcloud compute copy-files to fetch ssl certs from '
                'cluster master...')
  entry_point.compute.copy_files(**copy_files_args)


def WhichKubecfg():
  try:
    return subprocess.check_output(['which', 'kubecfg'])
  except subprocess.CalledProcessError:
    return None


class BaseKubecfgCommand(base.Command):
  """Base Command for commands that use kubecfg."""

  @staticmethod
  def Args(parser):
    """Common flags for kubecfg commands."""
    parser.add_argument(
        '--purge-config-cache',
        help='Clear cached config data for the cluster. If set, will call '
        '\'container clusters describe\' directly to get cluster data before '
        'executing kubernetes client command.',
        action='store_true')

  @exceptions.RaiseToolExceptionInsteadOf(Error)
  def Run(self, args):
    """Load and return ContainerConfig prior to calling a kubecfg command.

    Args:
      args: an argparse namespace. All the arguments that were provided to this
        command invocation.

    Returns:
      ContainerConfig for the project,zone,cluster specified by args/properties.

    Raises:
      Error: if the current platform is not supported by kubecfg.
    """
    local = platforms.Platform.Current()
    if local.operating_system == platforms.OperatingSystem.WINDOWS:
      raise Error(
          'This command requires the kubernetes client (kubecfg), which is '
          'not available for Windows at this time.')
    if not WhichKubecfg():
      raise Error(
          'This command requires the kubernetes client (kubecfg), which is '
          'installed with the gcloud preview component. Run '
          '\'gcloud components update preview\', or make sure kubecfg is '
          'installed somewhere on your path.')
    resources = self.context['container_registry']
    zone_id = resources.Parse(args.zone, collection='compute.zones').zone
    project_id = properties.VALUES.core.project.Get(required=True)

    c_config = None
    if not args.purge_config_cache:
      c_config = ContainerConfig.Load(args.cluster_name, zone_id, project_id)

    if not c_config:
      log.out.Print('Fetching cluster endpoint and auth data.')
      # Call DescribeCluster to get auth info and cache for next time
      cluster = DescribeCluster(args.cluster_name, zone_id, project_id,
                                self.context)

      if not HealthCheckCluster(cluster.endpoint,
                                cluster.masterAuth.user,
                                cluster.masterAuth.password):
        log.warn('Timeout reached waiting for cluster healthcheck. The cluster '
                 'may not have finished initializing.')
      c_config = ContainerConfig.Persist(cluster, project_id)
      FetchCertFiles(self.entry_point, cluster.name, zone_id, project_id)

    return c_config

  def CallKubecfg(self, c_config, kubecfg_args):
    """Shell out to call to kubecfg tool.

    Args:
      c_config: ContainerConfig object for cluster.
      kubecfg_args: specific args to call kubecfg with (not including required
          args for host, auth, etc).
    Returns:
      (output, error), where
        output: str, raw output of the kubecfg command.
        error: subprocess.CalledProcessError, if the command exited with
          non-zero status, None if command exited with success.
    """
    base_args = [
        'kubecfg',
        '-h', c_config.https_endpoint,
        '-auth', c_config.auth_file,
    ]
    cert_files = c_config.GetCertFiles()
    if cert_files:
      base_args += [
          '-certificate_authority=%s' % cert_files.certificate_authority,
          '-client_key=%s' % cert_files.client_key,
          '-client_certificate=%s' % cert_files.client_certificate,
      ]
    else:
      log.warn('No certificate files found in %s. Certificate checking '
               'disabled for calls to cluster master.', c_config.config_dir)
      base_args += ['-insecure_skip_tls_verify=true']
    try:
      log.debug('Calling \'%s\'', ' '.join(base_args + kubecfg_args))
      output = subprocess.check_output(base_args + kubecfg_args,
                                       stderr=subprocess.STDOUT)
      return (output, None)
    except subprocess.CalledProcessError as error:
      return (error.output, error)

  def Display(self, args, result):
    """This method is called to print the result of the Run() method.

    Args:
      args: The arguments that command was run with.
      result: The value returned from the Run() method.
    """
    output, error = result
    if error:
      log.debug('kubecfg command %s returned non-zero exit status %d',
                error.cmd, error.returncode)
      log.error(output)
    else:
      log.out.Print(output)


class BaseKubecfgGroup(base.Group):
  """Base Group for commands that use kubecfg."""

  @staticmethod
  def Args(parser):
    """Add arguments to the parser.

    Args:
      parser: argparse.ArgumentParser, This is a standard argparser parser with
        which you can register arguments.  See the public argparse documentation
        for its capabilities.
    """
    parser.add_argument(
        '--cluster-name', '-n',
        required=True,
        help='The name of this cluster.')

  def Filter(self, context, args):
    """Modify the context that will be given to this group's commands when run.

    Args:
      context: {str:object}, A set of key-value pairs that can be used for
          common initialization among commands.
      args: argparse.Namespace: The same namespace given to the corresponding
          .Run() invocation.

    Returns:
      The refined command context.
    """
    return context


CertFiles = collections.namedtuple(
    'CertFiles', 'certificate_authority,client_certificate,client_key')


class ContainerConfig(object):
  """Encapsulates persistent container config data.

  Call ContainerConfig.Load() or ContainerConfig.Persist() to create this
  object.
  """

  _CONFIG_DIR_FORMAT = '{project}.{zone}.{cluster}'

  CONFIG_ROOT = os.path.join(config.Paths().global_config_dir, 'container')

  CONFIG_FILE_NAME = 'cluster.json'

  AUTH_FILE_NAME = 'kubernetes_auth'

  def __init__(self, cluster_name, zone_id, project_id, endpoint):
    self._config_dir = ContainerConfig.GetConfigDir(
        cluster_name, zone_id, project_id)
    self._endpoint = endpoint
    def _Fullpath(filename):
      return os.path.abspath(os.path.join(self._config_dir, filename))

    self._auth_file = _Fullpath(ContainerConfig.AUTH_FILE_NAME)
    self._config_file = _Fullpath(ContainerConfig.CONFIG_FILE_NAME)
    self._cert_files = CertFiles(_Fullpath(KMASTER_CERT_AUTHORITY),
                                 _Fullpath(KMASTER_CLIENT_CERT),
                                 _Fullpath(KMASTER_CLIENT_KEY))

  @property
  def endpoint(self):
    return self._endpoint

  @property
  def https_endpoint(self):
    return 'https://%s' % self.endpoint

  @property
  def auth_file(self):
    return self._auth_file

  @property
  def config_file(self):
    return self._config_file

  @property
  def config_dir(self):
    return self._config_dir

  def SaveAuthFile(self, user, password):
    auth_data = {
        'User': user,
        'Password': password
    }
    with file_utils.Context(
        file_utils.OpenForWritingPrivate(self.auth_file)) as f:
      f.write(json.dumps(auth_data, indent=4))

  def SaveConfigFile(self):
    config_data = {
        'endpoint': self.endpoint,
    }
    with file_utils.Context(
        file_utils.OpenForWritingPrivate(self.config_file)) as f:
      f.write(json.dumps(config_data, indent=4))

  @staticmethod
  def GetConfigDir(cluster_name, zone_id, project_id):
    return os.path.join(
        ContainerConfig.CONFIG_ROOT,
        ContainerConfig._CONFIG_DIR_FORMAT.format(
            project=project_id, zone=zone_id, cluster=cluster_name))

  @classmethod
  def Persist(cls, cluster, project_id):
    """Save config data for the given cluster.

    Persists config file and kubernetes auth file for the given cluster
    to cloud-sdk config directory and returns ContainerConfig object
    encapsulating the same data.

    Args:
      cluster: valid Cluster message to persist config data for.
      project_id: project that owns this cluster.
    Returns:
      ContainerConfig of the persisted data.
    """
    config_dir = cls.GetConfigDir(cluster.name, cluster.zone, project_id)
    log.debug('Saving cluster config to %s', config_dir)

    config.EnsureSDKWriteAccess()
    file_utils.MakeDir(config_dir)

    c_config = cls(cluster.name, cluster.zone, project_id, cluster.endpoint)
    c_config.SaveAuthFile(cluster.masterAuth.user, cluster.masterAuth.password)
    c_config.SaveConfigFile()
    return c_config

  @classmethod
  def Load(cls, cluster_name, zone_id, project_id):
    """Load and verify config for given cluster.

    Args:
      cluster_name: name of cluster to load config for.
      zone_id: compute zone the cluster is running in.
      project_id: project in which the cluster is running.
    Returns:
      ClusterConfig for the cluster, or None if config data is missing or
      incomplete.
    """
    config_dir = cls.GetConfigDir(cluster_name, zone_id, project_id)
    config_file = os.path.join(config_dir, cls.CONFIG_FILE_NAME)

    log.debug('Loading cluster config from %s', config_file)
    c_config = None
    if os.path.isfile(config_file):
      with file_utils.Context(open(config_file)) as f:
        try:
          obj = json.load(f)
          c_config = cls(cluster_name, zone_id, project_id, obj['endpoint'])
        except ValueError:
          pass
    if c_config and c_config.CheckAuthFile():
      return c_config
    else:
      log.debug('Failed to load cluster config data from %s', config_file)

  @classmethod
  def Purge(cls, cluster_name, zone_id, project_id):
    config_dir = cls.GetConfigDir(cluster_name, zone_id, project_id)
    if os.path.exists(config_dir):
      file_utils.RmTree(config_dir)

  def CheckAuthFile(self):
    """Verify kubernetes auth_file has expected format.

    Returns:
      True if auth file exists and has expected format, else false.
    """
    log.debug('Checking kubernetes auth file %s', self.auth_file)
    if os.path.isfile(self.auth_file):
      with file_utils.Context(open(self.auth_file)) as f:
        try:
          obj = json.load(f)
          if obj.get('User') and obj.get('Password'):
            return True
        except ValueError:
          pass
    return False

  def GetCertFiles(self):
    """Get paths of kubernetes certificate files for this cluster.

    Returns:
      list of str, paths to kubernetes certificate files for the cluster,
      or None, if the files do not exist.
    """
    cert_files = self._cert_files
    log.debug('Checking kubernetes cert files %s', cert_files)
    for path in cert_files:
      if not os.path.isfile(path):
        return None
    return cert_files

