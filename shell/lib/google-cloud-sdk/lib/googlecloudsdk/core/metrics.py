# Copyright 2013 Google Inc. All Rights Reserved.

"""Used to collect anonymous SDK usage information."""

import atexit
import collections
import hashlib
import mutex
import os
import Queue
import socket
import sys
import threading
import urllib
import uuid

import httplib2

from googlecloudsdk.core import config
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.core.util import platforms


_ENDPOINT = 'https://ssl.google-analytics.com/collect'
_TID = 'UA-36037335-2'


_Event = collections.namedtuple('Event',
                                ['category', 'action', 'label', 'value'])


class _MetricsWorker(object):
  """A class to process usage events."""

  DONE = 'DONE'

  @staticmethod
  def StartMetrics():
    """Starts the thread for handling events.

    Returns:
      The running MetricsWorker or None if initialization failed.
    """
    disabled = properties.VALUES.core.disable_usage_reporting.GetBool()
    if disabled is None:
      # If there is no preference set, fall back to the installation default.
      disabled = config.INSTALLATION_CONFIG.disable_usage_reporting
    if disabled:
      log.debug('Metrics are disabled.')
      return None

    try:
      return _MetricsWorker()
    # pylint: disable=bare-except, We never want to fail because of metrics.
    # Worst case scenario, they are just not sent.
    except:
      # If any part of this fails, just don't do any reporting
      log.debug('Metrics failed to start: %s', sys.exc_info())
      return None

  def __init__(self):
    """Initialize a new MetricsWorker.

    This should only be invoked through the static _StartMetics() function which
    will do the appropriate error handling.
    """
    user_agent = 'CloudSDK/{version} {fragment}'.format(
        version=config.CLOUD_SDK_VERSION,
        fragment=platforms.Platform.Current().UserAgentFragment())
    self.__headers = {
        'User-Agent': user_agent,
    }
    self.__project_ids = {}

    hostname = socket.getfqdn()
    install_type = 'Google' if hostname.endswith('.google.com') else 'External'
    self.__params = [
        ('v', '1'),
        ('tid', _TID),
        ('cid', _MetricsWorker.__GetCID()),
        ('t', 'event'),
        ('cd1', config.INSTALLATION_CONFIG.release_channel),
        ('cd2', install_type),
    ]

    self.__queue = Queue.Queue()
    self.__thread = self.__Start()
    log.debug('Metrics started...')

  @staticmethod
  def __GetCID():
    """Gets the client id from the config file, or generates a new one.

    Returns:
      str, The hex string of the client id.
    """
    uuid_path = config.Paths().analytics_cid_path
    cid = None
    if os.path.exists(uuid_path):
      with open(uuid_path) as f:
        cid = f.read()
      if cid:
        return cid

    with open(uuid_path, 'w') as f:
      cid = uuid.uuid4().hex
      f.write(cid)  # A random UUID

    return cid

  def __Start(self):
    """Starts the reporting thread.

    Returns:
      The running Thread object.
    """
    t = threading.Thread(target=self.__Run)
    t.daemon = True
    t.start()
    return t

  def __Run(self):
    # Save local references for speed.
    queue = self.__queue
    base_params = self.__params
    headers = self.__headers
    while True:
      event = queue.get()
      try:
        if event == _MetricsWorker.DONE:
          return
        self.__SendEvent(headers, base_params, event)
        log.debug('Sent event: %s', str(event))
      # pylint: disable=broad-except, httplib2 raises all sort of exceptions
      # from different modules.  We never want a failure to report metrics to
      # surface in the terminal so catch everything and log it.
      except Exception as e:
        log.file_only_logger.exception('Failed to send event: %s, %s',
                                       str(event), e)
      finally:
        queue.task_done()

  def __GetProjectIDHash(self):
    """Gets the hash of the current project id.

    Returns:
      str, The hex digest of the current project id or None if the
      project is not set.
    """
    project_id = properties.VALUES.core.project.Get()
    if not project_id:
      return None
    hashed_id = self.__project_ids.get(project_id)
    if not hashed_id:
      checksum = hashlib.sha1()
      checksum.update(project_id)
      hashed_id = checksum.hexdigest()
      self.__project_ids[project_id] = hashed_id
    return hashed_id

  def __SendEvent(self, headers, base_params, event):
    """Sends the given event to analytics.

    Args:
      headers: {name, value}, The HTTP headers to use for this request.
      base_params: [(name, value)], The analytics parameters to use for this
        event.
      event: Event, The event to send
    """
    h = httplib2.Http()
    params = [
        ('ec', event.category),
        ('ea', event.action),
        ('el', event.label),
        ('ev', event.value),
    ]
    project_id_hash = self.__GetProjectIDHash()
    if project_id_hash:
      params.append(('cd11', project_id_hash))
    params.extend(base_params)

    body = urllib.urlencode(params)
    h.request(_ENDPOINT, method='POST', body=body, headers=headers)

  def ProcessEvent(self, event):
    """Adds the given event to the processing queue.

    Args:
      event: _Event, The event to send.
    """
    self.__queue.put(event)

  def Shutdown(self):
    """Shutdown the metrics thread."""
    self.__queue.put(_MetricsWorker.DONE)
    # An arbitrarily short time to wait.  Hopefully this will be enough to allow
    # the thread to get some execution time and finish.  If not, it is a daemon
    # thread so it will just be killed when we exit, and maybe the metrics
    # will not be sent.
    self.__thread.join(.5)


_metrics_worker = None
_mutex_lock = mutex.mutex()
_metrics_worker_started = False


@atexit.register
def Shutdown():
  """Shuts down the reporting thread.

  The thread will be restarted if you record new events.
  """
  def _Shutdown(unused_none):
    global _metrics_worker, _metrics_worker_started
    if _metrics_worker:
      log.debug('Shutting down metrics...')
      _metrics_worker.Shutdown()
      _metrics_worker = None
      _metrics_worker_started = False
  _mutex_lock.lock(function=_Shutdown, argument=None)
  _mutex_lock.unlock()


def _ProcessEvent(category, action, label, value=0):
  def _CreateWorker(unused_none):
    global _metrics_worker, _metrics_worker_started
    if not _metrics_worker_started:
      _metrics_worker = _MetricsWorker.StartMetrics()
      _metrics_worker_started = True
  _mutex_lock.lock(function=_CreateWorker, argument=None)
  _mutex_lock.unlock()
  if _metrics_worker:
    _metrics_worker.ProcessEvent(
        _Event(category=category, action=action, label=label, value=value))


def Installs(component_id, version_string):
  """Logs that an SDK component was installed.

  Args:
    component_id: str, The component id that was installed.
    version_string: str, The version of the component.
  """
  _ProcessEvent('Installs', component_id, version_string)


def Commands(command_path, version_string):
  """Logs that an SDK command was run.

  Args:
    command_path: str, The '.' separated name of the calliope command.
    version_string: str, The version of the command.
  """
  if not version_string:
    version_string = 'unknown'
  _ProcessEvent('Commands', command_path, version_string)


def Executions(command_name, version_string):
  """Logs that a top level SDK script was run.

  Args:
    command_name: str, The script name.
    version_string: str, The version of the command.
  """
  if not version_string:
    version_string = 'unknown'
  _ProcessEvent('Executions', command_name, version_string)
