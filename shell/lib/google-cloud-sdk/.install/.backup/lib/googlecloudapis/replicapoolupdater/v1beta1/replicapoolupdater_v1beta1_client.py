"""Generated client library for replicapoolupdater version v1beta1."""

from googlecloudapis.apitools.base.py import base_api
from googlecloudapis.replicapoolupdater.v1beta1 import replicapoolupdater_v1beta1_messages as messages


class ReplicapoolupdaterV1beta1(base_api.BaseApiClient):
  """Generated client library for service replicapoolupdater version v1beta1."""

  MESSAGES_MODULE = messages

  _PACKAGE = u'replicapoolupdater'
  _SCOPES = [u'https://www.googleapis.com/auth/cloud-platform', u'https://www.googleapis.com/auth/replicapool', u'https://www.googleapis.com/auth/replicapool.readonly']
  _VERSION = u'v1beta1'
  _CLIENT_ID = ''
  _CLIENT_SECRET = ''
  _USER_AGENT = ''
  _CLIENT_CLASS_NAME = u'ReplicapoolupdaterV1beta1'
  _URL_VERSION = u'v1beta1'

  def __init__(self, url='', credentials=None,
               get_credentials=True, http=None, model=None,
               log_request=False, log_response=False,
               credentials_args=None, default_global_params=None,
               additional_http_headers=None):
    """Create a new replicapoolupdater handle."""
    url = url or u'https://www.googleapis.com/replicapoolupdater/v1beta1/'
    super(ReplicapoolupdaterV1beta1, self).__init__(
        url, credentials=credentials,
        get_credentials=get_credentials, http=http, model=model,
        log_request=log_request, log_response=log_response,
        credentials_args=credentials_args,
        default_global_params=default_global_params,
        additional_http_headers=additional_http_headers)
    self.updates = self.UpdatesService(self)

  class UpdatesService(base_api.BaseApiService):
    """Service class for the updates resource."""

    _NAME = u'updates'

    def __init__(self, client):
      super(ReplicapoolupdaterV1beta1.UpdatesService, self).__init__(client)
      self._method_configs = {
          'Cancel': base_api.ApiMethodInfo(
              http_method=u'POST',
              method_id=u'replicapoolupdater.updates.cancel',
              ordered_params=[u'project', u'zone', u'instanceGroupManager', u'updateHandle'],
              path_params=[u'instanceGroupManager', u'project', u'updateHandle', u'zone'],
              query_params=[],
              relative_path=u'projects/{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}/updates/{updateHandle}/cancel',
              request_field='',
              request_type_name=u'ReplicapoolupdaterUpdatesCancelRequest',
              response_type_name=u'ReplicapoolupdaterUpdatesCancelResponse',
              supports_download=False,
          ),
          'Get': base_api.ApiMethodInfo(
              http_method=u'GET',
              method_id=u'replicapoolupdater.updates.get',
              ordered_params=[u'project', u'zone', u'instanceGroupManager', u'updateHandle'],
              path_params=[u'instanceGroupManager', u'project', u'updateHandle', u'zone'],
              query_params=[],
              relative_path=u'projects/{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}/updates/{updateHandle}',
              request_field='',
              request_type_name=u'ReplicapoolupdaterUpdatesGetRequest',
              response_type_name=u'Update',
              supports_download=False,
          ),
          'Insert': base_api.ApiMethodInfo(
              http_method=u'POST',
              method_id=u'replicapoolupdater.updates.insert',
              ordered_params=[u'project', u'zone', u'instanceGroupManager'],
              path_params=[u'instanceGroupManager', u'project', u'zone'],
              query_params=[],
              relative_path=u'projects/{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}/updates',
              request_field=u'update',
              request_type_name=u'ReplicapoolupdaterUpdatesInsertRequest',
              response_type_name=u'InsertResponse',
              supports_download=False,
          ),
          'List': base_api.ApiMethodInfo(
              http_method=u'GET',
              method_id=u'replicapoolupdater.updates.list',
              ordered_params=[u'project', u'zone', u'instanceGroupManager'],
              path_params=[u'instanceGroupManager', u'project', u'zone'],
              query_params=[u'maxResults', u'pageToken'],
              relative_path=u'projects/{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}/updates',
              request_field='',
              request_type_name=u'ReplicapoolupdaterUpdatesListRequest',
              response_type_name=u'UpdateList',
              supports_download=False,
          ),
          'Pause': base_api.ApiMethodInfo(
              http_method=u'POST',
              method_id=u'replicapoolupdater.updates.pause',
              ordered_params=[u'project', u'zone', u'instanceGroupManager', u'updateHandle'],
              path_params=[u'instanceGroupManager', u'project', u'updateHandle', u'zone'],
              query_params=[],
              relative_path=u'projects/{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}/updates/{updateHandle}/pause',
              request_field='',
              request_type_name=u'ReplicapoolupdaterUpdatesPauseRequest',
              response_type_name=u'ReplicapoolupdaterUpdatesPauseResponse',
              supports_download=False,
          ),
          'Rollback': base_api.ApiMethodInfo(
              http_method=u'POST',
              method_id=u'replicapoolupdater.updates.rollback',
              ordered_params=[u'project', u'zone', u'instanceGroupManager', u'updateHandle'],
              path_params=[u'instanceGroupManager', u'project', u'updateHandle', u'zone'],
              query_params=[],
              relative_path=u'projects/{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}/updates/{updateHandle}/rollback',
              request_field='',
              request_type_name=u'ReplicapoolupdaterUpdatesRollbackRequest',
              response_type_name=u'ReplicapoolupdaterUpdatesRollbackResponse',
              supports_download=False,
          ),
          'Rollforward': base_api.ApiMethodInfo(
              http_method=u'POST',
              method_id=u'replicapoolupdater.updates.rollforward',
              ordered_params=[u'project', u'zone', u'instanceGroupManager', u'updateHandle'],
              path_params=[u'instanceGroupManager', u'project', u'updateHandle', u'zone'],
              query_params=[],
              relative_path=u'projects/{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}/updates/{updateHandle}/rollforward',
              request_field='',
              request_type_name=u'ReplicapoolupdaterUpdatesRollforwardRequest',
              response_type_name=u'ReplicapoolupdaterUpdatesRollforwardResponse',
              supports_download=False,
          ),
          }

      self._upload_configs = {
          }

    def Cancel(self, request, global_params=None):
      """Called on the particular Update endpoint. Cancels the update in state PAUSED. No-op if invoked in state CANCELLED.

      Args:
        request: (ReplicapoolupdaterUpdatesCancelRequest) input message
        global_params: (StandardQueryParameters, default: None) global arguments
      Returns:
        (ReplicapoolupdaterUpdatesCancelResponse) The response message.
      """
      config = self.GetMethodConfig('Cancel')
      return self._RunMethod(
          config, request, global_params=global_params)

    def Get(self, request, global_params=None):
      """Called on the particular Update endpoint. Returns the Update resource.

      Args:
        request: (ReplicapoolupdaterUpdatesGetRequest) input message
        global_params: (StandardQueryParameters, default: None) global arguments
      Returns:
        (Update) The response message.
      """
      config = self.GetMethodConfig('Get')
      return self._RunMethod(
          config, request, global_params=global_params)

    def Insert(self, request, global_params=None):
      """Called on the collection endpoint. Inserts the new Update resource and starts the update.

      Args:
        request: (ReplicapoolupdaterUpdatesInsertRequest) input message
        global_params: (StandardQueryParameters, default: None) global arguments
      Returns:
        (InsertResponse) The response message.
      """
      config = self.GetMethodConfig('Insert')
      return self._RunMethod(
          config, request, global_params=global_params)

    def List(self, request, global_params=None):
      """Called on the collection endpoint. Lists updates for a given instance group, in reverse chronological order. Pagination is supported, see ListRequestHeader.

      Args:
        request: (ReplicapoolupdaterUpdatesListRequest) input message
        global_params: (StandardQueryParameters, default: None) global arguments
      Returns:
        (UpdateList) The response message.
      """
      config = self.GetMethodConfig('List')
      return self._RunMethod(
          config, request, global_params=global_params)

    def Pause(self, request, global_params=None):
      """Called on the particular Update endpoint. Pauses the update in state from { ROLLING_FORWARD, ROLLING_BACK, PAUSED }. No-op if invoked in state PAUSED.

      Args:
        request: (ReplicapoolupdaterUpdatesPauseRequest) input message
        global_params: (StandardQueryParameters, default: None) global arguments
      Returns:
        (ReplicapoolupdaterUpdatesPauseResponse) The response message.
      """
      config = self.GetMethodConfig('Pause')
      return self._RunMethod(
          config, request, global_params=global_params)

    def Rollback(self, request, global_params=None):
      """Called on the particular Update endpoint. Rolls back the update in state from { ROLLING_FORWARD, ROLLING_BACK, PAUSED }. No-op if invoked in state ROLLED_BACK.

      Args:
        request: (ReplicapoolupdaterUpdatesRollbackRequest) input message
        global_params: (StandardQueryParameters, default: None) global arguments
      Returns:
        (ReplicapoolupdaterUpdatesRollbackResponse) The response message.
      """
      config = self.GetMethodConfig('Rollback')
      return self._RunMethod(
          config, request, global_params=global_params)

    def Rollforward(self, request, global_params=None):
      """Called on the particular Update endpoint. Rolls forward the update in state from { ROLLING_FORWARD, ROLLING_BACK, PAUSED }. No-op if invoked in state ROLLED_OUT.

      Args:
        request: (ReplicapoolupdaterUpdatesRollforwardRequest) input message
        global_params: (StandardQueryParameters, default: None) global arguments
      Returns:
        (ReplicapoolupdaterUpdatesRollforwardResponse) The response message.
      """
      config = self.GetMethodConfig('Rollforward')
      return self._RunMethod(
          config, request, global_params=global_params)
