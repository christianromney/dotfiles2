"""Generated message classes for replicapoolupdater version v1beta1.

The Google Compute Engine Instance Group Updater API provides services for
updating groups of Compute Engine Instances.
"""

from protorpc import messages


package = 'replicapoolupdater'


class InsertResponse(messages.Message):
  """Response returned by Insert method.

  Fields:
    updateHandle: Unique (in the context of a group) handle of an update.
  """

  updateHandle = messages.StringField(1)


class ReplicapoolupdaterUpdatesCancelRequest(messages.Message):
  """A ReplicapoolupdaterUpdatesCancelRequest object.

  Fields:
    instanceGroupManager: Name of the instance group manager for this request.
    project: Project ID for this request.
    updateHandle: Unique (in the context of a group) handle of an update.
    zone: Zone for the instance group manager.
  """

  instanceGroupManager = messages.StringField(1, required=True)
  project = messages.StringField(2, required=True)
  updateHandle = messages.StringField(3, required=True)
  zone = messages.StringField(4, required=True)


class ReplicapoolupdaterUpdatesCancelResponse(messages.Message):
  """An empty ReplicapoolupdaterUpdatesCancel response."""


class ReplicapoolupdaterUpdatesGetRequest(messages.Message):
  """A ReplicapoolupdaterUpdatesGetRequest object.

  Fields:
    instanceGroupManager: Name of the instance group manager for this request.
    project: Project ID for this request.
    updateHandle: Unique (in the context of a group) handle of an update.
    zone: Zone for the instance group manager.
  """

  instanceGroupManager = messages.StringField(1, required=True)
  project = messages.StringField(2, required=True)
  updateHandle = messages.StringField(3, required=True)
  zone = messages.StringField(4, required=True)


class ReplicapoolupdaterUpdatesInsertRequest(messages.Message):
  """A ReplicapoolupdaterUpdatesInsertRequest object.

  Fields:
    instanceGroupManager: Name of the instance group manager for this request.
    project: Project ID for this request.
    update: A Update resource to be passed as the request body.
    zone: Zone for the instance group manager.
  """

  instanceGroupManager = messages.StringField(1, required=True)
  project = messages.StringField(2, required=True)
  update = messages.MessageField('Update', 3)
  zone = messages.StringField(4, required=True)


class ReplicapoolupdaterUpdatesListRequest(messages.Message):
  """A ReplicapoolupdaterUpdatesListRequest object.

  Fields:
    instanceGroupManager: Name of the instance group manager for this request.
    maxResults: Maximum count of results to be returned. Acceptable values are
      1 to 100, inclusive. (Default: 50)
    pageToken: Set this to the nextPageToken value returned by a previous list
      request to obtain the next page of results from the previous list
      request.
    project: Project ID for this request.
    zone: Zone for the instance group manager.
  """

  instanceGroupManager = messages.StringField(1, required=True)
  maxResults = messages.IntegerField(2, variant=messages.Variant.INT32, default=50)
  pageToken = messages.StringField(3)
  project = messages.StringField(4, required=True)
  zone = messages.StringField(5, required=True)


class ReplicapoolupdaterUpdatesPauseRequest(messages.Message):
  """A ReplicapoolupdaterUpdatesPauseRequest object.

  Fields:
    instanceGroupManager: Name of the instance group manager for this request.
    project: Project ID for this request.
    updateHandle: Unique (in the context of a group) handle of an update.
    zone: Zone for the instance group manager.
  """

  instanceGroupManager = messages.StringField(1, required=True)
  project = messages.StringField(2, required=True)
  updateHandle = messages.StringField(3, required=True)
  zone = messages.StringField(4, required=True)


class ReplicapoolupdaterUpdatesPauseResponse(messages.Message):
  """An empty ReplicapoolupdaterUpdatesPause response."""


class ReplicapoolupdaterUpdatesRollbackRequest(messages.Message):
  """A ReplicapoolupdaterUpdatesRollbackRequest object.

  Fields:
    instanceGroupManager: Name of the instance group manager for this request.
    project: Project ID for this request.
    updateHandle: Unique (in the context of a group) handle of an update.
    zone: Zone for the instance group manager.
  """

  instanceGroupManager = messages.StringField(1, required=True)
  project = messages.StringField(2, required=True)
  updateHandle = messages.StringField(3, required=True)
  zone = messages.StringField(4, required=True)


class ReplicapoolupdaterUpdatesRollbackResponse(messages.Message):
  """An empty ReplicapoolupdaterUpdatesRollback response."""


class ReplicapoolupdaterUpdatesRollforwardRequest(messages.Message):
  """A ReplicapoolupdaterUpdatesRollforwardRequest object.

  Fields:
    instanceGroupManager: Name of the instance group manager for this request.
    project: Project ID for this request.
    updateHandle: Unique (in the context of a group) handle of an update.
    zone: Zone for the instance group manager.
  """

  instanceGroupManager = messages.StringField(1, required=True)
  project = messages.StringField(2, required=True)
  updateHandle = messages.StringField(3, required=True)
  zone = messages.StringField(4, required=True)


class ReplicapoolupdaterUpdatesRollforwardResponse(messages.Message):
  """An empty ReplicapoolupdaterUpdatesRollforward response."""


class StandardQueryParameters(messages.Message):
  """Query parameters accepted by all methods.

  Enums:
    AltValueValuesEnum: Data format for the response.

  Fields:
    alt: Data format for the response.
    fields: Selector specifying which fields to include in a partial response.
    key: API key. Your API key identifies your project and provides you with
      API access, quota, and reports. Required unless you provide an OAuth 2.0
      token.
    oauth_token: OAuth 2.0 token for the current user.
    prettyPrint: Returns response with indentations and line breaks.
    quotaUser: Available to use for quota purposes for server-side
      applications. Can be any arbitrary string assigned to a user, but should
      not exceed 40 characters. Overrides userIp if both are provided.
    trace: A tracing token of the form "token:<tokenid>" or "email:<ldap>" to
      include in api requests.
    userIp: IP address of the site where the request originates. Use this if
      you want to enforce per-user limits.
  """

  class AltValueValuesEnum(messages.Enum):
    """Data format for the response.

    Values:
      json: Responses with Content-Type of application/json
    """
    json = 0

  alt = messages.EnumField('AltValueValuesEnum', 1, default=u'json')
  fields = messages.StringField(2)
  key = messages.StringField(3)
  oauth_token = messages.StringField(4)
  prettyPrint = messages.BooleanField(5, default=True)
  quotaUser = messages.StringField(6)
  trace = messages.StringField(7)
  userIp = messages.StringField(8)


class Update(messages.Message):
  """Resource describing a single update (rollout) of an instance group to the
  given template.

  Fields:
    details: [Output Only] Human-readable description of an update progress.
    handle: [Output Only] Unique (in the context of a group) handle assigned
      to this update.
    instanceTemplate: Url of an instance template to be applied.
    kind: [Output only] The resource type. Always replicapoolupdater#update.
    policy: Parameters of an update process.
    selfLink: [Output only] The fully qualified URL for this resource.
    state: [Output Only] Current state of an update.
    targetState: [Output Only] Requested state of an update. This is the state
      that the updater is moving towards. Acceptable values are:   -
      "ROLLED_OUT": The user has requested the update to go forward.  -
      "ROLLED_BACK": The user has requested the update to be rolled back.  -
      "PAUSED": The user has requested the update to be paused.   -
      "CANCELLED": The user has requested the update to be cancelled. The
      updater service is in the process of canceling the update.
  """

  details = messages.StringField(1)
  handle = messages.StringField(2)
  instanceTemplate = messages.StringField(3)
  kind = messages.StringField(4, default=u'replicapoolupdater#update')
  policy = messages.MessageField('UpdatePolicy', 5)
  selfLink = messages.StringField(6)
  state = messages.StringField(7)
  targetState = messages.StringField(8)


class UpdateList(messages.Message):
  """Response returned by List method.

  Fields:
    items: Collection of requested updates.
    nextPageToken: A token used to continue a truncated list request.
  """

  items = messages.MessageField('Update', 1, repeated=True)
  nextPageToken = messages.StringField(2)


class UpdatePolicy(messages.Message):
  """Parameters of an update process.

  Fields:
    canary: Parameters of a canary phase. If absent, canary will NOT be
      performed.
    maxNumConcurrentInstances: Maximum number of instances that can be updated
      simultaneously (concurrently). An update of an instance starts when the
      instance is about to be restarted and finishes after the instance has
      been restarted and the sleep period (defined by
      sleep_after_instance_restart_sec) has passed.
    sleepAfterInstanceRestartSec: Time period after the instance has been
      restarted but before marking the update of this instance as done.
  """

  canary = messages.MessageField('UpdatePolicyCanary', 1)
  maxNumConcurrentInstances = messages.IntegerField(2, variant=messages.Variant.INT32)
  sleepAfterInstanceRestartSec = messages.IntegerField(3, variant=messages.Variant.INT32)


class UpdatePolicyCanary(messages.Message):
  """Parameters of a canary phase.

  Fields:
    numInstances: Number of instances updated as a part of canary phase. If
      absent, the default number of instances will be used.
  """

  numInstances = messages.IntegerField(1, variant=messages.Variant.INT32)


