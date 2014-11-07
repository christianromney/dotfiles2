"""Generated message classes for developerprojects version v1.

Developer Projects API.
"""

from protorpc import messages


package = 'developerprojects'


class DeveloperprojectsProjectsCreateRequest(messages.Message):
  """A DeveloperprojectsProjectsCreateRequest object.

  Fields:
    appengineStorageLocation: The storage location for the AppEngine Project
      Valid options are defined in the StorageLocation enum in
      apphosting/client/services/api/admin.proto
    createAppengineProject: If true, an AppEngine project will be created.
    project: A Project resource to be passed as the request body.
  """

  appengineStorageLocation = messages.StringField(1)
  createAppengineProject = messages.BooleanField(2, default=True)
  project = messages.MessageField('Project', 3)


class DeveloperprojectsProjectsDeleteRequest(messages.Message):
  """A DeveloperprojectsProjectsDeleteRequest object.

  Fields:
    projectId: A reference that uniquely identifies the project. This can
      either be the Project.project_id or the Project.project_number. This is
      following the naming PRD at goto/project-naming-prd. The caller must be
      an owner of this project, and billing must already be disabled.
  """

  projectId = messages.StringField(1, required=True)


class DeveloperprojectsProjectsDeleteResponse(messages.Message):
  """An empty DeveloperprojectsProjectsDelete response."""


class DeveloperprojectsProjectsGetRequest(messages.Message):
  """A DeveloperprojectsProjectsGetRequest object.

  Fields:
    projectId: The unique identifier of a project.
  """

  projectId = messages.StringField(1, required=True)


class DeveloperprojectsProjectsListRequest(messages.Message):
  """A DeveloperprojectsProjectsListRequest object.

  Fields:
    maxResults: Maximum number of items to return on a single page.
    pageToken: Pagination token.
    query: A query expression for filtering the results of the request.
  """

  maxResults = messages.IntegerField(1, variant=messages.Variant.INT32)
  pageToken = messages.StringField(2)
  query = messages.StringField(3)


class DeveloperprojectsProjectsUndeleteRequest(messages.Message):
  """A DeveloperprojectsProjectsUndeleteRequest object.

  Fields:
    projectId: A reference that uniquely identifies the project. This can
      either be the Project.project_id or the Project.project_number. This is
      following the naming PRD at goto/project-naming-prd. The caller must be
      an owner of this project.
  """

  projectId = messages.StringField(1, required=True)


class DeveloperprojectsProjectsUndeleteResponse(messages.Message):
  """An empty DeveloperprojectsProjectsUndelete response."""


class ListProjectsResponse(messages.Message):
  """A page of the response received from the ListProjects method.  A
  paginated response where more pages are available will have
  `next_page_token` set. This token can be used in a subsequent request to
  retrieve the next request page.

  Fields:
    nextPageToken: Pagination token.  If the result set is too large to fit in
      a single response, this token will be filled in. It encodes the position
      of the current result cursor. Feeding this value into a new list request
      as 'page_token' parameter gives the next page of the results.  When
      next_page_token is not filled in, there is no next page and the client
      is looking at the last page in the result set.  Pagination tokens have a
      limited lifetime.
    projects: The list of projects that matched the list query, possibly
      paginated.  The resource is partially filled in, based on the
      retrieval_options specified in the `retrieval_options` field of the list
      request.
  """

  nextPageToken = messages.StringField(1)
  projects = messages.MessageField('Project', 2, repeated=True)


class Project(messages.Message):
  """Project message type.

  Enums:
    AbuseStateValueValuesEnum: The project abuse state. Reports whether any
      components related to the project have been flagged. Should be ABUSE_OK
      for a project that is in good standing.  This field is read-only.
    LifecycleStateValueValuesEnum: The project lifecycle state.  This field is
      read-only.

  Fields:
    abuseState: The project abuse state. Reports whether any components
      related to the project have been flagged. Should be ABUSE_OK for a
      project that is in good standing.  This field is read-only.
    appengineName: Name identifying legacy projects. This field should not be
      used for new projects. This field is read-only.
    createdMs: The time at which the project was created in milliseconds since
      the epoch.  This is a read-only field.
    labels: The labels associated with this project.
    lifecycleState: The project lifecycle state.  This field is read-only.
    projectId: The unique, user-assigned id of the project. The id must be
      6?30 lowercase letters, digits, or hyphens. It must start with a letter.
      Trailing hyphens are prohibited.  Example: "tokyo-rain-123" This field
      is read-only.
    projectNumber: The number uniquely identifying the project.  Example:
      415104041262 This field is read-only.
    title: The user-assigned title of the project. This field is optional and
      may remain unset.  Example: "My Project"  This is a read-write field.
  """

  class AbuseStateValueValuesEnum(messages.Enum):
    """The project abuse state. Reports whether any components related to the
    project have been flagged. Should be ABUSE_OK for a project that is in
    good standing.  This field is read-only.

    Values:
      abuseOk: <no description>
      abuseSuspended: <no description>
      abuseThrottled: <no description>
      abuseUnknown: <no description>
      abuseWarned: <no description>
    """
    abuseOk = 0
    abuseSuspended = 1
    abuseThrottled = 2
    abuseUnknown = 3
    abuseWarned = 4

  class LifecycleStateValueValuesEnum(messages.Enum):
    """The project lifecycle state.  This field is read-only.

    Values:
      lifecycleActive: <no description>
      lifecycleDeleteIrreversible: <no description>
      lifecycleDeleteReversible: <no description>
      lifecycleDeleted: <no description>
      lifecycleUnknown: <no description>
    """
    lifecycleActive = 0
    lifecycleDeleteIrreversible = 1
    lifecycleDeleteReversible = 2
    lifecycleDeleted = 3
    lifecycleUnknown = 4

  abuseState = messages.EnumField('AbuseStateValueValuesEnum', 1)
  appengineName = messages.StringField(2)
  createdMs = messages.IntegerField(3)
  labels = messages.MessageField('ProjectLabelsEntry', 4, repeated=True)
  lifecycleState = messages.EnumField('LifecycleStateValueValuesEnum', 5)
  projectId = messages.StringField(6)
  projectNumber = messages.IntegerField(7)
  title = messages.StringField(8)


class ProjectLabelsEntry(messages.Message):
  """A ProjectLabelsEntry object.

  Fields:
    key: A string attribute.
    value: A string attribute.
  """

  key = messages.StringField(1)
  value = messages.StringField(2)


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


