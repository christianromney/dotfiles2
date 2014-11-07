"""Generated message classes for container version v1beta1.

The Google Container Engine API is used for building and managing container
based applications, powered by the open source Kubernetes technology.
"""

from protorpc import messages


package = 'container'


class Cluster(messages.Message):
  """A Cluster object.

  Enums:
    StatusValueValuesEnum:

  Fields:
    clusterApiVersion: A string attribute.
    containerIpv4Cidr: container/services_ipv4_cidr and node_routing_prefix
      are currently output only, but are expected to eventually become
      configurable.
    creationTimestamp: A string attribute.
    description: A string attribute.
    endpoint: A string attribute.
    masterAuth: A MasterAuth attribute.
    name: A string attribute.
    nodeConfig: A NodeConfig attribute.
    nodeRoutingPrefixSize: A integer attribute.
    numNodes: A integer attribute.
    servicesIpv4Cidr: A string attribute.
    status: A StatusValueValuesEnum attribute.
    statusMessage: A string attribute.
    zone: The fields below are output only
  """

  class StatusValueValuesEnum(messages.Enum):
    """StatusValueValuesEnum enum type.

    Values:
      error: <no description>
      provisioning: <no description>
      running: <no description>
      stopping: <no description>
    """
    error = 0
    provisioning = 1
    running = 2
    stopping = 3

  clusterApiVersion = messages.StringField(1)
  containerIpv4Cidr = messages.StringField(2)
  creationTimestamp = messages.StringField(3)
  description = messages.StringField(4)
  endpoint = messages.StringField(5)
  masterAuth = messages.MessageField('MasterAuth', 6)
  name = messages.StringField(7)
  nodeConfig = messages.MessageField('NodeConfig', 8)
  nodeRoutingPrefixSize = messages.IntegerField(9, variant=messages.Variant.INT32)
  numNodes = messages.IntegerField(10, variant=messages.Variant.INT32)
  servicesIpv4Cidr = messages.StringField(11)
  status = messages.EnumField('StatusValueValuesEnum', 12)
  statusMessage = messages.StringField(13)
  zone = messages.StringField(14)


class ContainerProjectsClustersListRequest(messages.Message):
  """A ContainerProjectsClustersListRequest object.

  Fields:
    projectId: A string attribute.
  """

  projectId = messages.StringField(1, required=True)


class ContainerProjectsOperationsListRequest(messages.Message):
  """A ContainerProjectsOperationsListRequest object.

  Fields:
    projectId: A string attribute.
  """

  projectId = messages.StringField(1, required=True)


class ContainerProjectsZonesClustersCreateRequest(messages.Message):
  """A ContainerProjectsZonesClustersCreateRequest object.

  Fields:
    createClusterRequest: A CreateClusterRequest resource to be passed as the
      request body.
    projectId: A string attribute.
    zoneId: A string attribute.
  """

  createClusterRequest = messages.MessageField('CreateClusterRequest', 1)
  projectId = messages.StringField(2, required=True)
  zoneId = messages.StringField(3, required=True)


class ContainerProjectsZonesClustersDeleteRequest(messages.Message):
  """A ContainerProjectsZonesClustersDeleteRequest object.

  Fields:
    clusterId: A string attribute.
    projectId: A string attribute.
    zoneId: A string attribute.
  """

  clusterId = messages.StringField(1, required=True)
  projectId = messages.StringField(2, required=True)
  zoneId = messages.StringField(3, required=True)


class ContainerProjectsZonesClustersGetRequest(messages.Message):
  """A ContainerProjectsZonesClustersGetRequest object.

  Fields:
    clusterId: A string attribute.
    projectId: A string attribute.
    zoneId: A string attribute.
  """

  clusterId = messages.StringField(1, required=True)
  projectId = messages.StringField(2, required=True)
  zoneId = messages.StringField(3, required=True)


class ContainerProjectsZonesClustersListRequest(messages.Message):
  """A ContainerProjectsZonesClustersListRequest object.

  Fields:
    projectId: A string attribute.
    zoneId: A string attribute.
  """

  projectId = messages.StringField(1, required=True)
  zoneId = messages.StringField(2, required=True)


class ContainerProjectsZonesOperationsGetRequest(messages.Message):
  """A ContainerProjectsZonesOperationsGetRequest object.

  Fields:
    operationId: A string attribute.
    projectId: A string attribute.
    zoneId: A string attribute.
  """

  operationId = messages.StringField(1, required=True)
  projectId = messages.StringField(2, required=True)
  zoneId = messages.StringField(3, required=True)


class ContainerProjectsZonesOperationsListRequest(messages.Message):
  """A ContainerProjectsZonesOperationsListRequest object.

  Fields:
    projectId: A string attribute.
    zoneId: A string attribute.
  """

  projectId = messages.StringField(1, required=True)
  zoneId = messages.StringField(2, required=True)


class CreateClusterRequest(messages.Message):
  """A CreateClusterRequest object.

  Fields:
    cluster: A Cluster attribute.
  """

  cluster = messages.MessageField('Cluster', 1)


class ListAggregatedClustersResponse(messages.Message):
  """A ListAggregatedClustersResponse object.

  Fields:
    clusters: A Cluster attribute.
  """

  clusters = messages.MessageField('Cluster', 1, repeated=True)


class ListAggregatedOperationsResponse(messages.Message):
  """A ListAggregatedOperationsResponse object.

  Fields:
    operations: A Operation attribute.
  """

  operations = messages.MessageField('Operation', 1, repeated=True)


class ListClustersResponse(messages.Message):
  """A ListClustersResponse object.

  Fields:
    clusters: A Cluster attribute.
  """

  clusters = messages.MessageField('Cluster', 1, repeated=True)


class ListOperationsResponse(messages.Message):
  """A ListOperationsResponse object.

  Fields:
    operations: A Operation attribute.
  """

  operations = messages.MessageField('Operation', 1, repeated=True)


class MasterAuth(messages.Message):
  """A MasterAuth object.

  Fields:
    password: A string attribute.
    user: A string attribute.
  """

  password = messages.StringField(1)
  user = messages.StringField(2)


class NodeConfig(messages.Message):
  """A NodeConfig object.

  Fields:
    machineType: Name of a valid GCE instance type (e.g. 'n1-standard-1')
    sourceImage: Name of a valid GCE image. Note that this must be the fully
      specified name, not a shortened alias. In other words a source_image
      like 'projects/debian-cloud/global/images/debian-7-wheezy-vYYYYMMDD'
      will work, but 'debian-7-wheezy' or 'debian-7-wheezy-vYYYYMMDD' won't.
  """

  machineType = messages.StringField(1)
  sourceImage = messages.StringField(2)


class Operation(messages.Message):
  """Defines the operation resource.

  Enums:
    OperationTypeValueValuesEnum: The type of the operation, indicating what
      user action it corresponds to.
    StatusValueValuesEnum: Current status of the operation. Can be one of the
      following: "PENDING", "RUNNING", or "DONE".

  Fields:
    errorMessage: A textual description of an error that has occurred. Empty
      if no error has occurred.
    name: The server-assigned resource name for this operation. If the
      operation is fulfilled upfront, it may not have a resource name.
    operationType: The type of the operation, indicating what user action it
      corresponds to.
    status: Current status of the operation. Can be one of the following:
      "PENDING", "RUNNING", or "DONE".
    target: The URL of the resource or collection that this operation is
      directly associated with, it is optional.
    zone: The compute zone (e.g. us-central1-a) on which the operation is
      being done.
  """

  class OperationTypeValueValuesEnum(messages.Enum):
    """The type of the operation, indicating what user action it corresponds
    to.

    Values:
      createCluster: <no description>
      deleteCluster: <no description>
    """
    createCluster = 0
    deleteCluster = 1

  class StatusValueValuesEnum(messages.Enum):
    """Current status of the operation. Can be one of the following:
    "PENDING", "RUNNING", or "DONE".

    Values:
      done: <no description>
      pending: <no description>
      running: <no description>
    """
    done = 0
    pending = 1
    running = 2

  errorMessage = messages.StringField(1)
  name = messages.StringField(2)
  operationType = messages.EnumField('OperationTypeValueValuesEnum', 3)
  status = messages.EnumField('StatusValueValuesEnum', 4)
  target = messages.StringField(5)
  zone = messages.StringField(6)


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


