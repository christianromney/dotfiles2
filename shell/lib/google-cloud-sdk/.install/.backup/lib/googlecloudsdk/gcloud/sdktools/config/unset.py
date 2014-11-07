# Copyright 2013 Google Inc. All Rights Reserved.

"""Command to unset properties."""

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions as c_exc
from googlecloudsdk.core import properties


class Unset(base.Command):
  """Erase Google Cloud SDK properties.

  Unset a property to be as if it were never defined in the first place. You
  may optionally use the --scope flag to specify a configuration file to update.
  """

  @staticmethod
  def Args(parser):
    """Adds args for this command."""
    Unset.group_class.SCOPE_FLAG.AddToParser(parser)
    property_arg = parser.add_argument(
        'property',
        help='The property to be unset.')
    property_arg.completer = Unset.group_class.PropertiesCompleter

  @c_exc.RaiseToolExceptionInsteadOf(properties.Error)
  def Run(self, args):
    """Runs this command."""
    prop = self.group.PropertyFromString(args.property)
    properties.PersistProperty(prop, None,
                               scope=properties.Scope.FromId(args.scope))
