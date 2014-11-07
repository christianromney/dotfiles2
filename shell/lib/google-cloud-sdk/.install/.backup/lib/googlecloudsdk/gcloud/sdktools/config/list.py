# Copyright 2013 Google Inc. All Rights Reserved.

"""Command to list properties."""

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions as c_exc
from googlecloudsdk.core import log
from googlecloudsdk.core import properties


class List(base.Command):
  """View Google Cloud SDK properties.

  List all currently available Cloud SDK properties associated with your current
  workspace or global configuration.
  """

  @staticmethod
  def Args(parser):
    """Adds args for this command."""
    parser.add_argument(
        '--all', action='store_true',
        help='List all set and unset properties that match the arguments.')
    property_arg = parser.add_argument(
        'property',
        nargs='?',
        help='The property to be listed.')
    property_arg.completer = List.group_class.PropertiesCompleter

  @c_exc.RaiseToolExceptionInsteadOf(properties.Error)
  def Run(self, args):
    """List available properties."""
    section, prop = self.group.ParsePropertyString(args.property)

    if prop:
      return {section: {
          prop: properties.VALUES.Section(section).Property(prop).Get()}}
    if section:
      return {section: properties.VALUES.Section(section).AllValues(
          list_unset=args.all)}
    return properties.VALUES.AllValues(list_unset=args.all)

  def Display(self, args, result):
    writer = log.out

    for section, props in sorted(result.iteritems()):
      writer.write('[{section}]\n'.format(section=section))
      for prop, value in sorted(props.iteritems()):
        if value is None:
          writer.write('{prop} (unset)\n'.format(prop=prop))
        else:
          writer.write('{prop} = {value}\n'.format(prop=prop, value=value))
