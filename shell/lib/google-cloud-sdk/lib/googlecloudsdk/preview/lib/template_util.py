# Copyright 2014 Google Inc. All Rights Reserved.

"""Common utility functions for DeploymentManger Templates.

   Sample DeploymentManager Template.
   The %file: entries are replaced with commands from the referenced scripts.

   {
      name: simple_deployment
      modules: {}
      config: {}
      actions: [
          action-foo: {
            commands: [
              "echo 'Hello World!'",
              "%file:script1.sh"
            ]
          },
          action-bar: {
            commands: [
              "echo 'Hello World 2!'",
              "%file:script2.sh"
            ]
          }
      ]
   }
"""

import os
import uuid
import yaml

from googlecloudsdk.calliope import exceptions

ACTIONS = 'actions'
AMPERSANDS = ' && '
COMMANDS = 'commands'
NAME = 'name'
NEW_LINE = '\n'
PERCENT_FILE = '%file:'
USER_SCRIPTS_DIR = 'user-scripts/'


def ParseTemplate(template_file, template_name):
  """Parses a DeploymentManager Template YAML file and returns it as a dict.

  Args:
    template_file: Absolute path to a YAML template file
    template_name: The name to assign the template parsed from the template_file

  Returns:
    the parsed template

  Raises:
    ToolException: if unable to read or parse the template file
  """
  new_template = None
  with open(template_file) as opened_file:
    new_template = yaml.load(opened_file)

  if not isinstance(new_template, dict):
    raise exceptions.ToolException(
        'Unable to fetch Template from file ', template_file)
  absolute_path = os.path.dirname(os.path.abspath(template_file))
  FindAndReplaceFileReferences(new_template, absolute_path)
  new_template[NAME] = template_name

  return new_template


def FindAndReplaceFileReferences(template, absolute_path):
  """Processes %file:script.sh entries in DeploymentManager Template Actions.

     It replaces the %file:* entries with the commands from the script.sh files

  Args:
     template: the parsed template dict.
     absolute_path: the directory path that the template file and
           scripts are to be found in.
  """
  if not isinstance(template, dict) or ACTIONS not in template:
    return

  actions = template[ACTIONS]
  if not isinstance(actions, dict):
    return

  for action in actions.values():
    if not isinstance(action, dict) or COMMANDS not in action:
      continue

    expanded_commands = []
    # Go through each command and expand %file: entries
    for command in action[COMMANDS]:
      expanded_commands.append(ParseCommands(command, absolute_path))

    action[COMMANDS] = expanded_commands


def GenerateCommandName(filename):
  """Generates unique strings given a filename.

  Args:
    filename: the name to prefix to the generated unique string

  Returns:
    a random string that is prefixed with the provided filename
  """
  return str(uuid.uuid4()).replace('-', '') + '-' + filename


def ParseCommands(command, absolute_path):
  """Parses commands out of a single file and returns them.

  Args:
    command: a command entry to parse. Can be a normal command, or a %file:...
        reference to commands in a shell script.
    absolute_path: the directory path that the template file and
        scripts are to be found in.

  Returns:
    the list of parsed commands

  Raises:
    BadFileException: if unable to find any %file: referenced scripts
  """
  if not isinstance(command, str):
    return

  if not command.startswith(PERCENT_FILE):
    return command

  script_file = command.replace(PERCENT_FILE, '', 1)  # remove the %file prefix

  script_name = os.path.basename(script_file)
  filepath = os.path.join(absolute_path, script_file)
  if not os.path.exists(filepath):
    raise exceptions.BadFileException(
        'Unable to find file corresponding to [' + command +
        '] referenced in Template. Expected to find ' + filepath)

  commands = []
  command_name = GenerateCommandName(script_name)

  file_dest = USER_SCRIPTS_DIR + command_name
  # create the USER_SCRIPTS_DIR in case it hasn't been created before
  commands.append('/bin/mkdir -p ' + USER_SCRIPTS_DIR)
  commands.append(AMPERSANDS)

  # copy the script using: cat << EOF > file_dest

  # Note: The double quotes "" around command_name are _crucial_!!!
  # They disallow sub-commands like `sub_cmd` inside the scripts contents
  # from getting executing while the cat_command is running.
  cat_command = 'cat << "' + command_name + '" > ' + file_dest + NEW_LINE
  with open(filepath) as script:
    contents = script.read()
    cat_command += contents
  cat_command += NEW_LINE
  cat_command += command_name  # EOF
  cat_command += NEW_LINE
  commands.append(cat_command)

  # make the script executable
  commands.append('/bin/chmod +x ' + file_dest)
  commands.append(AMPERSANDS)

  # run the executable script
  commands.append('./' + file_dest)

  return ''.join(commands)
