# Copyright 2013 Google Inc. All Rights Reserved.

"""Functions to help with shelling out to other commands."""

import os
import sys


def GetPythonExecutable():
  """Gets the path to the Python interpreter that should be used."""
  cloudsdk_python = os.environ.get('CLOUDSDK_PYTHON')
  if cloudsdk_python:
    return cloudsdk_python
  python_bin = sys.executable
  if not python_bin:
    raise ValueError('Could not find Python executable.')
  return python_bin


def GetShellExecutable():
  """Gets the path to the Shell that should be used."""
  shell = os.getenv('SHELL', None)

  shells = ['/bin/bash', '/bin/sh']
  if shell:
    shells.insert(0, shell)

  for s in shells:
    if os.path.isfile(s):
      return s

  raise ValueError("You must set your 'SHELL' environment variable to a "
                   "valid shell executable to use this tool.")


def _GetToolArgs(interpreter, interpreter_args, executable_path, *args):
  tool_args = []
  if interpreter:
    tool_args.append(interpreter)
  if interpreter_args:
    tool_args.extend(interpreter_args)
  tool_args.append(executable_path)
  tool_args.extend(list(args))
  return tool_args


def ArgsForPythonTool(executable_path, *args):
  """Constructs an argument list for calling the Python interpreter.

  Args:
    executable_path: str, The full path to the Python main file.
    *args: args for the command

  Returns:
    An argument list to execute the Python interpreter
  """
  python_executable = GetPythonExecutable()
  python_args_str = os.environ.get('CLOUDSDK_PYTHON_ARGS')
  if not python_args_str:
    # TODO(user): Remove site packages logic after b/14469124 is fixed.
    import_site_packages = (os.environ.get('CLOUDSDK_PYTHON_SITEPACKAGES') or
                            os.environ.get('VIRTUAL_ENV'))
    if import_site_packages:
      python_args_str = ''
    else:
      python_args_str = '-S'
  python_args = python_args_str.split()
  return _GetToolArgs(
      python_executable, python_args, executable_path, *args)


def ArgsForShellTool(executable_path, *args):
  """Constructs an argument list for calling the bash interpreter.

  Args:
    executable_path: str, The full path to the shell script.
    *args: args for the command

  Returns:
    An argument list to execute the bash interpreter
  """
  shell_bin = GetShellExecutable()
  return _GetToolArgs(shell_bin, [], executable_path, *args)


def ArgsForCMDTool(executable_path, *args):
  """Constructs an argument list for calling the cmd interpreter.

  Args:
    executable_path: str, The full path to the cmd script.
    *args: args for the command

  Returns:
    An argument list to execute the cmd interpreter
  """
  return _GetToolArgs('cmd', ['/c'], executable_path, *args)


def ArgsForBinaryTool(executable_path, *args):
  """Constructs an argument list for calling a native binary.

  Args:
    executable_path: str, The full path to the binary.
    *args: args for the command

  Returns:
    An argument list to execute the native binary
  """
  return _GetToolArgs(None, None, executable_path, *args)
