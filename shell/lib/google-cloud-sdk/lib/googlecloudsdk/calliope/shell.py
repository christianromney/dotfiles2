# Copyright 2013 Google Inc. All Rights Reserved.

"""Helper action for the gcloud --shell[=shell-path] option.
"""

import argparse
import os
import StringIO
import subprocess
import sys
import textwrap

from googlecloudsdk.core import log
from googlecloudsdk.core.util import files as file_utils


def EqualSpacedColmunTable(items,
                           table_width=80,
                           table_indent=2,
                           column_pad=2):
  """Construct a table of equal-spaced columns from items in a string array.

  The number of columns is determined by the data.  The table will contain at
  least one column, even if the resulting table is wider than table_width.

  Args:
    items: [str], The column data, one element per column.
    table_width: int, The total table width in characters.
    table_indent: int, The left indent space count.
    column_pad: int, The column pad space count.

  Returns:
    str, Formatted table.
  """

  # determine the max width of all items -- this is the (minimum) column width
  column_width = len(max(items, key=len))
  # adjust the column width to include the column pad
  column_width += column_pad
  # determine the number of columns that fit the indent and pad constraints
  columns = (table_width - table_indent) / column_width
  # one more pass through the items to construct the table row by row
  table = ''  # the table
  col = 0     # the current column index, col==0 => start of new row
  pad = ' ' * table_indent  # the left-padding for the next column
  for item in items:
    table += pad + item
    col += 1
    if col >= columns:
      # the next column starts a new row
      col = 0
      table += '\n'
      # the leftmost column is padded by the table indent
      pad = ' ' * table_indent
    else:
      # this pad aligns the next column
      pad = ' ' * (column_width - len(item))
  # a partial last row needs a line terminator
  if col > 0:
    table += '\n'
  return table


# This code handles { bash, dash, ksh, zsh } modulo capabilities of the shell.
def GenerateRcFile(prefix, prompt, subcommands, interactive, buf):
  """Generate the shell rc file for the subshell in buf.

  Args:
    prefix: str, The command line prefix for each of the subcommands.
    prompt: str, The prompt that should appear for the shell.
    subcommands: [str], The different subcommands that should be available.
    interactive: bool, True if interactive
    buf: writeable, Buffer to store the . file in.
  """

  actual_gcloud = os.path.abspath(sys.argv[0])

  buf.write(textwrap.dedent(
      """\
      # gcloud --shell initialization file - deleted on shell exit
      eval $_GCLOUD_RESTORE_
      _gcloud_shell=
      _gcloud_completion=
      case "{$BASH_VERSION}{$KSH_VERSION}{$ZSH_VERSION}" in
      {?*}{*}{*})  # bash
        [ -f $HOME/.bashrc ] && . $HOME/.bashrc
        _gcloud_shell=bash
        _gcloud_completion=1
        ;;
      {}{?*}{*})  # ksh
        case $ENV in
        ?*)  [ -f "$ENV" ] && . "$ENV" ;;
        esac
        _gcloud_shell=ksh
        ;;
      {}{}{?*})   # zsh
        : zsh :
        [ -f ${ZDOTDIR:-$HOME}/.zshenv ] && . ${ZDOTDIR:-$HOME}/.zshenv
        _gcloud_shell=zsh
        _gcloud_completion=1
        ;;
      *)        # assume $ENV-aware shell { dash }
        case $ENV in
        ?*)  [ -f "$ENV" ] && . "$ENV" ;;
        esac
        ;;
      esac
      """))

  if os.sep == '\\':
    # Workaround for UWIN ksh(1) PATH lookup on pure windows paths.
    # The embeded '/' means "this is literal, don't do PATH lookup".
    executable = '/'.join(sys.executable.rsplit('\\', 1))
  else:
    executable = sys.executable
  buf.write(textwrap.dedent(
      """
      PYTHONPATH='{pythonpath}'

      python() {{
        '{python}' "$@"
      }}
      gcloud() {{
        python '{actual_gcloud}' "$@"
      }}
      """).format(python=executable,
                  pythonpath=os.pathsep.join(sys.path),
                  actual_gcloud=actual_gcloud))

  if interactive:
    # the "subcommands" function lists a table of available subcommands
    buf.write(textwrap.dedent(
        """
        echo '# "subcommands" lists {prefix} subcommands,' \\
             '"exit" or CONTROL-D exits'
        subcommands() {{
          echo "{prefix} subcommands:"
          echo
          echo "{table}"
        }}
        """).format(prefix=prefix,
                    table=EqualSpacedColmunTable(
                        sorted(subcommands + ['-h', '--help']),
                        table_width=64)))
    buf.write(textwrap.dedent(
        """
        case $_gcloud_shell in
        bash|zsh)
          _rcfile=$CLOUDSDK_ROOT_DIR/completion.$_gcloud_shell.inc
          if [ -f "$_rcfile" ]
          then
            . "$_rcfile"
            _gcloud_shell_argcomplete() {{
              COMP_LINE="{prefix} $COMP_LINE"
              shift
              COMP_POINT=$(( $COMP_POINT + {prefix_pos} ))
              _python_argcomplete {prefix} "$@"
            }}
            _gcloud_complete() {{
              complete -o default -F _gcloud_shell_argcomplete "$@"
            }}
          else
            # disable completion when completion rc file not found
            _gcloud_completion=
          fi
          ;;
        esac
        """).format(prefix=prefix,
                    prefix_pos=len(prefix) + 1))
    # generate subcommand aliases and command completion assertions
    for subcommand in subcommands:
      buf.write(textwrap.dedent(
          """\
          alias {command}="{prefix} {command}"
          [ -n "$_gcloud_completion" ] && _gcloud_complete "{command}"
          """).format(command=subcommand,
                      prefix=prefix))
    # --help is problematic.  dash does not handle ``alias -- ...'' like the
    # others, but it allows ``-*'' aliases in a multi-operand alias command
    # when any ``-*'' alias is preceded by at least one identifier alias.  This
    # special case ensures the proper order.
    buf.write(textwrap.dedent(
        """\
        alias subcommands=subcommands -h="{prefix} -h" --help="{prefix} --help"

        PS1='{prompt} $ '
        """).format(prompt=prompt,
                    prefix=prefix))
  else:
    # no aliases in non-interactive scripts -- generate subcommand functions
    for subcommand in subcommands:
      buf.write(textwrap.dedent(
          """\
          {command}() {{
            {prefix} {command} "$@"
          }}
          """).format(command=subcommand,
                      prefix=prefix))
    buf.write('PS1="" PS2=""\n')


def ShellAction(subcommands, cli):
  """Get an argparse action that launches a subshell.

  Args:
    subcommands: [str], List of the commands and subgroups that will be turned
        into aliases in the subshell.
    cli: calliope.CLI, The CLI hosting this calliope session.

  Returns:
    argparse.Action, the action to use.
  """

  class Action(argparse.Action):

    def __init__(self, **kwargs):
      super(Action, self).__init__(**kwargs)

    def __call__(self, parser, namespace, values, option_string=None):
      alias_args = ['gcloud']

      # subprocess.call() below handles 'shell not found' diagnostics
      if values:
        shell = values
      else:
        shell = os.environ.get('SHELL')
        if not shell:
          # search for a default for SHELL, biased from left to right
          for shell in ['bash', 'ksh', 'sh', 'zsh', 'dash']:
            path = file_utils.FindExecutableOnPath(shell)
            if path:
              shell = path
              break
          else:
            shell = 'sh'

      for arg in cli.argv:
        if arg == '--shell' or arg.startswith('--shell='):
          # Only things up to, and not including, the first --shell.
          # TODO(user): This search can have false positives. eg,
          # $ gcloud --project --shell auth --shell
          # If someone somehow had a project "--shell", or if some other flag
          # flag value was legitimately "--shell". For now, we'll let this be
          # a problematic, but rare, corner case.
          break

        # TODO(user): Make this quoting more robust.
        if ' ' in arg:
          arg = '"{arg}"'.format(arg=arg)

        alias_args.append(arg)

      alias_prefix = ' '.join(alias_args)
      prompt = ' '.join(['gcloud']+alias_args[1:])
      interactive = sys.stdin.isatty()
      buf = StringIO.StringIO()
      GenerateRcFile(alias_prefix, prompt, subcommands, interactive, buf)

      exit_code = 0
      with file_utils.TemporaryDirectory() as tmpdir:
        # link or symlink not available on all targets so we make N copies.
        envfile = '.gcloudenv'
        for rcfile in ['.bashrc', '.zshrc', envfile]:
          path = os.path.join(tmpdir, rcfile)
          with open(path, 'w') as f:
            f.write(buf.getvalue())
            f.flush()
        try:
          restore = ''
          for name in ['HOME', 'ZDOTDIR', 'ENV']:
            val = os.environ.get(name)
            if val is not None:
              restore += ' ' + name + "='" +  val + "'"
          if restore is not '':
            restore = 'export' + restore
          env = dict(os.environ)
          env['_GCLOUD_RESTORE_'] = restore
          env['HOME'] = tmpdir
          env['ZDOTDIR'] = tmpdir
          if os.sep == '\\':
            # Workaround for UWIN ksh(1) PATH lookup on pure windows paths.
            # The embeded '/' means "this is literal, don't do PATH lookup".
            # Also handles the eval of $ENV that eliminates \'s.
            env['ENV'] = '/'.join([tmpdir, envfile]).replace('\\', '\\\\')
          else:
            env['ENV'] = path
          # Why print terminal escape sequences if stdout is not a terminal?
          if not sys.stdout.isatty():
            env['TERM'] = 'dumb'
          cmd = [shell, '-i']
          # not interactive implies batch mode with commands on stdin. Since zsh
          # insists on reading from /dev/tty we stuff it in a new sesssion which
          # detaches /dev/tty and forces it to read from stdin.  bash and dash
          # complain about no tty in -i mode, so zsh is special-cased.
          if not interactive and os.path.basename(shell).startswith('zsh'):
            # eventually change preexec_fn=os.setsid to start_new_session=True
            exit_code = subprocess.call(cmd, env=env, preexec_fn=os.setsid)
          else:
            exit_code = subprocess.call(cmd, env=env)
        except OSError as e:
          log.error("""\
could not run the shell [{shell}] -- \
make sure it is installed and on the system PATH [{e}]\
""".format(e=e, shell=shell))
          exit_code = 1

      sys.exit(exit_code)

  return Action
