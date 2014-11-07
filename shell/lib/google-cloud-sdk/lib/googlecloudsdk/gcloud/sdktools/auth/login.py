# Copyright 2013 Google Inc. All Rights Reserved.

"""The auth command gets tokens via oauth2."""

import textwrap
import webbrowser

from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions as c_exc
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.core.credentials import gce as c_gce
from googlecloudsdk.core.credentials import store as c_store
from googlecloudsdk.core.util import console_io


# A list of results for webbrowser.get().name that indicate we should not
# attempt to open a web browser for the user.
_WEBBROWSER_NAMES_BLACKLIST = [
    'www-browser',
]


class Login(base.Command):
  """Get credentials for the tools in the Google Cloud SDK via a web flow.

  Obtains access credentials for the Google Cloud Platform resources of the
  given account, via a web flow.  If the account already has valid credentials,
  it is set as active without re-running the web flow.
  """

  @staticmethod
  def Args(parser):
    """Set args for gcloud auth."""

    parser.add_argument(
        '--no-launch-browser',
        action='store_false', default=True, dest='launch_browser',
        help=('Print a URL to be copied instead of launching a web browser.'))
    parser.add_argument(
        '--do-not-activate', action='store_true',
        help='Do not set the new credentials as active.')
    parser.add_argument(
        '--force', action='store_true',
        help='Re-run the web flow even if the given account has valid '
        'credentials.')
    parser.add_argument(
        'account', nargs='?', help='The account to log in as.')

  @c_exc.RaiseToolExceptionInsteadOf(c_store.Error)
  def Run(self, args):
    """Run the authentication command."""

    if c_gce.Metadata().connected:
      message = textwrap.dedent("""
          You are running on a GCE VM. It is recommended that you use
          service accounts for authentication.

          You can run:

            $ gcloud config set account ``ACCOUNT''

          to switch accounts if necessary.

          Your credentials may be visible to others with access to this
          virtual machine. Are you sure you want to authenticate with
          your personal account?
          """)
      answer = console_io.PromptContinue(message=message)
      if not answer:
        return None

    account = args.account

    if account and not args.force:
      creds = c_store.LoadIfValid(account=account)
      if creds:
        # Account already has valid creds, just switch to it.
        return self.LoginAs(account, creds, args.project, args.do_not_activate)

    # No valid creds, do the web flow.
    creds = self.DoWebFlow(args.launch_browser)
    web_flow_account = creds.token_response['id_token']['email']
    if account and account != web_flow_account:
      raise c_exc.ToolException(
          'You attempted to log in as account [{account}] but the received '
          'credentials were for account [{web_flow_account}].\n\n'
          'Please check that your browser is logged in as account [{account}] '
          'and that you are using the correct browser profile.'.format(
              account=account, web_flow_account=web_flow_account))

    account = web_flow_account
    # We got new creds, and they are for the correct user.
    c_store.Store(creds, account)
    return self.LoginAs(account, creds, args.project, args.do_not_activate)

  def LoginAs(self, account, creds, project, do_not_activate):
    """Logs in with valid credentials."""
    if do_not_activate:
      return creds
    properties.PersistProperty(properties.VALUES.core.account, account)
    if project:
      properties.PersistProperty(properties.VALUES.core.project, project)
    log.out.write(
        '\nYou are now logged in as [{account}].\n'
        'Your current project is [{project}].  You can change this setting by '
        'running:\n  $ gcloud config set project ``PROJECT''\n'.format(
            account=account, project=properties.VALUES.core.project.Get()))
    return creds

  def DoWebFlow(self, launch_browser):
    """Launches a browser to get credentials."""
    try:
      # Sometimes it's not possible to launch the web browser. This often
      # happens when people ssh into other machines.
      if launch_browser:
        if c_gce.Metadata().connected:
          launch_browser = False
        try:
          browser = webbrowser.get()
          if (hasattr(browser, 'name')
              and browser.name in _WEBBROWSER_NAMES_BLACKLIST):
            launch_browser = False
        except webbrowser.Error:
          launch_browser = False

      return c_store.AcquireFromWebFlow(launch_browser=launch_browser)
    except c_store.FlowError:
      log.error(
          ('There was a problem with the web flow. Try running with '
           '--no-launch-browser'))
      raise
