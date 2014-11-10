# Copyright 2014 Google Inc. All Rights Reserved.
"""Common utility functions for Updater."""

import time

from googlecloudsdk.core.util import console_io


def WaitForUpdateState(
    client, update_ref, desired_states, desired_target_state, message):
  """Waits until the given update reaches desired state.

  Wait loop terminates when either:
    1. The update's state becomes one of desired_states.
  or
    2. The update's target state becomes different than desired_target_state.

  Args:
    client: interface to the updater service
    update_ref: reference to the update being watched
    desired_states: list of desired states
    desired_target_state: desired target state
    message: message to be displayed by progress tracker

  Returns:
    ReplicaPoolUpdate, the most recent representation of the update.
  """
  with console_io.ProgressTracker(message, autotick=False) as pt:
    while True:
      update = client.Get(update_ref.Request())
      if (update.state in desired_states
          or update.targetState != desired_target_state):
        return update
      pt.Tick()
      time.sleep(2)
