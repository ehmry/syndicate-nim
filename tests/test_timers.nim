# SPDX-License-Identifier: MIT

import
  std / times

import
  syndicate, syndicate / actors / timers

proc now(): float64 =
  getTime().toUnixFloat()

runActor("test_timers")do (ds: Cap; turn: var Turn):
  onPublish(turn, ds, grab(LaterThan(seconds: now() + 1.0))):
    stderr.writeLine "slept one second once"
    onPublish(turn, ds, grab(LaterThan(seconds: now() + 1.0))):
      stderr.writeLine "slept one second twice"
      onPublish(turn, ds, grab(LaterThan(seconds: now() + 1.0))):
        stderr.writeLine "slept one second thrice"
        quit()
  spawnTimers(turn, ds)