# SPDX-License-Identifier: MIT

import
  std / [asyncdispatch, os, times]

import
  preserves, syndicate, syndicate / actors / timers

proc now(): float64 =
  getTime().toUnixFloat()

proc testTimers(turn: var Turn; ds: Ref) =
  onPublish(turn, ds, ?LaterThan(seconds: now() + 1.0)):
    stderr.writeLine "slept one second once"
    onPublish(turn, ds, ?LaterThan(seconds: now() + 1.0)):
      stderr.writeLine "slept one second twice"
      onPublish(turn, ds, ?LaterThan(seconds: now() + 1.0)):
        stderr.writeLine "slept one second thrice"
        quit()
  spawnTimers(turn, ds)

type
  Args {.preservesDictionary.} = object
  
proc asInferior(): bool =
  commandLineParams() == @["--inferior"]

if asInferior():
  stderr.writeLine "connect stdio"
  runActor("test_timers")do (root: Ref; turn: var Turn):
    connectStdio(root, turn)
    during(turn, root, ?Args)do (ds: Ref):
      testTimers(turn, ds)
else:
  stderr.writeLine "use local dataspace"
  discard bootDataspace("test_timers")do (ds: Ref; turn: var Turn):
    testTimers(turn, ds)
  for i in 0 .. 10:
    poll()