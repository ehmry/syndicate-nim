# SPDX-License-Identifier: MIT

import
  std / [asyncdispatch, random]

import
  preserves

import
  syndicate

import
  syndicate / [actors, capabilities]

randomize()
type
  A* {.preservesRecord: "A".} = object
    str*: string

  B* {.preservesRecord: "B".} = object
    str*: string

bootDataspace("x")do (ds: Ref; turn: var Turn):
  connectStdio(ds, turn)
  discard publish(turn, ds, A(str: "A stdio"))
  discard publish(turn, ds, B(str: "B stdio"))
  onPublish(turn, ds, ?A)do (v: Assertion):
    stderr.writeLine "received over stdio ", v
bootDataspace("y")do (ds: Ref; turn: var Turn):
  connectUnix(turn, "/run/user/1000/dataspace", capabilities.mint())do (
      turn: var Turn; a: Assertion) -> TurnAction:
    let ds = unembed a
    discard publish(turn, ds, A(str: "A unix"))
    discard publish(turn, ds, B(str: "B unix"))
    onPublish(turn, ds, ?B)do (v: Assertion):
      stderr.writeLine "received over unix ", v
runForever()