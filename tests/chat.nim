# SPDX-License-Identifier: MIT

import
  std / [asyncdispatch, asyncfile, random, strutils]

import
  preserves, preserves / parse

import
  syndicate / protocols / [simpleChatProtocol, sturdy]

import
  syndicate / [actors, dataspaces, patterns, relay]

from syndicate / protocols / protocol import Handle

from os import getCurrentProcessId

randomize()
const
  capStr = """<ref "syndicate" [] #x"a6480df5306611ddd0d3882b546e1977">"""
proc noOp(turn: var Turn) =
  discard

waitFor runActor("chat")do (turn: var Turn):
  var cap: SturdyRef[Ref]
  doAssert fromPreserve(cap, parsePreserves(capStr, Ref))
  connectUnix(turn, "/run/syndicate/ds", cap)do (turn: var Turn; a: Assertion) -> TurnAction:
    let ds = unembed a
    var
      username: string
      usernameHandle: Handle
    proc updateUsername(turn: var Turn; u: string) =
      username = u
      usernameHandle = replace(turn, ds, usernameHandle,
                               Present(username: username))

    updateUsername(turn, "user" & $getCurrentProcessId())
    echo "username updated?"
    proc duringPresent(turn: var Turn; a: Assertion): TurnAction =
      echo "observed ", a
      noOp

    discard observe(turn, ds, toPattern(Present), during(duringPresent))
    echo "post-observe"
echo "actor completed"