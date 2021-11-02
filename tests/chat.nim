# SPDX-License-Identifier: MIT

import
  std / [asyncdispatch, strutils]

import
  preserves, preserves / parse

import
  syndicate, syndicate / [actors, capabilities, dataspaces, patterns, relay],
  syndicate / protocols / [simpleChatProtocol]

from syndicate / protocols / protocol import Handle

from os import getCurrentProcessId

proc mint(): SturdyRef =
  var key: array[16, byte]
  mint(key, "syndicate")

waitFor runActor("chat")do (turn: var Turn):
  let cap = mint()
  connectUnix(turn, "/run/syndicate/ds", cap)do (turn: var Turn; a: Assertion) -> TurnAction:
    let ds = unembed a
    var
      username: string
      usernameHandle: Handle
    proc updateUsername(turn: var Turn; u: string) =
      username = u
      var p = Present(username: username)
      usernameHandle = replace(turn, ds, usernameHandle, p)

    updateUsername(turn, "user" & $getCurrentProcessId())
    onPublish(turn, ds, Present ? {0: `?*`()})do (username: string):
      echo username, " arrived"
      onRetract:
        echo username, " left"
    onMessage(turn, ds, Says ? {0: `?*`(), 1: `?*`()})do (who: string;
        what: string):
      echo who, ": ", what
    message(turn, ds, Says(who: username, what: "hello"))
echo "actor completed"