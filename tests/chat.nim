# SPDX-License-Identifier: MIT

import
  std / [asyncdispatch, strutils]

import
  preserves, preserves / parse

import
  syndicate, syndicate / protocols / [simpleChatProtocol]

import
  syndicate / [actors, capabilities, dataspaces, patterns, relay]

from syndicate / protocols / protocol import Handle

from os import getCurrentProcessId

when defined(linux):
  proc getentropy(buf: pointer; bufLen: csize_t): cint {.importc,
      header: "sys/random.h".}
proc mint(): SturdyRef =
  let pr = parsePreserves("""<ref "syndicate" [] #x"a6480df5306611ddd0d3882b546e1977">""",
                          Ref)
  doAssert fromPreserve(result, pr)

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
    onPublish(turn, ds, Present ? {0: `?`()})do (username: string):
      echo username, " arrived"
    onMessage(turn, ds, Says ? {0: `?`(), 1: `?`()})do (who: string;
        what: string):
      echo who, ": ", what
    message(turn, ds, Says(who: username, what: "hello"))
echo "actor completed"