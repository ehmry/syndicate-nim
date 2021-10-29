# SPDX-License-Identifier: MIT

import
  std / [asyncdispatch, asyncfile, strutils]

import
  preserves, preserves / parse

import
  syndicate / protocols / [simpleChatProtocol]

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
  assert fromPreserve(result, pr)

proc noOp(turn: var Turn) =
  discard

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
    proc duringPresent(turn: var Turn; v: Assertion): TurnAction =
      var a: tuple[username: string]
      assert fromPreserve(a, v), $v
      echo a.username, " arrived"
      proc onRetract(turn: var Turn) =
        echo a.username, " left"

      onRetract

    discard observe(turn, ds, Present ? {0: `?`()}, during(duringPresent))
    discard observe(turn, ds, Says ? {0: `?`(), 1: `?`()}, newEntity(message = proc (
        e: Entity; turn: var Turn; v: Assertion) =
      var msg: tuple[who: string, what: string]
      assert fromPreserve(msg, v), $v
      echo msg.who, ": ", msg.what))
    message(turn, ds, Says(who: username, what: "hello"))
echo "actor completed"