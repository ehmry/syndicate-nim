# SPDX-License-Identifier: MIT

## See the swaybar-protocol(7) manpage.
import
  std / [asyncdispatch, json, os, strutils]

import
  preserves, preserves / jsonhooks

import
  syndicate, syndicate / capabilities

proc unixSocketPath(): string =
  let args = commandLineParams()
  case args.len
  of 1:
    result = args[0]
  of 0:
    result = getEnv("SYNDICATE_SOCK")
    if result == "":
      result = getEnv("XDG_RUNTIME_DIR", "/run/user/1000") / "dataspace"
  else:
    quit "must pass Syndicate socket location as the only argument"

proc mintCap(): SturdyRef =
  var key: array[16, byte]
  mint(key, "syndicate")

type
  SwaybarStatus {.preservesRecord: "swaybar-status".} = object
  
bootDataspace("main")do (root: Ref; turn: var Turn):
  let header = %*{"version": 1}
  stdout.write(header, "\n")
  stdout.flushFile()
  stdout.write("[")
  connectUnix(turn, unixSocketPath(), mintCap())do (turn: var Turn; ds: Ref):
    var lineElements = initSet[Ref]()
    proc sendLine(turn: var Turn) =
      var line: JsonNode
      if fromPreserve(line, lineElements):
        stdout.writeLine(line, ",")
        stdout.flushFile()
      else:
        stderr.writeLine "could not convert ", lineElements

    during(turn, ds, ?SwaybarStatus)do (a: Assertion):
      lineElements.incl a
      sendLine(turn)
    do:
      lineElements.excl a
      sendLine(turn)
runForever()