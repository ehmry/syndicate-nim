# SPDX-License-Identifier: MIT

import
  std / [hashes, macros, tables]

import
  preserves

import
  ./actors, ./bags, ./patterns, ./protocols / dataspace

from ./protocols / protocol import Handle

type
  Observe = dataspace.Observe[Ref]
  Turn = actors.Turn
type
  DuringProc* = proc (turn: var Turn; a: Assertion): TurnAction {.gcsafe.}
type
  DuringActionKind = enum
    null, dead, act
  DuringAction = object
    case
    of null, dead:
      nil
    of act:
      
  
type
  DuringEntity = ref object of Entity
  
proc duringPublish(e: Entity; turn: var Turn; a: Assertion; h: Handle) =
  var de = DuringEntity(e)
  let action = de.cb(turn, a)
  let g = de.assertionMap.getOrDefault h
  case g.kind
  of null:
    de.assertionMap[h] = DuringAction(kind: act, action: action)
  of dead:
    de.assertionMap.del h
    freshen(turn, action)
  of act:
    raiseAssert("during: duplicate handle in publish: " & $h)

proc duringRetract(e: Entity; turn: var Turn; h: Handle) =
  var de = DuringEntity(e)
  let g = de.assertionMap.getOrDefault h
  case g.kind
  of null:
    de.assertionMap[h] = DuringAction(kind: dead)
  of dead:
    raiseAssert("during: duplicate handle in retract: " & $h)
  of act:
    de.assertionMap.del h
    g.action(turn)

proc during*(cb: DuringProc): DuringEntity =
  result = DuringEntity(cb: cb)
  result.setProcs(publish = duringPublish, retract = duringRetract)

proc observe*(turn: var Turn; ds: Ref; pat: Pattern; e: Entity): Handle =
  publish(turn, ds, Observe(pattern: pat, observer: embed newRef(turn, e)))
