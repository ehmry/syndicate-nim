# SPDX-License-Identifier: MIT

import
  std / [hashes, tables]

import
  preserves

import
  ./actors, ./patterns, ./protocols / dataspace

from ./protocols / protocol import Handle

type
  Observe = dataspace.Observe
  Turn = actors.Turn
type
  DuringProc* = proc (turn: var Turn; a: Assertion; h: Handle): TurnAction {.
      gcsafe.}
  DuringActionKind = enum
    null, dead, act
  DuringAction = object
    case
    of null, dead:
      nil
    of act:
      
  
  DuringEntity {.final.} = ref object of Entity
  
method publish(de: DuringEntity; turn: var Turn; a: AssertionRef; h: Handle) =
  let action = de.cb(turn, a.value, h)
  let g = de.assertionMap.getOrDefault h
  case g.kind
  of null:
    de.assertionMap[h] = DuringAction(kind: act, action: action)
  of dead:
    de.assertionMap.del h
    freshen(turn, action)
  of act:
    raiseAssert("during: duplicate handle in publish: " & $h)

method retract(de: DuringEntity; turn: var Turn; h: Handle) =
  let g = de.assertionMap.getOrDefault h
  case g.kind
  of null:
    de.assertionMap[h] = DuringAction(kind: dead)
  of dead:
    raiseAssert("during: duplicate handle in retract: " & $h)
  of act:
    de.assertionMap.del h
    if not g.action.isNil:
      g.action(turn)

proc during*(cb: DuringProc): DuringEntity =
  DuringEntity(cb: cb)

proc observe*(turn: var Turn; ds: Cap; pat: Pattern; e: Entity): Handle =
  publish(turn, ds, Observe(pattern: pat, observer: newCap(turn, e)))
