# SPDX-License-Identifier: MIT

import
  std / [hashes, tables], pkg / preserves, ./actors, ./patterns,
  ./protocols / dataspace

type
  DuringProc* = proc (turn: Turn; a: Value; h: Handle): TurnAction
  DuringActionKind = enum
    null, dead, act
  DuringAction = object
    case
    of null, dead:
      nil
    of act:
      
  
  DuringEntity {.final.} = ref object of Entity
  
method publish(de: DuringEntity; turn: Turn; a: AssertionRef; h: Handle) =
  discard inFacet(turn)do (turn: Turn):
    let action = de.cb(turn, a.value, h)
    let g = de.assertionMap.getOrDefault h
    case g.kind
    of null:
      de.assertionMap[h] = DuringAction(kind: act, action: action)
    of dead:
      de.assertionMap.del h
      action(turn)
    of act:
      raiseAssert("during: duplicate handle in publish: " & $h)

method retract(de: DuringEntity; turn: Turn; h: Handle) =
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

proc observe*(turn: Turn; ds: Cap; pat: Pattern; e: Entity): Handle =
  publish(turn, ds, Observe(pattern: pat, observer: newCap(turn, e)))
