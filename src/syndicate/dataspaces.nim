# SPDX-License-Identifier: MIT

import
  std / [asyncdispatch, hashes, macros, options, tables]

import
  preserves

import
  ../syndicate / protocols / [dataspace, dataspacePatterns]

import
  ./actors, ./bags

from ../syndicate / protocols / protocol import Handle

type
  Value = Preserve[Ref]
  Pattern* = dataspacePatterns.Pattern[Ref]
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
  
method publish(de: DuringEntity; turn: var Turn; a: Assertion; h: Handle) =
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

method retract(de: DuringEntity; turn: var Turn; h: Handle) =
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

proc observe*(turn: var Turn; ds: Ref; pat: Pattern; e: Entity): Handle =
  publish(turn, ds, Observe(pattern: pat, observer: embed newRef(turn, e)))
