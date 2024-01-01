# SPDX-License-Identifier: MIT

import
  std / [hashes, options, tables]

import
  preserves

import
  ./actors, ./protocols / dataspace, ./skeletons

from ./protocols / protocol import Handle

type
  Assertion = Value
  Observe = dataspace.Observe
  Turn = actors.Turn
  Dataspace {.final.} = ref object of Entity
  
method publish(ds: Dataspace; turn: var Turn; a: AssertionRef; h: Handle) {.
    gcsafe.} =
  if add(ds.index, turn, a.value):
    var obs = a.value.preservesTo(Observe)
    if obs.isSome:
      var cap = obs.get.observer.unembed(Cap)
      if cap.isSome:
        ds.index.add(turn, obs.get.pattern, cap.get)
  ds.handleMap[h] = a.value

method retract(ds: Dataspace; turn: var Turn; h: Handle) {.gcsafe.} =
  let v = ds.handleMap[h]
  if remove(ds.index, turn, v):
    ds.handleMap.del h
    var obs = v.preservesTo(Observe)
    if obs.isSome:
      var cap = obs.get.observer.unembed(Cap)
      ds.index.remove(turn, obs.get.pattern, cap.get)

method message(ds: Dataspace; turn: var Turn; a: AssertionRef) {.gcsafe.} =
  ds.index.deliverMessage(turn, a.value)

proc newDataspace*(turn: var Turn): Cap =
  newCap(turn, Dataspace(index: initIndex()))

type
  BootProc = proc (turn: var Turn; ds: Cap) {.gcsafe.}
type
  DeprecatedBootProc = proc (ds: Cap; turn: var Turn) {.gcsafe.}
proc bootDataspace*(name: string; bootProc: BootProc): Actor =
  bootActor(name)do (turn: var Turn):
    discard turn.facet.preventInertCheck()
    bootProc(turn, newDataspace(turn))

proc bootDataspace*(name: string; bootProc: DeprecatedBootProc): Actor {.
    deprecated.} =
  bootDataspace(name)do (turn: var Turn; ds: Cap):
    bootProc(ds, turn)
