# SPDX-License-Identifier: MIT

import
  std / [hashes, tables]

import
  preserves

import
  ./actors, ./protocols / dataspace, ./skeletons

from ./protocols / protocol import Handle

type
  Assertion = Preserve[Cap]
  Observe = dataspace.Observe[Cap]
  Turn = actors.Turn
  Dataspace {.final.} = ref object of Entity
  
method publish(ds: Dataspace; turn: var Turn; a: AssertionRef; h: Handle) {.
    gcsafe.} =
  if add(ds.index, turn, a.value):
    var obs: Observe
    if obs.fromPreserve a.value:
      ds.index.add(turn, obs.pattern, obs.observer)
  ds.handleMap[h] = a.value

method retract(ds: Dataspace; turn: var Turn; h: Handle) {.gcsafe.} =
  let v = ds.handleMap[h]
  if remove(ds.index, turn, v):
    ds.handleMap.del h
    var obs: Observe
    if obs.fromPreserve v:
      ds.index.remove(turn, obs.pattern, obs.observer)

method message(ds: Dataspace; turn: var Turn; a: AssertionRef) {.gcsafe.} =
  ds.index.deliverMessage(turn, a.value)

proc newDataspace*(turn: var Turn): Cap =
  newCap(turn, Dataspace(index: initIndex()))

type
  BootProc = proc (ds: Cap; turn: var Turn) {.gcsafe.}
proc bootDataspace*(name: string; bootProc: BootProc): Actor =
  bootActor(name)do (turn: var Turn):
    discard turn.facet.preventInertCheck()
    bootProc(newDataspace(turn), turn)
