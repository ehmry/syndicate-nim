# SPDX-License-Identifier: MIT

import
  std / [hashes, tables]

import
  preserves

import
  ./actors, ./protocols / dataspace, ./skeletons

from ./protocols / protocol import Handle

type
  Assertion = Preserve[Ref]
  Observe = dataspace.Observe[Ref]
  Turn = actors.Turn
  Dataspace {.final.} = ref object of Entity
  
method publish(ds: Dataspace; turn: var Turn; v: Assertion; h: Handle) =
  if add(ds.index, turn, v):
    var obs: Observe
    if obs.fromPreserve v:
      ds.index.add(turn, obs.pattern, obs.observer)
  ds.handleMap[h] = v

method retract(ds: Dataspace; turn: var Turn; h: Handle) =
  try:
    let v = ds.handleMap[h]
    if remove(ds.index, turn, v):
      ds.handleMap.del h
      var obs: Observe
      if obs.fromPreserve v:
        ds.index.remove(turn, obs.pattern, obs.observer)
  except KeyError:
    discard

method message(ds: Dataspace; turn: var Turn; v: Assertion) =
  ds.index.deliverMessage(turn, v)

proc newDataspace*(turn: var Turn): Ref =
  newRef(turn, Dataspace(index: initIndex()))

type
  BootProc = proc (ds: Ref; turn: var Turn) {.gcsafe.}
proc bootDataspace*(name: string; bootProc: BootProc): Actor {.discardable.} =
  bootActor(name)do (turn: var Turn):
    discard turn.facet.preventInertCheck()
    bootProc(newDataspace(turn), turn)
