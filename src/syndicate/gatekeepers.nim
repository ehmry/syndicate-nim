# SPDX-License-Identifier: MIT

import
  std / options, pkg / preserves, ../syndicate, ./protocols / [gatekeeper, rpc]

export
  gatekeeper, rpc

proc resultOk*[T](v: T): Result =
  ## Sugar for producing a `ok` record.
  result = Result(orKind: ResultKind.ok)
  result.ok.value = v.toPreserves

proc resultError*[T](v: T): Result =
  ## Sugar for producing a `ok` record.
  result = Result(orKind: ResultKind.error)
  result.error.error = v.toPreserves

proc toResolved*(res: Result): Resolved =
  case res.orKind
  of ResultKind.ok:
    var cap = res.ok.value.unembed(Cap)
    result = Resolved(orKind: ResolvedKind.accepted)
    result.accepted.responderSession = cap.get
  of ResultKind.error:
    result = Resolved(orKind: ResolvedKind.Rejected)
    result.rejected.detail = res.error.error

type
  Gatekeeper = ref object
  
proc spawnGatekeeper*(turn: Turn; target: Cap; name = "gatekeeper"): Gatekeeper =
  ## Spawn a new `Gatekeeper`.
  result = Gatekeeper(ds: target)
  result.actor = spawnActor(turn, name)do (turn: Turn):
    preventInertCheck(turn)

proc stop*(gk: Gatekeeper) =
  ## Stop a `Gatekeeper`.
  stop(gk.actor)

proc serve*[T](gk: Gatekeeper; resolve: proc (turn: Turn; step: T): Result) =
  ## Call `resolve` for resolve requests at `gk` that match type `T`.
  ## `resolve` is called in the context of a new actor.
  gk.actor.facet.rundo (turn: Turn):
    during(turn, gk.ds, Resolve ?: {0: grabType(T), 1: grab()})do (step: T;
        obs: Cap):
      try:
        block:
          var r = resolve(turn, step)
          publish(turn, obs, r)
          publish(turn, obs, r.toResolved)
      except CatchableError as err:
        var reject = Resolved(orKind: ResolvedKind.Rejected)
        reject.rejected.detail = err.msg.toPreserves
        publish(turn, obs, reject)
        publish(turn, obs, err.msg.resultError)
