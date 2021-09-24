# SPDX-License-Identifier: MIT

import
  std / [asyncdispatch, macros, options]

import
  preserves

import
  syndicate / [actors]

proc startActorSystem*(name: string; bootProc: TurnAction): Future[void] =
  let actor = newActor(name, bootProc)
  result = actor.future
