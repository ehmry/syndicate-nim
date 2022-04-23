# SPDX-License-Identifier: MIT

## This module implements the `Syndicate DSL <https://syndicate-lang.org/doc/syndicate/>`_.
runnableExamples:
  from syndicate / protocols / simpleChatProtocol import Present, Says

  import
    std / asyncdispatch

  bootDataspace("example")do (ds: Ref; turn: var Turn):
    let
      me = "user"
      presenceHandle = publish(turn, ds, Present(username: me))
    onMessage(turn, ds, ?Says)do (who: string; what: string):
      echo who, ": ", what
      retract(turn, presenceHandle)
    during(turn, ds, ?Present)do (username: string):
      echo "[", username, " arrived]"
      message(turn, ds, Says(who: me, what: "users are losers"))
    do:
      echo "[", username, "departed]"
  poll()
import
  std / macros

import
  preserves, preserves / jsonhooks

import
  ./syndicate / [actors, dataspaces, durings, patterns]

from ./syndicate / relays import connectStdio, connectUnix

export
  Assertion, Facet, Handle, Ref, Symbol, Turn, TurnAction, bootDataspace, `?`,
  `$`, connectStdio, connectUnix, facet, drop, grab, message, publish, retract,
  replace, run, stop, unembed

type
  PublishProc = proc (turn: var Turn; v: Assertion; h: Handle) {.closure.}
  RetractProc = proc (turn: var Turn; h: Handle) {.closure.}
  MessageProc = proc (turn: var Turn; v: Assertion) {.closure.}
  ClosureEntity = ref object of Entity
  
method publish(e: ClosureEntity; turn: var Turn; v: Assertion; h: Handle) =
  if not e.publishImpl.isNil:
    e.publishImpl(turn, v, h)

method retract(e: ClosureEntity; turn: var Turn; h: Handle) =
  if not e.retractImpl.isNil:
    e.retractImpl(turn, h)

method message(e: ClosureEntity; turn: var Turn; v: Assertion) =
  if not e.messageImpl.isNil:
    e.messageImpl(turn, v)

proc wrapPublishHandler(handler: NimNode): NimNode =
  handler.expectKind nnkDo
  var innerProc = newNimNode(nnkProcDef)
  handler.copyChildrenTo innerProc
  innerProc[0] = genSym(nskProc, "message")
  var
    formalArgs = handler[3]
    valuesSym = genSym(nskVar, "values")
    valuesTuple = newNimNode(nnkTupleTy, handler)
    innerTuple = newNimNode(nnkVarTuple, handler)
    varSectionInner = newNimNode(nnkVarSection, handler).add(innerTuple)
  for i, arg in formalArgs:
    if i > 0:
      arg.expectKind nnkIdentDefs
      if arg[1].kind == nnkEmpty:
        error("type required for capture", arg)
      var def = newNimNode(nnkIdentDefs, arg)
      arg.copyChildrenTo def
      valuesTuple.add(def)
      innerTuple.add(arg[0])
  innerTuple.add(newEmptyNode(), valuesSym)
  var
    varSectionOuter = newNimNode(nnkVarSection, handler).add(
        newIdentDefs(valuesSym, valuesTuple))
    publishBody = newStmtList(varSectionInner, handler[6])
    turnSym = ident"turn"
    handleSym = ident"handle"
    handlerSym = genSym(nskProc, "publish")
  quote:
    proc `handlerSym`(`turnSym`: var Turn; bindings: Assertion;
                      `handleSym`: Handle) =
      `varSectionOuter`
      if fromPreserve(`valuesSym`, bindings):
        `publishBody`

  
proc wrapMessageHandler(handler: NimNode): NimNode =
  handler.expectKind nnkDo
  var innerProc = newNimNode(nnkProcDef)
  handler.copyChildrenTo innerProc
  innerProc[0] = genSym(nskProc, "message")
  var
    formalArgs = handler[3]
    valuesSym = genSym(nskVar, "values")
    valuesTuple = newNimNode(nnkTupleTy, handler)
    innerTuple = newNimNode(nnkVarTuple, handler)
    varSectionInner = newNimNode(nnkVarSection, handler).add(innerTuple)
  for i, arg in formalArgs:
    if i > 0:
      arg.expectKind nnkIdentDefs
      if arg[1].kind == nnkEmpty:
        error("type required for capture", arg)
      var def = newNimNode(nnkIdentDefs, arg)
      arg.copyChildrenTo def
      valuesTuple.add(def)
      innerTuple.add(arg[0])
  innerTuple.add(newEmptyNode(), valuesSym)
  var
    varSectionOuter = newNimNode(nnkVarSection, handler).add(
        newIdentDefs(valuesSym, valuesTuple))
    body = newStmtList(varSectionInner, handler[6])
    turnSym = ident"turn"
    handlerSym = genSym(nskProc, "message")
  quote:
    proc `handlerSym`(`turnSym`: var Turn; bindings: Assertion) =
      `varSectionOuter`
      if fromPreserve(`valuesSym`, bindings):
        `body`

  
macro onPublish*(turn: Turn; ds: Ref; pattern: Pattern; doHandler: untyped) =
  let
    handlerProc = wrapPublishHandler(doHandler)
    handlerSym = handlerProc[0]
  result = quote do:
    `handlerProc`
    discard observe(`turn`, `ds`, `pattern`,
                    ClosureEntity(publishImpl: `handlerSym`))

macro onMessage*(turn: Turn; ds: Ref; pattern: Pattern; doHandler: untyped) =
  let
    handlerProc = wrapMessageHandler(doHandler)
    handlerSym = handlerProc[0]
  result = quote do:
    `handlerProc`
    discard observe(`turn`, `ds`, `pattern`,
                    ClosureEntity(messageImpl: `handlerSym`))

proc wrapDuringHandler(entryBody, exitBody: NimNode): NimNode =
  entryBody.expectKind nnkDo
  var innerProc = newNimNode(nnkProcDef)
  entryBody.copyChildrenTo innerProc
  innerProc[0] = genSym(nskProc, "during")
  var
    formalArgs = entryBody[3]
    valuesSym = ident("rawValues")
    valuesTuple = newNimNode(nnkTupleTy, entryBody)
    innerTuple = newNimNode(nnkVarTuple, entryBody)
    varSectionInner = newNimNode(nnkVarSection, entryBody).add(innerTuple)
  for i, arg in formalArgs:
    if i > 0:
      arg.expectKind nnkIdentDefs
      if arg[1].kind == nnkEmpty:
        error("type required for capture", arg)
      var def = newNimNode(nnkIdentDefs, arg)
      arg.copyChildrenTo def
      valuesTuple.add(def)
      innerTuple.add(arg[0])
  innerTuple.add(newEmptyNode(), valuesSym)
  var
    varSectionOuter = newNimNode(nnkVarSection, entryBody).add(
        newIdentDefs(valuesSym, valuesTuple))
    publishBody = newStmtList(varSectionInner, entryBody[6])
    turnSym = ident"turn"
    bindingsSym = ident"bindings"
    handleSym = ident"duringHandle"
    entrySym = genSym(nskProc, "during")
    duringSym = genSym(nskProc, "during")
  if exitBody.isNil:
    quote:
      proc `duringSym`(`turnSym`: var Turn; `bindingsSym`: Assertion;
                       `handleSym`: Handle): TurnAction =
        `varSectionOuter`
        if fromPreserve(`valuesSym`, `bindingsSym`):
          `publishBody`

  else:
    quote:
      proc `duringSym`(`turnSym`: var Turn; `bindingsSym`: Assertion;
                       `handleSym`: Handle): TurnAction =
        `varSectionOuter`
        if fromPreserve(`valuesSym`, `bindingsSym`):
          `publishBody`
          proc action(turn: var Turn) =
            `exitBody`

          result = action

  
macro during*(turn: Turn; ds: Ref; pattern: Pattern;
              publishBody, retractBody: untyped) =
  ## Call `publishBody` when an assertion matching `pattern` is published to `ds` and
  ## call `retractBody` on retraction. Assertions that match `pattern` but are not
  ## convertable to the arguments of `publishBody` are silently discarded.
  ## 
  ## The following symbols are injected into the scope of both bodies:
  ## - `turn` - active turn at entry of `publishBody`
  ## - `bindings` - raw Preserves sequence that matched `pattern`
  ## - `duringHandle` - dataspace handle of the assertion that triggered `publishBody`
  let
    callbackProc = wrapDuringHandler(publishBody, retractBody)
    callbackSym = callbackProc[0]
  result = quote do:
    `callbackProc`
    discard observe(`turn`, `ds`, `pattern`, during(`callbackSym`))

macro during*(turn: Turn; ds: Ref; pattern: Pattern; publishBody: untyped) =
  ## Variant of `during` without a retract body.
  let
    callbackProc = wrapDuringHandler(publishBody, nil)
    callbackSym = callbackProc[0]
  result = quote do:
    `callbackProc`
    discard observe(`turn`, `ds`, `pattern`, during(`callbackSym`))
