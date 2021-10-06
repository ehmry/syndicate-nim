# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  PacketKind* {.pure.} = enum
    `Turn`, `Error`
  Packet*[E = void] = ref object ## ``/ Turn / Error``
    case kind*: PacketKind
    of PacketKind.`Turn`:
      
    of PacketKind.`Error`:
      
  
  Error*[E = void] {.record: "error".} = ref object ## ``<error @message string @detail any>``
  
  Assertion*[E = void] = Preserve[E] ## ``any``
  Handle* = distinct BiggestInt ## ``int``
  EventKind* {.pure.} = enum
    `Assert`, `Retract`, `Message`, `Sync`
  Event*[E = void] = ref object ## ``/ Assert / Retract / Message / Sync``
    case kind*: EventKind
    of EventKind.`Assert`:
      
    of EventKind.`Retract`:
      
    of EventKind.`Message`:
      
    of EventKind.`Sync`:
      
  
  Oid* = distinct BiggestInt ## ``int``
  Turn*[E = void] = seq[TurnEvent[E]] ## ``[ TurnEvent ... ]``
  TurnEvent*[E = void] = tuple[`oid`: Oid, `event`: Event[E]] ## ``[@oid Oid @event Event]``
  Assert*[E = void] {.record: "assert".} = ref object ## ``<assert @assertion Assertion @handle Handle>``
  
  Retract* {.record: "retract".} = ref object ## ``<retract @handle Handle>``
  
  Message*[E = void] {.record: "message".} = ref object ## ``<message @body Assertion>``
  
  Sync* {.record: "sync".} = ref object ## ``<sync @peer #!#t>``
  
proc toPreserveHook*(v: Packet; E: typedesc): Preserve[E] =
  case v.kind
  of PacketKind.`Turn`:
    toPreserve(v.`turn`, E)
  of PacketKind.`Error`:
    toPreserve(v.`error`, E)

proc fromPreserveHook*[E](v: var Packet; pr: Preserve[E]): bool =
  if false:
    discard
  elif isRecord(pr) and pr.label.isSymbol("Error"):
    v = Packet(kind: PacketKind.`Error`)
    result = fromPreserve(v.`error`, pr)

proc `error`*[E = void](`message`: string | Preserve[E]; `detail`: Preserve[E]): Preserve[
    E] =
  ## Preserves constructor for ``Error``.
  initRecord[E](symbol[E]("error"), toPreserve(`message`, E),
                toPreserve(`detail`, E))

proc toPreserveHook*(`error`: Error; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("error"), toPreserve(`error`.`message`, E),
                toPreserve(`error`.`detail`, E))

proc toPreserveHook*(v: Event; E: typedesc): Preserve[E] =
  case v.kind
  of EventKind.`Assert`:
    toPreserve(v.`assert`, E)
  of EventKind.`Retract`:
    toPreserve(v.`retract`, E)
  of EventKind.`Message`:
    toPreserve(v.`message`, E)
  of EventKind.`Sync`:
    toPreserve(v.`sync`, E)

proc fromPreserveHook*[E](v: var Event; pr: Preserve[E]): bool =
  if isRecord(pr) and pr.label.isSymbol("Assert"):
    v = Event(kind: EventKind.`Assert`)
    result = fromPreserve(v.`assert`, pr)
  elif isRecord(pr) and pr.label.isSymbol("Retract"):
    v = Event(kind: EventKind.`Retract`)
    result = fromPreserve(v.`retract`, pr)
  elif isRecord(pr) and pr.label.isSymbol("Message"):
    v = Event(kind: EventKind.`Message`)
    result = fromPreserve(v.`message`, pr)
  elif isRecord(pr) and pr.label.isSymbol("Sync"):
    v = Event(kind: EventKind.`Sync`)
    result = fromPreserve(v.`sync`, pr)

proc toPreserveHook*(`turnevent`: TurnEvent; E: typedesc): Preserve[E] =
  Preserve[E](kind: pkSequence, sequence: @[toPreserve(`turnevent`.`oid`, E),
      toPreserve(`turnevent`.`event`, E)])

proc `assert`*[E = void](`assertion`: Assertion | Preserve[E];
                         `handle`: Handle | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``Assert``.
  initRecord[E](symbol[E]("assert"), toPreserve(`assertion`, E),
                toPreserve(`handle`, E))

proc toPreserveHook*(`assert`: Assert; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("assert"), toPreserve(`assert`.`assertion`, E),
                toPreserve(`assert`.`handle`, E))

proc `retract`*[E = void](`handle`: Handle | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``Retract``.
  initRecord[E](symbol[E]("retract"), toPreserve(`handle`, E))

proc toPreserveHook*(`retract`: Retract; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("retract"), toPreserve(`retract`.`handle`, E))

proc `message`*[E = void](`body`: Assertion | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``Message``.
  initRecord[E](symbol[E]("message"), toPreserve(`body`, E))

proc toPreserveHook*(`message`: Message; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("message"), toPreserve(`message`.`body`, E))

proc `sync`*[E = void](`peer`: Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``Sync``.
  initRecord[E](symbol[E]("sync"), toPreserve(`peer`, E))

proc toPreserveHook*(`sync`: Sync; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("sync"), toPreserve(`sync`.`peer`, E))

proc `$`*[E](x: Packet[E] | Error[E] | Assertion[E] | Handle | Event[E] | Oid |
    Turn[E] |
    TurnEvent[E] |
    Assert[E] |
    Retract |
    Message[E] |
    Sync): string =
  `$`(toPreserve(x, E))

proc `encode`*[E](x: Packet[E] | Error[E] | Assertion[E] | Handle | Event[E] |
    Oid |
    Turn[E] |
    TurnEvent[E] |
    Assert[E] |
    Retract |
    Message[E] |
    Sync): seq[byte] =
  encode(toPreserve(x, E))
