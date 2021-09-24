# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  Error*[E] {.preservesRecord: "error".} = ref object
  
  Turn*[E] = seq[TurnEvent[E]]
  Message*[E] {.preservesRecord: "message".} = ref object
  
  Retract* {.preservesRecord: "retract".} = object
  
  Assert*[E] {.preservesRecord: "assert".} = ref object
  
  Sync*[E] {.preservesRecord: "sync".} = ref object
  
  TurnEvent*[E] {.preservesTuple.} = ref object
  
  Oid* = BiggestInt
  Assertion*[E] = Preserve[E]
  Handle* = BiggestInt
  PacketKind* {.pure.} = enum
    `Turn`, `Error`
  `Packet`*[E] {.preservesOr.} = ref object
    case orKind*: PacketKind
    of PacketKind.`Turn`:
      
    of PacketKind.`Error`:
      
  
  EventKind* {.pure.} = enum
    `Assert`, `Retract`, `Message`, `Sync`
  `Event`*[E] {.preservesOr.} = ref object
    case orKind*: EventKind
    of EventKind.`Assert`:
      
    of EventKind.`Retract`:
      
    of EventKind.`Message`:
      
    of EventKind.`Sync`:
      
  
proc `$`*[E](x: Error[E] | Turn[E] | Message[E] | Assert[E] | Sync[E] |
    TurnEvent[E] |
    Packet[E] |
    Event[E]): string =
  `$`(toPreserve(x, E))

proc encode*[E](x: Error[E] | Turn[E] | Message[E] | Assert[E] | Sync[E] |
    TurnEvent[E] |
    Packet[E] |
    Event[E]): seq[byte] =
  encode(toPreserve(x, E))

proc `$`*(x: Retract | Oid | Handle): string =
  `$`(toPreserve(x))

proc encode*(x: Retract | Oid | Handle): seq[byte] =
  encode(toPreserve(x))
