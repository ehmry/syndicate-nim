# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  Error* {.preservesRecord: "error".} = object
  
  Turn* = seq[TurnEvent]
  Message* {.preservesRecord: "message".} = object
  
  Retract* {.preservesRecord: "retract".} = object
  
  Assert* {.preservesRecord: "assert".} = object
  
  Extension* {.preservesRecord: "label".} = object
  
  Sync* {.preservesRecord: "sync".} = object
  
  TurnEvent* {.preservesTuple.} = object
  
  Oid* = BiggestInt
  Assertion* = Preserve[void]
  Handle* = BiggestInt
  PacketKind* {.pure.} = enum
    `Turn`, `Error`, `Extension`
  `Packet`* {.preservesOr.} = object
    case orKind*: PacketKind
    of PacketKind.`Turn`:
      
    of PacketKind.`Error`:
      
    of PacketKind.`Extension`:
      
  
  EventKind* {.pure.} = enum
    `Assert`, `Retract`, `Message`, `Sync`
  `Event`* {.preservesOr.} = object
    case orKind*: EventKind
    of EventKind.`Assert`:
      
    of EventKind.`Retract`:
      
    of EventKind.`Message`:
      
    of EventKind.`Sync`:
      
  
proc `$`*(x: Error | Turn | Message | Retract | Assert | Extension | Sync |
    TurnEvent |
    Oid |
    Handle |
    Packet |
    Event): string =
  `$`(toPreserve(x))

proc encode*(x: Error | Turn | Message | Retract | Assert | Extension | Sync |
    TurnEvent |
    Oid |
    Handle |
    Packet |
    Event): seq[byte] =
  encode(toPreserve(x))
