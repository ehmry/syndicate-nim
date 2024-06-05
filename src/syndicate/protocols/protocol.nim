# SPDX-License-Identifier: MIT

import
  preserves

type
  Error* {.preservesRecord: "error".} = object
  
  Turn* = seq[TurnEvent]
  Message* {.preservesRecord: "M".} = object
  
  Retract* {.preservesRecord: "R".} = object
  
  Assert* {.preservesRecord: "A".} = object
  
  Extension* = Value
  Sync* {.preservesRecord: "S".} = object
  
  TurnEvent* {.preservesTuple.} = object
  
  Oid* = BiggestInt
  Assertion* = Value
  Handle* = BiggestInt
  PacketKind* {.pure.} = enum
    `Nop`, `Turn`, `Error`, `Extension`
  `Packet`* {.preservesOr.} = object
    case orKind*: PacketKind
    of PacketKind.`Nop`:
      
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
      
  
proc `$`*(x: Error | Turn | Message | Retract | Assert | Sync | TurnEvent | Oid |
    Handle |
    Packet |
    Event): string =
  `$`(toPreserves(x))

proc encode*(x: Error | Turn | Message | Retract | Assert | Sync | TurnEvent |
    Oid |
    Handle |
    Packet |
    Event): seq[byte] =
  encode(toPreserves(x))
