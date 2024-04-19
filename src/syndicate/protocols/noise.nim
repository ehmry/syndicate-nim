# SPDX-License-Identifier: MIT

import
  preserves, std / options

type
  NoiseDescriptionDetail* = NoiseServiceSpec
  NoisePreSharedKeysKind* {.pure.} = enum
    `present`, `invalid`, `absent`
  NoisePreSharedKeysPresent* {.preservesDictionary.} = object
  
  NoisePreSharedKeysInvalid* {.preservesDictionary.} = object
  
  NoisePreSharedKeysAbsent* {.preservesDictionary.} = object
  `NoisePreSharedKeys`* {.preservesOr.} = object
    case orKind*: NoisePreSharedKeysKind
    of NoisePreSharedKeysKind.`present`:
      
    of NoisePreSharedKeysKind.`invalid`:
      
    of NoisePreSharedKeysKind.`absent`:
      
  
  SecretKeyFieldKind* {.pure.} = enum
    `present`, `invalid`, `absent`
  SecretKeyFieldPresent* {.preservesDictionary.} = object
  
  SecretKeyFieldInvalid* {.preservesDictionary.} = object
  
  SecretKeyFieldAbsent* {.preservesDictionary.} = object
  `SecretKeyField`* {.preservesOr.} = object
    case orKind*: SecretKeyFieldKind
    of SecretKeyFieldKind.`present`:
      
    of SecretKeyFieldKind.`invalid`:
      
    of SecretKeyFieldKind.`absent`:
      
  
  SessionItemKind* {.pure.} = enum
    `Initiator`, `Packet`
  `SessionItem`* {.preservesOr.} = object
    case orKind*: SessionItemKind
    of SessionItemKind.`Initiator`:
      
    of SessionItemKind.`Packet`:
      
  
  NoiseProtocolKind* {.pure.} = enum
    `present`, `invalid`, `absent`
  NoiseProtocolPresent* {.preservesDictionary.} = object
  
  NoiseProtocolInvalid* {.preservesDictionary.} = object
  
  NoiseProtocolAbsent* {.preservesDictionary.} = object
  `NoiseProtocol`* {.preservesOr.} = object
    case orKind*: NoiseProtocolKind
    of NoiseProtocolKind.`present`:
      
    of NoiseProtocolKind.`invalid`:
      
    of NoiseProtocolKind.`absent`:
      
  
  NoisePathStepDetail* = NoiseSpec
  NoiseServiceSpecKey* = seq[byte]
  NoiseServiceSpecPreSharedKeys* = Option[Value]
  NoiseServiceSpecProtocol* = Option[Value]
  NoiseServiceSpecSecretKey* = Option[Value]
  `NoiseServiceSpec`* {.preservesDictionary.} = object
  
  ServiceSelector* = Value
  Initiator* {.preservesRecord: "initiator".} = object
  
  NoiseStepDetail* = ServiceSelector
  NoiseSpecKey* = seq[byte]
  NoiseSpecPreSharedKeys* = Option[Value]
  NoiseSpecProtocol* = Option[Value]
  `NoiseSpec`* {.preservesDictionary.} = object
  
  PacketKind* {.pure.} = enum
    `complete`, `fragmented`
  `Packet`* {.preservesOr.} = object
    case orKind*: PacketKind
    of PacketKind.`complete`:
      
    of PacketKind.`fragmented`:
      
  
proc `$`*(x: NoiseDescriptionDetail | NoisePreSharedKeys | SecretKeyField |
    SessionItem |
    NoiseProtocol |
    NoisePathStepDetail |
    NoiseServiceSpec |
    Initiator |
    NoiseSpec |
    Packet): string =
  `$`(toPreserves(x))

proc encode*(x: NoiseDescriptionDetail | NoisePreSharedKeys | SecretKeyField |
    SessionItem |
    NoiseProtocol |
    NoisePathStepDetail |
    NoiseServiceSpec |
    Initiator |
    NoiseSpec |
    Packet): seq[byte] =
  encode(toPreserves(x))
