# SPDX-License-Identifier: MIT

import
  preserves, std / tables

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
  `NoiseServiceSpec`* = Table[Symbol, Value]
  ServiceSelector* = Value
  NoiseStepDetail* = ServiceSelector
  `NoiseSpec`* = Table[Symbol, Value]
  PacketKind* {.pure.} = enum
    `complete`, `fragmented`
  PacketComplete* = seq[byte]
  PacketFragmented* = seq[seq[byte]]
  `Packet`* {.preservesOr.} = object
    case orKind*: PacketKind
    of PacketKind.`complete`:
      
    of PacketKind.`fragmented`:
      
  
proc `$`*(x: NoiseDescriptionDetail | NoisePreSharedKeys | SecretKeyField |
    NoiseProtocol |
    NoisePathStepDetail |
    NoiseServiceSpec |
    NoiseSpec |
    Packet): string =
  `$`(toPreserves(x))

proc encode*(x: NoiseDescriptionDetail | NoisePreSharedKeys | SecretKeyField |
    NoiseProtocol |
    NoisePathStepDetail |
    NoiseServiceSpec |
    NoiseSpec |
    Packet): seq[byte] =
  encode(toPreserves(x))
