# SPDX-License-Identifier: MIT

import
  preserves

type
  CreditAmountKind* {.pure.} = enum
    `count`, `unbounded`
  `CreditAmount`* {.preservesOr.} = object
    case orKind*: CreditAmountKind
    of CreditAmountKind.`count`:
      
    of CreditAmountKind.`unbounded`:
      
  
  StreamError* {.preservesRecord: "error".} = object
  
  StreamListenerError* {.preservesRecord: "stream-listener-error".} = object
  
  StreamConnection* {.preservesRecord: "stream-connection".} = object
  
  `LineMode`* {.preservesOr, pure.} = enum
    `lf`, `crlf`
  SourceKind* {.pure.} = enum
    `sink`, `StreamError`, `credit`
  SourceSink* {.preservesRecord: "sink".} = object
  
  SourceCredit* {.preservesRecord: "credit".} = object
  
  `Source`* {.acyclic, preservesOr.} = ref object
    case orKind*: SourceKind
    of SourceKind.`sink`:
      
    of SourceKind.`StreamError`:
      
    of SourceKind.`credit`:
      
  
  SinkKind* {.pure.} = enum
    `source`, `StreamError`, `data`, `eof`
  SinkSource* {.preservesRecord: "source".} = object
  
  SinkData* {.preservesRecord: "data".} = object
  
  SinkEof* {.preservesRecord: "eof".} = object
  `Sink`* {.acyclic, preservesOr.} = ref object
    case orKind*: SinkKind
    of SinkKind.`source`:
      
    of SinkKind.`StreamError`:
      
    of SinkKind.`data`:
      
    of SinkKind.`eof`:
      
  
  StreamListenerReady* {.preservesRecord: "stream-listener-ready".} = object
  
  ModeKind* {.pure.} = enum
    `bytes`, `lines`, `packet`, `object`
  ModePacket* {.preservesRecord: "packet".} = object
  
  ModeObject* {.preservesRecord: "object".} = object
  
  `Mode`* {.preservesOr.} = object
    case orKind*: ModeKind
    of ModeKind.`bytes`:
      
    of ModeKind.`lines`:
      
    of ModeKind.`packet`:
      
    of ModeKind.`object`:
      
  
proc `$`*(x: CreditAmount | StreamError | StreamListenerError | StreamConnection |
    Source |
    Sink |
    StreamListenerReady |
    Mode): string =
  `$`(toPreserves(x))

proc encode*(x: CreditAmount | StreamError | StreamListenerError |
    StreamConnection |
    Source |
    Sink |
    StreamListenerReady |
    Mode): seq[byte] =
  encode(toPreserves(x))
