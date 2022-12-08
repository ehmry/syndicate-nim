# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  CreditAmountKind* {.pure.} = enum
    `count`, `unbounded`
  CreditAmountCount* = BiggestInt
  `CreditAmount`* {.preservesOr.} = object
    case orKind*: CreditAmountKind
    of CreditAmountKind.`count`:
      
    of CreditAmountKind.`unbounded`:
      
  
  StreamError* {.preservesRecord: "error".} = object
  
  StreamListenerError*[Cap] {.preservesRecord: "stream-listener-error".} = object
  
  StreamConnection*[Cap] {.preservesRecord: "stream-connection".} = object
  
  `LineMode`* {.preservesOr, pure.} = enum
    `lf`, `crlf`
  SourceKind* {.pure.} = enum
    `sink`, `StreamError`, `credit`
  SourceSink*[Cap] {.preservesRecord: "sink".} = object
  
  SourceCredit*[Cap] {.preservesRecord: "credit".} = object
  
  `Source`*[Cap] {.preservesOr.} = object
    case orKind*: SourceKind
    of SourceKind.`sink`:
      
    of SourceKind.`StreamError`:
      
    of SourceKind.`credit`:
      
  
  SinkKind* {.pure.} = enum
    `source`, `StreamError`, `data`, `eof`
  SinkSource*[Cap] {.preservesRecord: "source".} = object
  
  SinkData*[Cap] {.preservesRecord: "data".} = object
  
  SinkEof* {.preservesRecord: "eof".} = object
  `Sink`*[Cap] {.preservesOr.} = object
    case orKind*: SinkKind
    of SinkKind.`source`:
      
    of SinkKind.`StreamError`:
      
    of SinkKind.`data`:
      
    of SinkKind.`eof`:
      
  
  StreamListenerReady*[Cap] {.preservesRecord: "stream-listener-ready".} = object
  
  ModeKind* {.pure.} = enum
    `bytes`, `lines`, `packet`, `object`
  ModePacket* {.preservesRecord: "packet".} = object
  
  ModeObject*[Cap] {.preservesRecord: "object".} = object
  
  `Mode`*[Cap] {.preservesOr.} = object
    case orKind*: ModeKind
    of ModeKind.`bytes`:
      
    of ModeKind.`lines`:
      
    of ModeKind.`packet`:
      
    of ModeKind.`object`:
      
  
proc `$`*[Cap](x: StreamListenerError[Cap] | StreamConnection[Cap] | Source[Cap] |
    Sink[Cap] |
    StreamListenerReady[Cap] |
    Mode[Cap]): string =
  `$`(toPreserve(x, Cap))

proc encode*[Cap](x: StreamListenerError[Cap] | StreamConnection[Cap] |
    Source[Cap] |
    Sink[Cap] |
    StreamListenerReady[Cap] |
    Mode[Cap]): seq[byte] =
  encode(toPreserve(x, Cap))

proc `$`*(x: CreditAmount | StreamError): string =
  `$`(toPreserve(x))

proc encode*(x: CreditAmount | StreamError): seq[byte] =
  encode(toPreserve(x))
