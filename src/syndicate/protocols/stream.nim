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
  
  StreamListenerError*[E] {.preservesRecord: "stream-listener-error".} = ref object
  
  StreamConnection*[E] {.preservesRecord: "stream-connection".} = ref object
  
  `LineMode`* {.preservesOr.} = enum
    `lf`, `crlf`
  SourceKind* {.pure.} = enum
    `sink`, `StreamError`, `credit`
  SourceSink*[E] {.preservesRecord: "sink".} = ref object
  
  SourceCredit*[E] {.preservesRecord: "credit".} = ref object
  
  `Source`*[E] {.preservesOr.} = ref object
    case orKind*: SourceKind
    of SourceKind.`sink`:
      
    of SourceKind.`StreamError`:
      
    of SourceKind.`credit`:
      
  
  SinkKind* {.pure.} = enum
    `source`, `StreamError`, `data`, `eof`
  SinkSource*[E] {.preservesRecord: "source".} = ref object
  
  SinkData*[E] {.preservesRecord: "data".} = ref object
  
  SinkEof* {.preservesRecord: "eof".} = object
  `Sink`*[E] {.preservesOr.} = ref object
    case orKind*: SinkKind
    of SinkKind.`source`:
      
    of SinkKind.`StreamError`:
      
    of SinkKind.`data`:
      
    of SinkKind.`eof`:
      
  
  StreamListenerReady*[E] {.preservesRecord: "stream-listener-ready".} = ref object
  
  ModeKind* {.pure.} = enum
    `bytes`, `lines`, `packet`, `object`
  ModePacket* {.preservesRecord: "packet".} = object
  
  ModeObject*[E] {.preservesRecord: "object".} = ref object
  
  `Mode`*[E] {.preservesOr.} = ref object
    case orKind*: ModeKind
    of ModeKind.`bytes`:
      
    of ModeKind.`lines`:
      
    of ModeKind.`packet`:
      
    of ModeKind.`object`:
      
  
proc `$`*[E](x: StreamListenerError[E] | StreamConnection[E] | Source[E] |
    Sink[E] |
    StreamListenerReady[E] |
    Mode[E]): string =
  `$`(toPreserve(x, E))

proc encode*[E](x: StreamListenerError[E] | StreamConnection[E] | Source[E] |
    Sink[E] |
    StreamListenerReady[E] |
    Mode[E]): seq[byte] =
  encode(toPreserve(x, E))

proc `$`*(x: CreditAmount | StreamError): string =
  `$`(toPreserve(x))

proc encode*(x: CreditAmount | StreamError): seq[byte] =
  encode(toPreserve(x))
