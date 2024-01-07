# SPDX-License-Identifier: MIT

import
  preserves, std / tables

type
  HostPatternKind* {.pure.} = enum
    `host`, `any`
  `HostPattern`* {.preservesOr.} = object
    case orKind*: HostPatternKind
    of HostPatternKind.`host`:
      
    of HostPatternKind.`any`:
      
  
  HttpListener* {.preservesRecord: "http-listener".} = object
  
  MethodPatternKind* {.pure.} = enum
    `any`, `specific`
  `MethodPattern`* {.preservesOr.} = object
    case orKind*: MethodPatternKind
    of MethodPatternKind.`any`:
      
    of MethodPatternKind.`specific`:
      
  
  MimeType* = Symbol
  QueryValueKind* {.pure.} = enum
    `string`, `file`
  QueryValueFile* {.preservesRecord: "file".} = object
  
  `QueryValue`* {.preservesOr.} = object
    case orKind*: QueryValueKind
    of QueryValueKind.`string`:
      
    of QueryValueKind.`file`:
      
  
  HttpRequest* {.preservesRecord: "http-request".} = object
  
  RequestBodyKind* {.pure.} = enum
    `present`, `absent`
  `RequestBody`* {.preservesOr.} = object
    case orKind*: RequestBodyKind
    of RequestBodyKind.`present`:
      
    of RequestBodyKind.`absent`:
      
  
  Headers* = Table[Symbol, string]
  HttpResponseKind* {.pure.} = enum
    `status`, `header`, `chunk`, `done`
  HttpResponseStatus* {.preservesRecord: "status".} = object
  
  HttpResponseHeader* {.preservesRecord: "header".} = object
  
  HttpResponseChunk* {.preservesRecord: "chunk".} = object
  
  HttpResponseDone* {.preservesRecord: "done".} = object
  
  `HttpResponse`* {.preservesOr.} = object
    case orKind*: HttpResponseKind
    of HttpResponseKind.`status`:
      
    of HttpResponseKind.`header`:
      
    of HttpResponseKind.`chunk`:
      
    of HttpResponseKind.`done`:
      
  
  HttpService* {.preservesRecord: "http-service".} = object
  
  HttpBinding* {.preservesRecord: "http-bind".} = object
  
  HttpContext* {.preservesRecord: "request".} = object
  
  PathPatternElementKind* {.pure.} = enum
    `label`, `wildcard`, `rest`
  `PathPatternElement`* {.preservesOr.} = object
    case orKind*: PathPatternElementKind
    of PathPatternElementKind.`label`:
      
    of PathPatternElementKind.`wildcard`:
      
    of PathPatternElementKind.`rest`:
      
  
  ChunkKind* {.pure.} = enum
    `string`, `bytes`
  `Chunk`* {.preservesOr.} = object
    case orKind*: ChunkKind
    of ChunkKind.`string`:
      
    of ChunkKind.`bytes`:
      
  
  PathPattern* = seq[PathPatternElement]
proc `$`*(x: HostPattern | HttpListener | MethodPattern | MimeType | QueryValue |
    HttpRequest |
    RequestBody |
    Headers |
    HttpResponse |
    HttpService |
    HttpBinding |
    HttpContext |
    PathPatternElement |
    Chunk |
    PathPattern): string =
  `$`(toPreserves(x))

proc encode*(x: HostPattern | HttpListener | MethodPattern | MimeType |
    QueryValue |
    HttpRequest |
    RequestBody |
    Headers |
    HttpResponse |
    HttpService |
    HttpBinding |
    HttpContext |
    PathPatternElement |
    Chunk |
    PathPattern): seq[byte] =
  encode(toPreserves(x))
