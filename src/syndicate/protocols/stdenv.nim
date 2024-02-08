# SPDX-License-Identifier: MIT

import
  preserves, sturdy, gatekeeper

type
  StandardTransportKind* {.pure.} = enum
    `wsUrl`, `other`
  `StandardTransport`* {.preservesOr.} = object
    case orKind*: StandardTransportKind
    of StandardTransportKind.`wsUrl`:
      
    of StandardTransportKind.`other`:
      
  
  StandardRouteKind* {.pure.} = enum
    `standard`, `general`
  StandardRouteStandard* {.preservesTuple.} = object
  
  `StandardRoute`* {.preservesOr.} = object
    case orKind*: StandardRouteKind
    of StandardRouteKind.`standard`:
      
    of StandardRouteKind.`general`:
      
  
proc `$`*(x: StandardTransport | StandardRoute): string =
  `$`(toPreserves(x))

proc encode*(x: StandardTransport | StandardRoute): seq[byte] =
  encode(toPreserves(x))
