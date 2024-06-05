# SPDX-License-Identifier: MIT

import
  preserves

type
  Bind* {.preservesRecord: "bind".} = object
  
  Route* {.preservesRecord: "route".} = object
  
  BindObserverKind* {.pure.} = enum
    `absent`, `present`
  `BindObserver`* {.preservesOr.} = object
    case orKind*: BindObserverKind
    of BindObserverKind.`absent`:
      
    of BindObserverKind.`present`:
      
  
  TransportConnection* {.preservesRecord: "connect-transport".} = object
  
  Step* = Value
  ResolvedPathStep* {.preservesRecord: "path-step".} = object
  
  BoundKind* {.pure.} = enum
    `Rejected`, `bound`
  BoundBound* {.preservesRecord: "bound".} = object
  
  `Bound`* {.preservesOr.} = object
    case orKind*: BoundKind
    of BoundKind.`Rejected`:
      
    of BoundKind.`bound`:
      
  
  ForceDisconnect* {.preservesRecord: "force-disconnect".} = object
  Description* = Value
  Rejected* {.preservesRecord: "rejected".} = object
  
  Resolve* {.preservesRecord: "resolve".} = object
  
  ResolvedKind* {.pure.} = enum
    `Rejected`, `accepted`
  ResolvedAccepted* {.preservesRecord: "accepted".} = object
  
  `Resolved`* {.preservesOr.} = object
    case orKind*: ResolvedKind
    of ResolvedKind.`Rejected`:
      
    of ResolvedKind.`accepted`:
      
  
  TransportControl* = ForceDisconnect
  ResolvePath* {.preservesRecord: "resolve-path".} = object
  
  PathStep* = Value
proc `$`*(x: Bind | Route | BindObserver | TransportConnection |
    ResolvedPathStep |
    Bound |
    ForceDisconnect |
    Rejected |
    Resolve |
    Resolved |
    TransportControl |
    ResolvePath): string =
  `$`(toPreserves(x))

proc encode*(x: Bind | Route | BindObserver | TransportConnection |
    ResolvedPathStep |
    Bound |
    ForceDisconnect |
    Rejected |
    Resolve |
    Resolved |
    TransportControl |
    ResolvePath): seq[byte] =
  encode(toPreserves(x))
