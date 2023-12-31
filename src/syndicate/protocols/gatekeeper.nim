# SPDX-License-Identifier: MIT

import
  preserves

type
  Bind* {.preservesRecord: "bind".} = object
  
  Route* {.preservesRecord: "route".} = object
  
  BindObserverKind* {.pure.} = enum
    `present`, `absent`
  BindObserverPresent* = Bound
  `BindObserver`* {.preservesOr.} = object
    case orKind*: BindObserverKind
    of BindObserverKind.`present`:
      
    of BindObserverKind.`absent`:
      
  
  TransportConnection* {.preservesRecord: "connect-transport".} = object
  
  Step* = Value
  ResolvedPathStep* {.preservesRecord: "path-step".} = object
  
  BoundKind* {.pure.} = enum
    `bound`, `Rejected`
  BoundBound* {.preservesRecord: "bound".} = object
  
  `Bound`* {.preservesOr.} = object
    case orKind*: BoundKind
    of BoundKind.`bound`:
      
    of BoundKind.`Rejected`:
      
  
  ForceDisconnect* {.preservesRecord: "force-disconnect".} = object
  Description* = Value
  Rejected* {.preservesRecord: "rejected".} = object
  
  Resolve* {.preservesRecord: "resolve".} = object
  
  ResolvedKind* {.pure.} = enum
    `accepted`, `Rejected`
  ResolvedAccepted* {.preservesRecord: "accepted".} = object
  
  `Resolved`* {.preservesOr.} = object
    case orKind*: ResolvedKind
    of ResolvedKind.`accepted`:
      
    of ResolvedKind.`Rejected`:
      
  
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
