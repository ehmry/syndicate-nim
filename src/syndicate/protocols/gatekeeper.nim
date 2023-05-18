# SPDX-License-Identifier: MIT

import
  preserves

type
  Bind*[Cap] {.preservesRecord: "bind".} = object
  
  Route*[Cap] {.preservesRecord: "route".} = object
  
  BindObserverKind* {.pure.} = enum
    `present`, `absent`
  BindObserverPresent*[Cap] = Cap
  `BindObserver`*[Cap] {.preservesOr.} = object
    case orKind*: BindObserverKind
    of BindObserverKind.`present`:
      
    of BindObserverKind.`absent`:
      
  
  TransportConnection*[Cap] {.preservesRecord: "connect-transport".} = object
  
  Step*[Cap] = Preserve[Cap]
  ResolvedPathStep*[Cap] {.preservesRecord: "path-step".} = object
  
  BoundKind* {.pure.} = enum
    `bound`, `Rejected`
  BoundBound*[Cap] {.preservesRecord: "bound".} = object
  
  `Bound`*[Cap] {.preservesOr.} = object
    case orKind*: BoundKind
    of BoundKind.`bound`:
      
    of BoundKind.`Rejected`:
      
  
  ForceDisconnect* {.preservesRecord: "force-disconnect".} = object
  Description*[Cap] = Preserve[Cap]
  Rejected*[Cap] {.preservesRecord: "rejected".} = object
  
  Resolve*[Cap] {.preservesRecord: "resolve".} = object
  
  ResolvedKind* {.pure.} = enum
    `accepted`, `Rejected`
  ResolvedAccepted*[Cap] {.preservesRecord: "accepted".} = object
  
  `Resolved`*[Cap] {.preservesOr.} = object
    case orKind*: ResolvedKind
    of ResolvedKind.`accepted`:
      
    of ResolvedKind.`Rejected`:
      
  
  TransportControl* = ForceDisconnect
  ResolvePath*[Cap] {.preservesRecord: "resolve-path".} = object
  
  PathStep*[Cap] = Preserve[Cap]
proc `$`*[Cap](x: Bind[Cap] | Route[Cap] | BindObserver[Cap] |
    TransportConnection[Cap] |
    ResolvedPathStep[Cap] |
    Bound[Cap] |
    Rejected[Cap] |
    Resolve[Cap] |
    Resolved[Cap] |
    ResolvePath[Cap]): string =
  `$`(toPreserve(x, Cap))

proc encode*[Cap](x: Bind[Cap] | Route[Cap] | BindObserver[Cap] |
    TransportConnection[Cap] |
    ResolvedPathStep[Cap] |
    Bound[Cap] |
    Rejected[Cap] |
    Resolve[Cap] |
    Resolved[Cap] |
    ResolvePath[Cap]): seq[byte] =
  encode(toPreserve(x, Cap))

proc `$`*(x: ForceDisconnect | TransportControl): string =
  `$`(toPreserve(x))

proc encode*(x: ForceDisconnect | TransportControl): seq[byte] =
  encode(toPreserve(x))
