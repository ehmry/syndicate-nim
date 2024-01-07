# SPDX-License-Identifier: MIT

import
  preserves

type
  StateKind* {.pure.} = enum
    `started`, `ready`, `failed`, `complete`, `userDefined`
  `State`* {.preservesOr.} = object
    case orKind*: StateKind
    of StateKind.`started`:
      
    of StateKind.`ready`:
      
    of StateKind.`failed`:
      
    of StateKind.`complete`:
      
    of StateKind.`userDefined`:
      
  
  ServiceObject* {.preservesRecord: "service-object".} = object
  
  RequireService* {.preservesRecord: "require-service".} = object
  
  RestartService* {.preservesRecord: "restart-service".} = object
  
  RunService* {.preservesRecord: "run-service".} = object
  
  ServiceState* {.preservesRecord: "service-state".} = object
  
  ServiceDependency* {.preservesRecord: "depends-on".} = object
  
proc `$`*(x: State | ServiceObject | RequireService | RestartService |
    RunService |
    ServiceState |
    ServiceDependency): string =
  `$`(toPreserves(x))

proc encode*(x: State | ServiceObject | RequireService | RestartService |
    RunService |
    ServiceState |
    ServiceDependency): seq[byte] =
  encode(toPreserves(x))
