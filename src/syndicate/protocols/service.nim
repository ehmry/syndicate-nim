# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  StateKind* {.pure.} = enum
    `started`, `ready`, `failed`, `complete`, `userDefined`
  StateUserDefined*[Cap] = Preserve[Cap]
  `State`*[Cap] {.preservesOr.} = object
    case orKind*: StateKind
    of StateKind.`started`:
      
    of StateKind.`ready`:
      
    of StateKind.`failed`:
      
    of StateKind.`complete`:
      
    of StateKind.`userDefined`:
      
  
  ServiceObject*[Cap] {.preservesRecord: "service-object".} = object
  
  RequireService*[Cap] {.preservesRecord: "require-service".} = object
  
  RestartService*[Cap] {.preservesRecord: "restart-service".} = object
  
  RunService*[Cap] {.preservesRecord: "run-service".} = object
  
  ServiceState*[Cap] {.preservesRecord: "service-state".} = object
  
  ServiceDependency*[Cap] {.preservesRecord: "depends-on".} = object
  
proc `$`*[Cap](x: State[Cap] | ServiceObject[Cap] | RequireService[Cap] |
    RestartService[Cap] |
    RunService[Cap] |
    ServiceState[Cap] |
    ServiceDependency[Cap]): string =
  `$`(toPreserve(x, Cap))

proc encode*[Cap](x: State[Cap] | ServiceObject[Cap] | RequireService[Cap] |
    RestartService[Cap] |
    RunService[Cap] |
    ServiceState[Cap] |
    ServiceDependency[Cap]): seq[byte] =
  encode(toPreserve(x, Cap))
