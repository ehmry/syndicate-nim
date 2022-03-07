# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  StateKind* {.pure.} = enum
    `started`, `ready`, `failed`, `complete`, `userDefined`
  StateUserDefined*[E] = Preserve[E]
  `State`*[E] {.preservesOr.} = ref object
    case orKind*: StateKind
    of StateKind.`started`:
      
    of StateKind.`ready`:
      
    of StateKind.`failed`:
      
    of StateKind.`complete`:
      
    of StateKind.`userDefined`:
      
  
  ServiceObject*[E] {.preservesRecord: "service-object".} = ref object
  
  RequireService*[E] {.preservesRecord: "require-service".} = ref object
  
  RestartService*[E] {.preservesRecord: "restart-service".} = ref object
  
  RunService*[E] {.preservesRecord: "run-service".} = ref object
  
  ServiceState*[E] {.preservesRecord: "service-state".} = ref object
  
  ServiceDependency*[E] {.preservesRecord: "depends-on".} = ref object
  
proc `$`*[E](x: State[E] | ServiceObject[E] | RequireService[E] |
    RestartService[E] |
    RunService[E] |
    ServiceState[E] |
    ServiceDependency[E]): string =
  `$`(toPreserve(x, E))

proc encode*[E](x: State[E] | ServiceObject[E] | RequireService[E] |
    RestartService[E] |
    RunService[E] |
    ServiceState[E] |
    ServiceDependency[E]): seq[byte] =
  encode(toPreserve(x, E))
