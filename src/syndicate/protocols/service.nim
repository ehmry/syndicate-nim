# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  `State`* {.preservesOr.} = enum
    `started`, `ready`, `failed`, `complete`
  ServiceObject*[E] {.preservesRecord: "service-object".} = ref object
  
  CoreService*[E] {.preservesRecord: "core-service".} = ref object
  
  RequireService*[E] {.preservesRecord: "require-service".} = ref object
  
  RunService*[E] {.preservesRecord: "run-service".} = ref object
  
  ServiceState*[E] {.preservesRecord: "service-state".} = ref object
  
  ServiceDependency*[E] {.preservesRecord: "depends-on".} = ref object
  
proc `$`*[E](x: ServiceObject[E] | CoreService[E] | RequireService[E] |
    RunService[E] |
    ServiceState[E] |
    ServiceDependency[E]): string =
  `$`(toPreserve(x, E))

proc encode*[E](x: ServiceObject[E] | CoreService[E] | RequireService[E] |
    RunService[E] |
    ServiceState[E] |
    ServiceDependency[E]): seq[byte] =
  encode(toPreserve(x, E))
