# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  ServiceStarted*[E] {.preservesRecord: "service-started".} = ref object
  
  ServiceMilestone*[E] {.preservesRecord: "service-milestone".} = ref object
  
  RequireService*[E] {.preservesRecord: "require-service".} = ref object
  
  DependeeKind* {.pure.} = enum
    `ServiceStarted`, `ServiceRunning`
  `Dependee`*[E] {.preservesOr.} = ref object
    case orKind*: DependeeKind
    of DependeeKind.`ServiceStarted`:
      
    of DependeeKind.`ServiceRunning`:
      
  
  RunService*[E] {.preservesRecord: "run-service".} = ref object
  
  ServiceRunning*[E] {.preservesRecord: "service-running".} = ref object
  
  ServiceDependency*[E] {.preservesRecord: "depends-on".} = ref object
  
proc `$`*[E](x: ServiceStarted[E] | ServiceMilestone[E] | RequireService[E] |
    Dependee[E] |
    RunService[E] |
    ServiceRunning[E] |
    ServiceDependency[E]): string =
  `$`(toPreserve(x, E))

proc encode*[E](x: ServiceStarted[E] | ServiceMilestone[E] | RequireService[E] |
    Dependee[E] |
    RunService[E] |
    ServiceRunning[E] |
    ServiceDependency[E]): seq[byte] =
  encode(toPreserve(x, E))
