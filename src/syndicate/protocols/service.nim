# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  RequireService*[E = void] {.record: "require-service".} = ref object ## ``<require-service @serviceName any>``
  
  RunService*[E = void] {.record: "run-service".} = ref object ## ``<run-service @serviceName any>``
  
  ServiceStarted*[E = void] {.record: "service-started".} = ref object ## ``<service-started @serviceName any>``
  
  ServiceRunning*[E = void] {.record: "service-running".} = ref object ## ``<service-running @serviceName any>``
  
  ServiceDependency*[E = void] {.record: "depends-on".} = ref object ## ``<depends-on @depender any @dependee Dependee>``
  
  DependeeKind* {.pure.} = enum
    `Servicestarted`, `Servicerunning`
  Dependee*[E = void] = ref object ## ``/ ServiceStarted / ServiceRunning``
    case kind*: DependeeKind
    of DependeeKind.`Servicestarted`:
      
    of DependeeKind.`Servicerunning`:
      
  
  ServiceMilestone*[E = void] {.record: "service-milestone".} = ref object ## ``<service-milestone @serviceName any @milestone any>``
  
proc `requireService`*[E = void](`serviceName`: Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``RequireService``.
  initRecord[E](symbol[E]("require-service"), toPreserve(`serviceName`, E))

proc toPreserveHook*(`requireservice`: RequireService; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("require-service"),
                toPreserve(`requireservice`.`serviceName`, E))

proc `runService`*[E = void](`serviceName`: Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``RunService``.
  initRecord[E](symbol[E]("run-service"), toPreserve(`serviceName`, E))

proc toPreserveHook*(`runservice`: RunService; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("run-service"),
                toPreserve(`runservice`.`serviceName`, E))

proc `serviceStarted`*[E = void](`serviceName`: Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``ServiceStarted``.
  initRecord[E](symbol[E]("service-started"), toPreserve(`serviceName`, E))

proc toPreserveHook*(`servicestarted`: ServiceStarted; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("service-started"),
                toPreserve(`servicestarted`.`serviceName`, E))

proc `serviceRunning`*[E = void](`serviceName`: Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``ServiceRunning``.
  initRecord[E](symbol[E]("service-running"), toPreserve(`serviceName`, E))

proc toPreserveHook*(`servicerunning`: ServiceRunning; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("service-running"),
                toPreserve(`servicerunning`.`serviceName`, E))

proc `serviceDependency`*[E = void](`depender`: Preserve[E];
                                    `dependee`: Dependee | Preserve[E]): Preserve[
    E] =
  ## Preserves constructor for ``ServiceDependency``.
  initRecord[E](symbol[E]("depends-on"), toPreserve(`depender`, E),
                toPreserve(`dependee`, E))

proc toPreserveHook*(`servicedependency`: ServiceDependency; E: typedesc): Preserve[
    E] =
  initRecord[E](symbol[E]("depends-on"),
                toPreserve(`servicedependency`.`depender`, E),
                toPreserve(`servicedependency`.`dependee`, E))

proc toPreserveHook*(v: Dependee; E: typedesc): Preserve[E] =
  case v.kind
  of DependeeKind.`Servicestarted`:
    toPreserve(v.`servicestarted`, E)
  of DependeeKind.`Servicerunning`:
    toPreserve(v.`servicerunning`, E)

proc fromPreserveHook*[E](v: var Dependee; pr: Preserve[E]): bool =
  if isRecord(pr) and pr.label.isSymbol("ServiceStarted"):
    v = Dependee(kind: DependeeKind.`Servicestarted`)
    result = fromPreserve(v.`servicestarted`, pr)
  elif isRecord(pr) and pr.label.isSymbol("ServiceRunning"):
    v = Dependee(kind: DependeeKind.`Servicerunning`)
    result = fromPreserve(v.`servicerunning`, pr)

proc `serviceMilestone`*[E = void](`serviceName`: Preserve[E];
                                   `milestone`: Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``ServiceMilestone``.
  initRecord[E](symbol[E]("service-milestone"), toPreserve(`serviceName`, E),
                toPreserve(`milestone`, E))

proc toPreserveHook*(`servicemilestone`: ServiceMilestone; E: typedesc): Preserve[
    E] =
  initRecord[E](symbol[E]("service-milestone"),
                toPreserve(`servicemilestone`.`serviceName`, E),
                toPreserve(`servicemilestone`.`milestone`, E))

proc `$`*[E](x: RequireService[E] | RunService[E] | ServiceStarted[E] |
    ServiceRunning[E] |
    ServiceDependency[E] |
    Dependee[E] |
    ServiceMilestone[E]): string =
  `$`(toPreserve(x, E))

proc `encode`*[E](x: RequireService[E] | RunService[E] | ServiceStarted[E] |
    ServiceRunning[E] |
    ServiceDependency[E] |
    Dependee[E] |
    ServiceMilestone[E]): seq[byte] =
  encode(toPreserve(x, E))
