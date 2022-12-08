# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves, protocol, protocol, protocol, protocol, protocol,
  protocol

type
  TargetedTurnEvent*[Cap] {.preservesRecord: "event".} = object
  
  `LinkedTaskReleaseReason`* {.preservesOr, pure.} = enum
    `cancelled`, `normal`
  TurnId*[Cap] = Preserve[Cap]
  AssertionDescriptionKind* {.pure.} = enum
    `value`, `opaque`
  AssertionDescriptionValue*[Cap] {.preservesRecord: "value".} = object
  
  AssertionDescriptionOpaque*[Cap] {.preservesRecord: "opaque".} = object
  
  `AssertionDescription`*[Cap] {.preservesOr.} = object
    case orKind*: AssertionDescriptionKind
    of AssertionDescriptionKind.`value`:
      
    of AssertionDescriptionKind.`opaque`:
      
  
  NameKind* {.pure.} = enum
    `anonymous`, `named`
  NameAnonymous* {.preservesRecord: "anonymous".} = object
  NameNamed*[Cap] {.preservesRecord: "named".} = object
  
  `Name`*[Cap] {.preservesOr.} = object
    case orKind*: NameKind
    of NameKind.`anonymous`:
      
    of NameKind.`named`:
      
  
  ActorId*[Cap] = Preserve[Cap]
  FacetId*[Cap] = Preserve[Cap]
  `FacetStopReason`* {.preservesOr, pure.} = enum
    `explicitAction`, `inert`, `parentStopping`, `actorStopping`
  TaskId*[Cap] = Preserve[Cap]
  ActorActivationKind* {.pure.} = enum
    `start`, `turn`, `stop`
  ActorActivationStart*[Cap] {.preservesRecord: "start".} = object
  
  ActorActivationStop* {.preservesRecord: "stop".} = object
  
  `ActorActivation`*[Cap] {.preservesOr.} = object
    case orKind*: ActorActivationKind
    of ActorActivationKind.`start`:
      
    of ActorActivationKind.`turn`:
      
    of ActorActivationKind.`stop`:
      
  
  Target*[Cap] {.preservesRecord: "entity".} = object
  
  TurnCauseKind* {.pure.} = enum
    `turn`, `cleanup`, `linkedTaskRelease`, `periodicActivation`, `delay`,
    `external`
  TurnCauseTurn*[Cap] {.preservesRecord: "caused-by".} = object
  
  TurnCauseCleanup* {.preservesRecord: "cleanup".} = object
  TurnCauseLinkedTaskRelease*[Cap] {.preservesRecord: "linked-task-release".} = object
  
  TurnCausePeriodicActivation* {.preservesRecord: "periodic-activation".} = object
  
  TurnCauseDelay*[Cap] {.preservesRecord: "delay".} = object
  
  TurnCauseExternal*[Cap] {.preservesRecord: "external".} = object
  
  `TurnCause`*[Cap] {.preservesOr.} = object
    case orKind*: TurnCauseKind
    of TurnCauseKind.`turn`:
      
    of TurnCauseKind.`cleanup`:
      
    of TurnCauseKind.`linkedTaskRelease`:
      
    of TurnCauseKind.`periodicActivation`:
      
    of TurnCauseKind.`delay`:
      
    of TurnCauseKind.`external`:
      
  
  TurnEventKind* {.pure.} = enum
    `assert`, `retract`, `message`, `sync`, `breakLink`
  TurnEventAssert*[Cap] {.preservesRecord: "assert".} = object
  
  TurnEventRetract* {.preservesRecord: "retract".} = object
  
  TurnEventMessage*[Cap] {.preservesRecord: "message".} = object
  
  TurnEventSync*[Cap] {.preservesRecord: "sync".} = object
  
  TurnEventBreakLink*[Cap] {.preservesRecord: "break-link".} = object
  
  `TurnEvent`*[Cap] {.preservesOr.} = object
    case orKind*: TurnEventKind
    of TurnEventKind.`assert`:
      
    of TurnEventKind.`retract`:
      
    of TurnEventKind.`message`:
      
    of TurnEventKind.`sync`:
      
    of TurnEventKind.`breakLink`:
      
  
  TurnDescription*[Cap] {.preservesRecord: "turn".} = object
  
  ExitStatusKind* {.pure.} = enum
    `ok`, `Error`
  `ExitStatus`* {.preservesOr.} = object
    case orKind*: ExitStatusKind
    of ExitStatusKind.`ok`:
      
    of ExitStatusKind.`Error`:
      
  
  TraceEntry*[Cap] {.preservesRecord: "trace".} = object
  
  Oid*[Cap] = Preserve[Cap]
  ActionDescriptionKind* {.pure.} = enum
    `dequeue`, `enqueue`, `dequeueInternal`, `enqueueInternal`, `spawn`, `link`,
    `facetStart`, `facetStop`, `linkedTaskStart`
  ActionDescriptionDequeue*[Cap] {.preservesRecord: "dequeue".} = object
  
  ActionDescriptionEnqueue*[Cap] {.preservesRecord: "enqueue".} = object
  
  ActionDescriptionDequeueInternal*[Cap] {.preservesRecord: "dequeue-internal".} = object
  
  ActionDescriptionEnqueueInternal*[Cap] {.preservesRecord: "enqueue-internal".} = object
  
  ActionDescriptionSpawn*[Cap] {.preservesRecord: "spawn".} = object
  
  ActionDescriptionLink*[Cap] {.preservesRecord: "link".} = object
  
  ActionDescriptionFacetStart*[Cap] {.preservesRecord: "facet-start".} = object
  
  ActionDescriptionFacetStop*[Cap] {.preservesRecord: "facet-stop".} = object
  
  ActionDescriptionLinkedTaskStart*[Cap] {.preservesRecord: "linked-task-start".} = object
  
  `ActionDescription`*[Cap] {.preservesOr.} = object
    case orKind*: ActionDescriptionKind
    of ActionDescriptionKind.`dequeue`:
      
    of ActionDescriptionKind.`enqueue`:
      
    of ActionDescriptionKind.`dequeueInternal`:
      
    of ActionDescriptionKind.`enqueueInternal`:
      
    of ActionDescriptionKind.`spawn`:
      
    of ActionDescriptionKind.`link`:
      
    of ActionDescriptionKind.`facetStart`:
      
    of ActionDescriptionKind.`facetStop`:
      
    of ActionDescriptionKind.`linkedTaskStart`:
      
  
proc `$`*[Cap](x: TargetedTurnEvent[Cap] | AssertionDescription[Cap] | Name[Cap] |
    ActorActivation[Cap] |
    Target[Cap] |
    TurnCause[Cap] |
    TurnEvent[Cap] |
    TurnDescription[Cap] |
    TraceEntry[Cap] |
    ActionDescription[Cap]): string =
  `$`(toPreserve(x, Cap))

proc encode*[Cap](x: TargetedTurnEvent[Cap] | AssertionDescription[Cap] |
    Name[Cap] |
    ActorActivation[Cap] |
    Target[Cap] |
    TurnCause[Cap] |
    TurnEvent[Cap] |
    TurnDescription[Cap] |
    TraceEntry[Cap] |
    ActionDescription[Cap]): seq[byte] =
  encode(toPreserve(x, Cap))

proc `$`*(x: ExitStatus): string =
  `$`(toPreserve(x))

proc encode*(x: ExitStatus): seq[byte] =
  encode(toPreserve(x))
