# SPDX-License-Identifier: MIT

import
  preserves, protocol

type
  TargetedTurnEvent* {.preservesRecord: "event".} = object
  
  `LinkedTaskReleaseReason`* {.preservesOr, pure.} = enum
    `cancelled`, `normal`
  TurnId* = Value
  AssertionDescriptionKind* {.pure.} = enum
    `value`, `opaque`
  AssertionDescriptionValue* {.preservesRecord: "value".} = object
  
  AssertionDescriptionOpaque* {.preservesRecord: "opaque".} = object
  
  `AssertionDescription`* {.preservesOr.} = object
    case orKind*: AssertionDescriptionKind
    of AssertionDescriptionKind.`value`:
      
    of AssertionDescriptionKind.`opaque`:
      
  
  NameKind* {.pure.} = enum
    `anonymous`, `named`
  NameAnonymous* {.preservesRecord: "anonymous".} = object
  NameNamed* {.preservesRecord: "named".} = object
  
  `Name`* {.preservesOr.} = object
    case orKind*: NameKind
    of NameKind.`anonymous`:
      
    of NameKind.`named`:
      
  
  ActorId* = Value
  FacetId* = Value
  `FacetStopReason`* {.preservesOr, pure.} = enum
    `explicitAction`, `inert`, `parentStopping`, `actorStopping`
  TaskId* = Value
  ActorActivationKind* {.pure.} = enum
    `start`, `turn`, `stop`
  ActorActivationStart* {.preservesRecord: "start".} = object
  
  ActorActivationStop* {.preservesRecord: "stop".} = object
  
  `ActorActivation`* {.preservesOr.} = object
    case orKind*: ActorActivationKind
    of ActorActivationKind.`start`:
      
    of ActorActivationKind.`turn`:
      
    of ActorActivationKind.`stop`:
      
  
  Target* {.preservesRecord: "entity".} = object
  
  TurnCauseKind* {.pure.} = enum
    `turn`, `cleanup`, `linkedTaskRelease`, `periodicActivation`, `delay`,
    `external`
  TurnCauseTurn* {.preservesRecord: "caused-by".} = object
  
  TurnCauseCleanup* {.preservesRecord: "cleanup".} = object
  TurnCauseLinkedTaskRelease* {.preservesRecord: "linked-task-release".} = object
  
  TurnCausePeriodicActivation* {.preservesRecord: "periodic-activation".} = object
  
  TurnCauseDelay* {.preservesRecord: "delay".} = object
  
  TurnCauseExternal* {.preservesRecord: "external".} = object
  
  `TurnCause`* {.preservesOr.} = object
    case orKind*: TurnCauseKind
    of TurnCauseKind.`turn`:
      
    of TurnCauseKind.`cleanup`:
      
    of TurnCauseKind.`linkedTaskRelease`:
      
    of TurnCauseKind.`periodicActivation`:
      
    of TurnCauseKind.`delay`:
      
    of TurnCauseKind.`external`:
      
  
  TurnEventKind* {.pure.} = enum
    `assert`, `retract`, `message`, `sync`, `breakLink`
  TurnEventAssert* {.preservesRecord: "assert".} = object
  
  TurnEventRetract* {.preservesRecord: "retract".} = object
  
  TurnEventMessage* {.preservesRecord: "message".} = object
  
  TurnEventSync* {.preservesRecord: "sync".} = object
  
  TurnEventBreakLink* {.preservesRecord: "break-link".} = object
  
  `TurnEvent`* {.preservesOr.} = object
    case orKind*: TurnEventKind
    of TurnEventKind.`assert`:
      
    of TurnEventKind.`retract`:
      
    of TurnEventKind.`message`:
      
    of TurnEventKind.`sync`:
      
    of TurnEventKind.`breakLink`:
      
  
  TurnDescription* {.preservesRecord: "turn".} = object
  
  ExitStatusKind* {.pure.} = enum
    `ok`, `Error`
  `ExitStatus`* {.preservesOr.} = object
    case orKind*: ExitStatusKind
    of ExitStatusKind.`ok`:
      
    of ExitStatusKind.`Error`:
      
  
  TraceEntry* {.preservesRecord: "trace".} = object
  
  Oid* = Value
  ActionDescriptionKind* {.pure.} = enum
    `dequeue`, `enqueue`, `dequeueInternal`, `enqueueInternal`, `spawn`, `link`,
    `facetStart`, `facetStop`, `linkedTaskStart`
  ActionDescriptionDequeue* {.preservesRecord: "dequeue".} = object
  
  ActionDescriptionEnqueue* {.preservesRecord: "enqueue".} = object
  
  ActionDescriptionDequeueInternal* {.preservesRecord: "dequeue-internal".} = object
  
  ActionDescriptionEnqueueInternal* {.preservesRecord: "enqueue-internal".} = object
  
  ActionDescriptionSpawn* {.preservesRecord: "spawn".} = object
  
  ActionDescriptionLink* {.preservesRecord: "link".} = object
  
  ActionDescriptionFacetStart* {.preservesRecord: "facet-start".} = object
  
  ActionDescriptionFacetStop* {.preservesRecord: "facet-stop".} = object
  
  ActionDescriptionLinkedTaskStart* {.preservesRecord: "linked-task-start".} = object
  
  `ActionDescription`* {.preservesOr.} = object
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
      
  
proc `$`*(x: TargetedTurnEvent | AssertionDescription | Name | ActorActivation |
    Target |
    TurnCause |
    TurnEvent |
    TurnDescription |
    ExitStatus |
    TraceEntry |
    ActionDescription): string =
  `$`(toPreserves(x))

proc encode*(x: TargetedTurnEvent | AssertionDescription | Name |
    ActorActivation |
    Target |
    TurnCause |
    TurnEvent |
    TurnDescription |
    ExitStatus |
    TraceEntry |
    ActionDescription): seq[byte] =
  encode(toPreserves(x))
