# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  UserId* = BiggestInt
  NickConflict* {.preservesRecord: "nickConflict".} = object
  NickClaimResponseKind* {.pure.} = enum
    `true`, `NickConflict`
  `NickClaimResponse`* {.preservesOr.} = object
    case orKind*: NickClaimResponseKind
    of NickClaimResponseKind.`true`:
      
    of NickClaimResponseKind.`NickConflict`:
      
  
  Join*[Cap] {.preservesRecord: "joinedUser".} = object
  
  SessionKind* {.pure.} = enum
    `observeUsers`, `observeSpeech`, `NickClaim`, `Says`
  SessionObserveUsers*[Cap] {.preservesRecord: "Observe".} = object
  
  SessionObserveSpeech*[Cap] {.preservesRecord: "Observe".} = object
  
  `Session`*[Cap] {.preservesOr.} = object
    case orKind*: SessionKind
    of SessionKind.`observeUsers`:
      
    of SessionKind.`observeSpeech`:
      
    of SessionKind.`NickClaim`:
      
    of SessionKind.`Says`:
      
  
  UserInfo* {.preservesRecord: "user".} = object
  
  NickClaim*[Cap] {.preservesRecord: "claimNick".} = object
  
  Says* {.preservesRecord: "says".} = object
  
proc `$`*[Cap](x: Join[Cap] | Session[Cap] | NickClaim[Cap]): string =
  `$`(toPreserve(x, Cap))

proc encode*[Cap](x: Join[Cap] | Session[Cap] | NickClaim[Cap]): seq[byte] =
  encode(toPreserve(x, Cap))

proc `$`*(x: UserId | NickConflict | NickClaimResponse | UserInfo | Says): string =
  `$`(toPreserve(x))

proc encode*(x: UserId | NickConflict | NickClaimResponse | UserInfo | Says): seq[
    byte] =
  encode(toPreserve(x))
