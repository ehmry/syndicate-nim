# SPDX-License-Identifier: MIT

import
  preserves, std / tables

type
  PCompoundKind* {.pure.} = enum
    `rec`, `arr`, `dict`
  PCompoundRec*[Cap] {.preservesRecord: "rec".} = ref object
  
  PCompoundArr*[Cap] {.preservesRecord: "arr".} = ref object
  
  PCompoundDict*[Cap] {.preservesRecord: "dict".} = ref object
  
  `PCompound`*[Cap] {.preservesOr.} = ref object
    case orKind*: PCompoundKind
    of PCompoundKind.`rec`:
      
    of PCompoundKind.`arr`:
      
    of PCompoundKind.`dict`:
      
  
  Reject*[Cap] {.preservesRecord: "reject".} = ref object
  
  CaveatsFieldKind* {.pure.} = enum
    `present`, `invalid`, `absent`
  CaveatsFieldPresent*[Cap] {.preservesDictionary.} = ref object
  
  CaveatsFieldInvalid*[Cap] {.preservesDictionary.} = object
  
  CaveatsFieldAbsent* {.preservesDictionary.} = object
  `CaveatsField`*[Cap] {.preservesOr.} = ref object
    case orKind*: CaveatsFieldKind
    of CaveatsFieldKind.`present`:
      
    of CaveatsFieldKind.`invalid`:
      
    of CaveatsFieldKind.`absent`:
      
  
  SturdyDescriptionDetail*[Cap] {.preservesDictionary.} = object
  
  PAnd*[Cap] {.preservesRecord: "and".} = ref object
  
  SturdyStepDetail*[Cap] = Parameters[Cap]
  Rewrite*[Cap] {.preservesRecord: "rewrite".} = ref object
  
  Parameters*[Cap] = Preserve[Cap]
  TRef* {.preservesRecord: "ref".} = object
  
  PBind*[Cap] {.preservesRecord: "bind".} = ref object
  
  Lit*[Cap] {.preservesRecord: "lit".} = object
  
  TCompoundKind* {.pure.} = enum
    `rec`, `arr`, `dict`
  TCompoundRec*[Cap] {.preservesRecord: "rec".} = ref object
  
  TCompoundArr*[Cap] {.preservesRecord: "arr".} = ref object
  
  TCompoundDict*[Cap] {.preservesRecord: "dict".} = ref object
  
  `TCompound`*[Cap] {.preservesOr.} = ref object
    case orKind*: TCompoundKind
    of TCompoundKind.`rec`:
      
    of TCompoundKind.`arr`:
      
    of TCompoundKind.`dict`:
      
  
  SturdyPathStepDetail*[Cap] = Parameters[Cap]
  `PAtom`* {.preservesOr, pure.} = enum
    `Boolean`, `Float`, `Double`, `SignedInteger`, `String`, `ByteString`,
    `Symbol`
  PDiscard* {.preservesRecord: "_".} = object
  TemplateKind* {.pure.} = enum
    `TAttenuate`, `TRef`, `Lit`, `TCompound`
  `Template`*[Cap] {.preservesOr.} = ref object
    case orKind*: TemplateKind
    of TemplateKind.`TAttenuate`:
      
    of TemplateKind.`TRef`:
      
    of TemplateKind.`Lit`:
      
    of TemplateKind.`TCompound`:
      
  
  CaveatKind* {.pure.} = enum
    `Rewrite`, `Alts`, `Reject`, `unknown`
  CaveatUnknown*[Cap] = Preserve[Cap]
  `Caveat`*[Cap] {.preservesOr.} = ref object
    case orKind*: CaveatKind
    of CaveatKind.`Rewrite`:
      
    of CaveatKind.`Alts`:
      
    of CaveatKind.`Reject`:
      
    of CaveatKind.`unknown`:
      
  
  PNot*[Cap] {.preservesRecord: "not".} = ref object
  
  SturdyRef*[Cap] {.preservesRecord: "ref".} = ref object
  
  WireRefKind* {.pure.} = enum
    `mine`, `yours`
  WireRefMine* {.preservesTuple.} = object
  
  WireRefYours*[Cap] {.preservesTuple.} = ref object
  
  `WireRef`*[Cap] {.preservesOr.} = ref object
    case orKind*: WireRefKind
    of WireRefKind.`mine`:
      
    of WireRefKind.`yours`:
      
  
  TAttenuate*[Cap] {.preservesRecord: "attenuate".} = ref object
  
  Oid* = BiggestInt
  Alts*[Cap] {.preservesRecord: "or".} = ref object
  
  PatternKind* {.pure.} = enum
    `PDiscard`, `PAtom`, `PEmbedded`, `PBind`, `PAnd`, `PNot`, `Lit`,
    `PCompound`
  `Pattern`*[Cap] {.preservesOr.} = ref object
    case orKind*: PatternKind
    of PatternKind.`PDiscard`:
      
    of PatternKind.`PAtom`:
      
    of PatternKind.`PEmbedded`:
      
    of PatternKind.`PBind`:
      
    of PatternKind.`PAnd`:
      
    of PatternKind.`PNot`:
      
    of PatternKind.`Lit`:
      
    of PatternKind.`PCompound`:
      
  
proc `$`*[Cap](x: PCompound[Cap] | Reject[Cap] | CaveatsField[Cap] |
    SturdyDescriptionDetail[Cap] |
    PAnd[Cap] |
    Rewrite[Cap] |
    PBind[Cap] |
    Lit[Cap] |
    TCompound[Cap] |
    Template[Cap] |
    Caveat[Cap] |
    PNot[Cap] |
    SturdyRef[Cap] |
    WireRef[Cap] |
    TAttenuate[Cap] |
    Alts[Cap] |
    Pattern[Cap]): string =
  `$`(toPreserve(x, Cap))

proc encode*[Cap](x: PCompound[Cap] | Reject[Cap] | CaveatsField[Cap] |
    SturdyDescriptionDetail[Cap] |
    PAnd[Cap] |
    Rewrite[Cap] |
    PBind[Cap] |
    Lit[Cap] |
    TCompound[Cap] |
    Template[Cap] |
    Caveat[Cap] |
    PNot[Cap] |
    SturdyRef[Cap] |
    WireRef[Cap] |
    TAttenuate[Cap] |
    Alts[Cap] |
    Pattern[Cap]): seq[byte] =
  encode(toPreserve(x, Cap))

proc `$`*(x: TRef | PDiscard | Oid): string =
  `$`(toPreserve(x))

proc encode*(x: TRef | PDiscard | Oid): seq[byte] =
  encode(toPreserve(x))
