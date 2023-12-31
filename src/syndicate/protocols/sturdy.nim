# SPDX-License-Identifier: MIT

import
  preserves, std / tables

type
  PCompoundKind* {.pure.} = enum
    `rec`, `arr`, `dict`
  PCompoundRec* {.preservesRecord: "rec".} = object
  
  PCompoundArr* {.preservesRecord: "arr".} = object
  
  PCompoundDict* {.preservesRecord: "dict".} = object
  
  `PCompound`* {.preservesOr.} = object
    case orKind*: PCompoundKind
    of PCompoundKind.`rec`:
      
    of PCompoundKind.`arr`:
      
    of PCompoundKind.`dict`:
      
  
  Reject* {.preservesRecord: "reject".} = object
  
  CaveatsFieldKind* {.pure.} = enum
    `present`, `invalid`, `absent`
  CaveatsFieldPresent* {.preservesDictionary.} = object
  
  CaveatsFieldInvalid* {.preservesDictionary.} = object
  
  CaveatsFieldAbsent* {.preservesDictionary.} = object
  `CaveatsField`* {.preservesOr.} = object
    case orKind*: CaveatsFieldKind
    of CaveatsFieldKind.`present`:
      
    of CaveatsFieldKind.`invalid`:
      
    of CaveatsFieldKind.`absent`:
      
  
  SturdyDescriptionDetail* {.preservesDictionary.} = object
  
  PAnd* {.preservesRecord: "and".} = object
  
  SturdyStepDetail* = Parameters
  Rewrite* {.preservesRecord: "rewrite".} = object
  
  `Parameters`* = Table[Symbol, Value]
  TRef* {.preservesRecord: "ref".} = object
  
  PBind* {.preservesRecord: "bind".} = object
  
  Lit* {.preservesRecord: "lit".} = object
  
  TCompoundKind* {.pure.} = enum
    `rec`, `arr`, `dict`
  TCompoundRec* {.preservesRecord: "rec".} = object
  
  TCompoundArr* {.preservesRecord: "arr".} = object
  
  TCompoundDict* {.preservesRecord: "dict".} = object
  
  `TCompound`* {.preservesOr.} = object
    case orKind*: TCompoundKind
    of TCompoundKind.`rec`:
      
    of TCompoundKind.`arr`:
      
    of TCompoundKind.`dict`:
      
  
  SturdyPathStepDetail* = Parameters
  `PAtom`* {.preservesOr, pure.} = enum
    `Boolean`, `Float`, `Double`, `SignedInteger`, `String`, `ByteString`,
    `Symbol`
  PDiscard* {.preservesRecord: "_".} = object
  TemplateKind* {.pure.} = enum
    `TAttenuate`, `TRef`, `Lit`, `TCompound`
  `Template`* {.acyclic, preservesOr.} = ref object
    case orKind*: TemplateKind
    of TemplateKind.`TAttenuate`:
      
    of TemplateKind.`TRef`:
      
    of TemplateKind.`Lit`:
      
    of TemplateKind.`TCompound`:
      
  
  CaveatKind* {.pure.} = enum
    `Rewrite`, `Alts`, `Reject`, `unknown`
  CaveatUnknown* = Value
  `Caveat`* {.preservesOr.} = object
    case orKind*: CaveatKind
    of CaveatKind.`Rewrite`:
      
    of CaveatKind.`Alts`:
      
    of CaveatKind.`Reject`:
      
    of CaveatKind.`unknown`:
      
  
  PNot* {.preservesRecord: "not".} = object
  
  SturdyRef* {.preservesRecord: "ref".} = object
  
  WireRefKind* {.pure.} = enum
    `mine`, `yours`
  WireRefMine* {.preservesTuple.} = object
  
  WireRefYours* {.preservesTuple.} = object
  
  `WireRef`* {.preservesOr.} = object
    case orKind*: WireRefKind
    of WireRefKind.`mine`:
      
    of WireRefKind.`yours`:
      
  
  TAttenuate* {.preservesRecord: "attenuate".} = object
  
  Oid* = BiggestInt
  Alts* {.preservesRecord: "or".} = object
  
  PatternKind* {.pure.} = enum
    `PDiscard`, `PAtom`, `PEmbedded`, `PBind`, `PAnd`, `PNot`, `Lit`,
    `PCompound`
  `Pattern`* {.acyclic, preservesOr.} = ref object
    case orKind*: PatternKind
    of PatternKind.`PDiscard`:
      
    of PatternKind.`PAtom`:
      
    of PatternKind.`PEmbedded`:
      
    of PatternKind.`PBind`:
      
    of PatternKind.`PAnd`:
      
    of PatternKind.`PNot`:
      
    of PatternKind.`Lit`:
      
    of PatternKind.`PCompound`:
      
  
proc `$`*(x: PCompound | Reject | CaveatsField | SturdyDescriptionDetail | PAnd |
    SturdyStepDetail |
    Rewrite |
    Parameters |
    TRef |
    PBind |
    Lit |
    TCompound |
    SturdyPathStepDetail |
    PDiscard |
    Template |
    Caveat |
    PNot |
    SturdyRef |
    WireRef |
    TAttenuate |
    Oid |
    Alts |
    Pattern): string =
  `$`(toPreserves(x))

proc encode*(x: PCompound | Reject | CaveatsField | SturdyDescriptionDetail |
    PAnd |
    SturdyStepDetail |
    Rewrite |
    Parameters |
    TRef |
    PBind |
    Lit |
    TCompound |
    SturdyPathStepDetail |
    PDiscard |
    Template |
    Caveat |
    PNot |
    SturdyRef |
    WireRef |
    TAttenuate |
    Oid |
    Alts |
    Pattern): seq[byte] =
  encode(toPreserves(x))
