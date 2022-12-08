# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves, std / tables, std / tables

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
      
  
  PAnd*[Cap] {.preservesRecord: "and".} = ref object
  
  Rewrite*[Cap] {.preservesRecord: "rewrite".} = ref object
  
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
      
  
  `PAtom`* {.preservesOr, pure.} = enum
    `Boolean`, `Float`, `Double`, `SignedInteger`, `String`, `ByteString`,
    `Symbol`
  Attenuation*[Cap] = seq[Caveat[Cap]]
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
    `Rewrite`, `Alts`
  `Caveat`*[Cap] {.preservesOr.} = ref object
    case orKind*: CaveatKind
    of CaveatKind.`Rewrite`:
      
    of CaveatKind.`Alts`:
      
  
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
      
  
proc `$`*[Cap](x: PCompound[Cap] | PAnd[Cap] | Rewrite[Cap] | PBind[Cap] |
    Lit[Cap] |
    TCompound[Cap] |
    Attenuation[Cap] |
    Template[Cap] |
    Caveat[Cap] |
    PNot[Cap] |
    SturdyRef[Cap] |
    WireRef[Cap] |
    TAttenuate[Cap] |
    Alts[Cap] |
    Pattern[Cap]): string =
  `$`(toPreserve(x, Cap))

proc encode*[Cap](x: PCompound[Cap] | PAnd[Cap] | Rewrite[Cap] | PBind[Cap] |
    Lit[Cap] |
    TCompound[Cap] |
    Attenuation[Cap] |
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
