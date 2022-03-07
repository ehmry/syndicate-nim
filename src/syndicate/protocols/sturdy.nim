# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves, std / tables

type
  PCompoundKind* {.pure.} = enum
    `rec`, `arr`, `dict`
  PCompoundRec*[E] {.preservesRecord: "rec".} = ref object
  
  PCompoundArr*[E] {.preservesRecord: "arr".} = ref object
  
  PCompoundDict*[E] {.preservesRecord: "dict".} = ref object
  
  `PCompound`*[E] {.preservesOr.} = ref object
    case orKind*: PCompoundKind
    of PCompoundKind.`rec`:
      
    of PCompoundKind.`arr`:
      
    of PCompoundKind.`dict`:
      
  
  PAnd*[E] {.preservesRecord: "and".} = ref object
  
  Rewrite*[E] {.preservesRecord: "rewrite".} = ref object
  
  TRef* {.preservesRecord: "ref".} = object
  
  PBind*[E] {.preservesRecord: "bind".} = ref object
  
  Lit*[E] {.preservesRecord: "lit".} = ref object
  
  TCompoundKind* {.pure.} = enum
    `rec`, `arr`, `dict`
  TCompoundRec*[E] {.preservesRecord: "rec".} = ref object
  
  TCompoundArr*[E] {.preservesRecord: "arr".} = ref object
  
  TCompoundDict*[E] {.preservesRecord: "dict".} = ref object
  
  `TCompound`*[E] {.preservesOr.} = ref object
    case orKind*: TCompoundKind
    of TCompoundKind.`rec`:
      
    of TCompoundKind.`arr`:
      
    of TCompoundKind.`dict`:
      
  
  `PAtom`* {.preservesOr, pure.} = enum
    `Boolean`, `Float`, `Double`, `SignedInteger`, `String`, `ByteString`,
    `Symbol`
  Attenuation*[E] = seq[Caveat[E]]
  PDiscard* {.preservesRecord: "_".} = object
  TemplateKind* {.pure.} = enum
    `TAttenuate`, `TRef`, `Lit`, `TCompound`
  `Template`*[E] {.preservesOr.} = ref object
    case orKind*: TemplateKind
    of TemplateKind.`TAttenuate`:
      
    of TemplateKind.`TRef`:
      
    of TemplateKind.`Lit`:
      
    of TemplateKind.`TCompound`:
      
  
  CaveatKind* {.pure.} = enum
    `Rewrite`, `Alts`
  `Caveat`*[E] {.preservesOr.} = ref object
    case orKind*: CaveatKind
    of CaveatKind.`Rewrite`:
      
    of CaveatKind.`Alts`:
      
  
  PNot*[E] {.preservesRecord: "not".} = ref object
  
  SturdyRef*[E] {.preservesRecord: "ref".} = ref object
  
  WireRefKind* {.pure.} = enum
    `mine`, `yours`
  WireRefMine* {.preservesTuple.} = object
  
  WireRefYours*[E] {.preservesTuple.} = ref object
  
  `WireRef`*[E] {.preservesOr.} = ref object
    case orKind*: WireRefKind
    of WireRefKind.`mine`:
      
    of WireRefKind.`yours`:
      
  
  TAttenuate*[E] {.preservesRecord: "attenuate".} = ref object
  
  Oid* = int
  Alts*[E] {.preservesRecord: "or".} = ref object
  
  PatternKind* {.pure.} = enum
    `PDiscard`, `PAtom`, `PEmbedded`, `PBind`, `PAnd`, `PNot`, `Lit`,
    `PCompound`
  `Pattern`*[E] {.preservesOr.} = ref object
    case orKind*: PatternKind
    of PatternKind.`PDiscard`:
      
    of PatternKind.`PAtom`:
      
    of PatternKind.`PEmbedded`:
      
    of PatternKind.`PBind`:
      
    of PatternKind.`PAnd`:
      
    of PatternKind.`PNot`:
      
    of PatternKind.`Lit`:
      
    of PatternKind.`PCompound`:
      
  
proc `$`*[E](x: PCompound[E] | PAnd[E] | Rewrite[E] | PBind[E] | Lit[E] |
    TCompound[E] |
    Attenuation[E] |
    Template[E] |
    Caveat[E] |
    PNot[E] |
    SturdyRef[E] |
    WireRef[E] |
    TAttenuate[E] |
    Alts[E] |
    Pattern[E]): string =
  `$`(toPreserve(x, E))

proc encode*[E](x: PCompound[E] | PAnd[E] | Rewrite[E] | PBind[E] | Lit[E] |
    TCompound[E] |
    Attenuation[E] |
    Template[E] |
    Caveat[E] |
    PNot[E] |
    SturdyRef[E] |
    WireRef[E] |
    TAttenuate[E] |
    Alts[E] |
    Pattern[E]): seq[byte] =
  encode(toPreserve(x, E))

proc `$`*(x: TRef | PDiscard | Oid): string =
  `$`(toPreserve(x))

proc encode*(x: TRef | PDiscard | Oid): seq[byte] =
  encode(toPreserve(x))
