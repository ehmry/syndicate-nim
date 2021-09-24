# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves, std / tables, std / tables

type
  CRec*[E] {.preservesRecord: "rec".} = ref object
  
  PCompound*[E] {.preservesRecord: "compound".} = ref object
  
  ConstructorSpecKind* {.pure.} = enum
    `CRec`, `CArr`, `CDict`
  `ConstructorSpec`*[E] {.preservesOr.} = ref object
    case orKind*: ConstructorSpecKind
    of ConstructorSpecKind.`CRec`:
      
    of ConstructorSpecKind.`CArr`:
      
    of ConstructorSpecKind.`CDict`:
      
  
  PAnd*[E] {.preservesRecord: "and".} = ref object
  
  Rewrite*[E] {.preservesRecord: "rewrite".} = ref object
  
  TCompoundMembers*[E] = Table[Preserve[E], Template[E]]
  TRef* {.preservesRecord: "ref".} = object
  
  PBind*[E] {.preservesRecord: "bind".} = ref object
  
  Lit*[E] {.preservesRecord: "lit".} = ref object
  
  TCompound*[E] {.preservesRecord: "compound".} = ref object
  
  `PAtom`* {.preservesOr.} = enum
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
      
  
  CArr* {.preservesRecord: "arr".} = object
  
  PCompoundMembers*[E] = Table[Preserve[E], Pattern[E]]
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
  
  Oid* = BiggestInt
  Alts*[E] {.preservesRecord: "or".} = ref object
  
  CDict* {.preservesRecord: "dict".} = object
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
      
  
proc `$`*[E](x: CRec[E] | PCompound[E] | ConstructorSpec[E] | PAnd[E] |
    Rewrite[E] |
    TCompoundMembers[E] |
    PBind[E] |
    Lit[E] |
    TCompound[E] |
    Attenuation[E] |
    Template[E] |
    Caveat[E] |
    PCompoundMembers[E] |
    PNot[E] |
    SturdyRef[E] |
    WireRef[E] |
    TAttenuate[E] |
    Alts[E] |
    Pattern[E]): string =
  `$`(toPreserve(x, E))

proc encode*[E](x: CRec[E] | PCompound[E] | ConstructorSpec[E] | PAnd[E] |
    Rewrite[E] |
    TCompoundMembers[E] |
    PBind[E] |
    Lit[E] |
    TCompound[E] |
    Attenuation[E] |
    Template[E] |
    Caveat[E] |
    PCompoundMembers[E] |
    PNot[E] |
    SturdyRef[E] |
    WireRef[E] |
    TAttenuate[E] |
    Alts[E] |
    Pattern[E]): seq[byte] =
  encode(toPreserve(x, E))

proc `$`*(x: TRef | PDiscard | CArr | Oid | CDict): string =
  `$`(toPreserve(x))

proc encode*(x: TRef | PDiscard | CArr | Oid | CDict): seq[byte] =
  encode(toPreserve(x))
