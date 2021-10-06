# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves, std / tables, std / tables, std / tables

type
  PatternKind* {.pure.} = enum
    `Ddiscard`, `Dbind`, `Dlit`, `Dcompound`
  Pattern*[E = void] = ref object ## ``/ DDiscard / DBind / DLit / DCompound``
    case kind*: PatternKind
    of PatternKind.`Ddiscard`:
      
    of PatternKind.`Dbind`:
      
    of PatternKind.`Dlit`:
      
    of PatternKind.`Dcompound`:
      
  
  DDiscard* {.record: "_".} = object ## ``<_>``
    nil

  DBind*[E = void] {.record: "bind".} = ref object ## ``<bind @pattern Pattern>``
  
  DLit*[E = void] {.record: "lit".} = ref object ## ``<lit @value any>``
  
  DcompoundKind* {.pure.} = enum
    `rec`, `arr`, `dict`
  DCompoundrec*[E = void] {.record: "compound".} = ref object
  
  DCompoundarr*[E = void] {.record: "compound".} = ref object
  
  DCompounddict*[E = void] {.record: "compound".} = ref object
  
  DCompound*[E = void] = ref object ## ``/ <compound @ctor CRec @members {int : Pattern ...:...}> / <compound @ctor CArr @members {int : Pattern ...:...}> / <compound @ctor CDict @members {any : Pattern ...:...}>``
    case kind*: DcompoundKind
    of DcompoundKind.`rec`:
      
    of DcompoundKind.`arr`:
      
    of DcompoundKind.`dict`:
      
  
  CRec*[E = void] {.record: "rec".} = ref object ## ``<rec @label any @arity int>``
  
  CArr* {.record: "arr".} = ref object ## ``<arr @arity int>``
  
  CDict* {.record: "dict".} = object ## ``<dict>``
    nil

proc toPreserveHook*(v: Pattern; E: typedesc): Preserve[E] =
  case v.kind
  of PatternKind.`Ddiscard`:
    toPreserve(v.`ddiscard`, E)
  of PatternKind.`Dbind`:
    toPreserve(v.`dbind`, E)
  of PatternKind.`Dlit`:
    toPreserve(v.`dlit`, E)
  of PatternKind.`Dcompound`:
    toPreserve(v.`dcompound`, E)

proc fromPreserveHook*[E](v: var Pattern; pr: Preserve[E]): bool =
  if isRecord(pr) and pr.label.isSymbol("DDiscard"):
    v = Pattern(kind: PatternKind.`Ddiscard`)
    result = fromPreserve(v.`ddiscard`, pr)
  elif isRecord(pr) and pr.label.isSymbol("DBind"):
    v = Pattern(kind: PatternKind.`Dbind`)
    result = fromPreserve(v.`dbind`, pr)
  elif isRecord(pr) and pr.label.isSymbol("DLit"):
    v = Pattern(kind: PatternKind.`Dlit`)
    result = fromPreserve(v.`dlit`, pr)
  elif false:               ## snkOr - / <compound @ctor CRec @members {int : Pattern ...:...}> / <compound @ctor CArr @members {int : Pattern ...:...}> / <compound @ctor CDict @members {any : Pattern ...:...}>
    discard

proc `dDiscard`*[E = void](): Preserve[E] =
  ## Preserves constructor for ``DDiscard``.
  initRecord[E](symbol[E]("_"))

proc toPreserveHook*(`ddiscard`: DDiscard; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("_"))

proc `dBind`*[E = void](`pattern`: Pattern | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``DBind``.
  initRecord[E](symbol[E]("bind"), toPreserve(`pattern`, E))

proc toPreserveHook*(`dbind`: DBind; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("bind"), toPreserve(`dbind`.`pattern`, E))

proc `dLit`*[E = void](`value`: Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``DLit``.
  initRecord[E](symbol[E]("lit"), toPreserve(`value`, E))

proc toPreserveHook*(`dlit`: DLit; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("lit"), toPreserve(`dlit`.`value`, E))

proc toPreserveHook*(v: DCompound; E: typedesc): Preserve[E] =
  case v.kind
  of DCompoundKind.`rec`:
    toPreserve(v.`rec`, E)
  of DCompoundKind.`arr`:
    toPreserve(v.`arr`, E)
  of DCompoundKind.`dict`:
    toPreserve(v.`dict`, E)

proc fromPreserveHook*[E](v: var DCompound; pr: Preserve[E]): bool =
  if isRecord(pr) and pr.label.isSymbol("rec"):
    v = DCompound(kind: DCompoundKind.`rec`)
    result = fromPreserve(v.`rec`, pr)
  elif isRecord(pr) and pr.label.isSymbol("arr"):
    v = DCompound(kind: DCompoundKind.`arr`)
    result = fromPreserve(v.`arr`, pr)
  elif isRecord(pr) and pr.label.isSymbol("dict"):
    v = DCompound(kind: DCompoundKind.`dict`)
    result = fromPreserve(v.`dict`, pr)

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

proc `$`*[E](x: Pattern[E] | DDiscard | DBind[E] | DLit[E] | DCompound[E] |
    CRec[E] |
    CArr |
    CDict): string =
  `$`(toPreserve(x, E))

proc `encode`*[E](x: Pattern[E] | DDiscard | DBind[E] | DLit[E] | DCompound[E] |
    CRec[E] |
    CArr |
    CDict): seq[byte] =
  encode(toPreserve(x, E))
