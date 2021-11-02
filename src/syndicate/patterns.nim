# SPDX-License-Identifier: MIT

import
  std / [macros, tables]

import
  preserves

import
  ./protocols / dataspacePatterns

from ./actors import Ref

export
  dataspacePatterns.`$`

type
  CRec = dataspacePatterns.CRec[Ref]
  DCompoundRec = dataspacePatterns.DCompoundRec[Ref]
  DBind* = dataspacePatterns.DBind[Ref]
  DLit* = dataspacePatterns.DLit[Ref]
  DCompound* = dataspacePatterns.DCompound[Ref]
  Pattern* = dataspacePatterns.Pattern[Ref]
proc `?`*(d: DBind): Pattern =
  Pattern(orKind: PatternKind.DBind, dbind: d)

proc `?`*(d: DLit): Pattern =
  Pattern(orKind: PatternKind.DLit, dlit: d)

proc `?`*(d: DCompound): Pattern =
  Pattern(orKind: PatternKind.DCompound, dcompound: d)

proc `?`*(s: string): Pattern =
  ?DLit(value: toPreserve(s, Ref))

proc arity(T: typedesc): int =
  var t: T
  for _ in fields(t):
    inc result

proc `? _`*(): Pattern =
  Pattern(orKind: PatternKind.DDiscard)

proc `?*`*(): Pattern =
  ?DBind(pattern: `? _`())

proc `?`*(T: typedesc; bindings: openArray[(int, Pattern)]): Pattern =
  ## Pattern constructor operator.
  when T.hasCustomPragma(preservesRecord):
    var label = tosymbol(T.getCustomPragmaVal(preservesRecord), Ref)
    result = ?DCompound(orKind: DCompoundKind.rec, rec: DCompoundRec(
        ctor: CRec(label: label, arity: T.arity), members: toTable bindings))
  else:
    {.error: "no custom pragma on " & $T.}

proc observe*(pat: Pattern): Pattern =
  ?DCompound(orKind: DCompoundKind.rec, rec: DCompoundRec(
      ctor: CRec(label: toSymbol("Observe", Ref), arity: 2),
      members: toTable {0: pat}))
