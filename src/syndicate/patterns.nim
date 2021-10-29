# SPDX-License-Identifier: MIT

import
  std / [macros, tables]

import
  preserves

import
  ../syndicate / protocols / dataspacePatterns

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
proc `?`*(): Pattern =
  Pattern(orKind: PatternKind.DDiscard)

proc `?`*(d: DBind): Pattern =
  Pattern(orKind: PatternKind.DBind, dbind: d)

proc `?`*(d: DLit): Pattern =
  Pattern(orKind: PatternKind.DLit, dlit: d)

proc `?`*(d: DCompound): Pattern =
  Pattern(orKind: PatternKind.DCompound, dcompound: d)

proc arity(T: typedesc): int =
  var t: ptr T
  for _ in fields(t[]):
    inc result

proc `?`*(T: typedesc; bindings: openArray[(int, Pattern)]): Pattern =
  ## Pattern constructor operator.
  when T.hasCustomPragma(preservesRecord):
    var
      label = tosymbol(T.getCustomPragmaVal(preservesRecord), Ref)
      members: Table[BiggestInt, Pattern]
    for (i, p) in bindings:
      members[BiggestInt i] = ?DBind(pattern: p)
    result = ?DCompound(orKind: DCompoundKind.rec, rec: DCompoundRec(
        ctor: CRec(label: label, arity: T.arity), members: members))
