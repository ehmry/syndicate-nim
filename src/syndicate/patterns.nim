# SPDX-License-Identifier: MIT

import
  std / tables

from std / macros import hasCustomPragma, getCustomPragmaVal

import
  preserves

import
  ../syndicate / protocols / [dataspacePatterns]

from ./actors import Ref

type
  Pattern* = dataspacePatterns.Pattern[Ref]
proc bindDiscard(): Pattern =
  Pattern(orKind: PatternKind.DBind,
          dbind: DBind[Ref](pattern: Pattern(orKind: PatternKind.DDiscard)))

proc toPattern*(T: typedesc): Pattern =
  when T.hasCustomPragma(preservesRecord):
    let label = tosymbol(T.getCustomPragmaVal(preservesRecord), Ref)
    var
      arity: BiggestInt
      members: Table[BiggestInt, Pattern]
      t: ptr T
    for _ in fields(t[]):
      members[arity] = bindDiscard()
      inc arity
    result = Pattern(orKind: PatternKind.DCompound, dcompound: DCompound[Ref](
        orKind: DCompoundKind.rec, rec: DCompoundRec[Ref](
        ctor: CRec[Ref](label: label, arity: arity), members: members)))
