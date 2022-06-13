# SPDX-License-Identifier: MIT

import
  std / [tables, typetraits]

from std / sequtils import toSeq

import
  preserves

import
  ./protocols / dataspacePatterns

from ./actors import Ref

export
  dataspacePatterns.`$`, PatternKind, DCompoundKind

type
  AnyAtom* = dataspacePatterns.AnyAtom[Ref]
  DBind* = dataspacePatterns.DBind[Ref]
  DCompound* = dataspacePatterns.DCompound[Ref]
  DCompoundArr* = dataspacePatterns.DCompoundArr[Ref]
  DCompoundDict* = dataspacePatterns.DCompoundDict[Ref]
  DCompoundRec* = dataspacePatterns.DCompoundRec[Ref]
  DLit* = dataspacePatterns.DLit[Ref]
  Pattern* = dataspacePatterns.Pattern[Ref]
proc `?`*(d: DBind): Pattern =
  Pattern(orKind: PatternKind.DBind, dbind: d)

proc `?`*(d: DLit): Pattern =
  Pattern(orKind: PatternKind.DLit, dlit: d)

proc `?`*(d: DCompound): Pattern =
  Pattern(orKind: PatternKind.DCompound, dcompound: d)

proc `?`*(d: DCompoundRec): Pattern =
  ?DCompound(orKind: DCompoundKind.rec, rec: d)

proc `?`*(x: bool): Pattern =
  ?DLit(value: AnyAtom(orKind: AnyAtomKind.`bool`, bool: x))

proc `?`*(x: float32): Pattern =
  ?DLit(value: AnyAtom(orKind: AnyAtomKind.`float`, float: x))

proc `?`*(x: float64): Pattern =
  ?DLit(value: AnyAtom(orKind: AnyAtomKind.`double`, double: x))

proc `?`*(x: int): Pattern =
  ?DLit(value: AnyAtom(orKind: AnyAtomKind.`int`, int: x))

proc `?`*(s: string): Pattern =
  ?DLit(value: AnyAtom(orKind: AnyAtomKind.`string`, string: s))

proc `?`*(x: seq[byte]): Pattern =
  ?DLit(value: AnyAtom(orKind: AnyAtomKind.`bytes`, bytes: x))

proc `?`*(x: Symbol): Pattern =
  ?DLit(value: AnyAtom(orKind: AnyAtomKind.`symbol`, symbol: x))

proc drop*(): Pattern =
  Pattern(orKind: PatternKind.DDiscard)

proc grab*(): Pattern =
  ?DBind(pattern: drop())

proc `? _`*(): Pattern =
  drop()

proc `?*`*(): Pattern =
  grab()

proc `?`*[A, B](table: TableRef[A, B]): Pattern =
  raiseAssert "not implemented"

proc recordPattern*(label: Preserve[Ref]; fields: varargs[Pattern]): Pattern =
  ?DCompoundRec(label: label, fields: fields.toSeq)

type
  Value = Preserve[Ref]
  Path = seq[Value]
  Analysis* = tuple[constPaths: seq[Path], constValues: seq[Value],
                    capturePaths: seq[Path]]
func walk(result: var Analysis; path: var Path; p: Pattern)
func walk(result: var Analysis; path: var Path; key: int | Value; pat: Pattern) =
  path.add(key.toPreserve(Ref))
  walk(result, path, pat)
  discard path.pop

func walk(result: var Analysis; path: var Path; p: Pattern) =
  case p.orKind
  of PatternKind.DCompound:
    case p.dcompound.orKind
    of DCompoundKind.rec:
      for k, e in p.dcompound.rec.fields:
        walk(result, path, k, e)
    of DCompoundKind.arr:
      for k, e in p.dcompound.arr.items:
        walk(result, path, k, e)
    of DCompoundKind.dict:
      for k, e in p.dcompound.dict.entries:
        walk(result, path, k, e)
  of PatternKind.DBind:
    result.capturePaths.add(path)
    walk(result, path, p.dbind.pattern)
  of PatternKind.DDiscard:
    discard
  of PatternKind.DLit:
    result.constPaths.add(path)
    result.constValues.add(p.dlit.value.toPreserve(Ref))

func analyse*(p: Pattern): Analysis =
  var path: Path
  walk(result, path, p)
