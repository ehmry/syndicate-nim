# SPDX-License-Identifier: MIT

import
  std / [sequtils, tables, typetraits]

import
  preserves

import
  ./protocols / dataspacePatterns

from ./actors import Ref

export
  dataspacePatterns.`$`, PatternKind, DCompoundKind

type
  Assertion = Preserve[Ref]
  AnyAtom* = dataspacePatterns.AnyAtom[Ref]
  DBind* = dataspacePatterns.DBind[Ref]
  DCompound* = dataspacePatterns.DCompound[Ref]
  DCompoundArr* = dataspacePatterns.DCompoundArr[Ref]
  DCompoundDict* = dataspacePatterns.DCompoundDict[Ref]
  DCompoundRec* = dataspacePatterns.DCompoundRec[Ref]
  DLit* = dataspacePatterns.DLit[Ref]
  Pattern* = dataspacePatterns.Pattern[Ref]
proc `?`*(d: sink DBind): Pattern =
  Pattern(orKind: PatternKind.DBind, dbind: d)

proc `?`*(d: sink DLit): Pattern =
  Pattern(orKind: PatternKind.DLit, dlit: d)

proc `?`*(aa: sink AnyAtom): Pattern =
  ?DLit(value: aa)

proc `?`*(d: sink DCompound): Pattern =
  Pattern(orKind: PatternKind.DCompound, dcompound: d)

proc `?`*(d: sink DCompoundRec): Pattern =
  ?DCompound(orKind: DCompoundKind.rec, rec: d)

proc `?`*(d: sink DCompoundArr): Pattern =
  ?DCompound(orKind: DCompoundKind.arr, arr: d)

proc `?`*(d: sink DCompoundDict): Pattern =
  ?DCompound(orKind: DCompoundKind.dict, dict: d)

proc `?`*(x: bool): Pattern =
  ?AnyAtom(orKind: AnyAtomKind.`bool`, bool: x)

proc `?`*(x: float32): Pattern =
  ?AnyAtom(orKind: AnyAtomKind.`float`, float: x)

proc `?`*(x: float64): Pattern =
  ?AnyAtom(orKind: AnyAtomKind.`double`, double: x)

proc `?`*(x: int): Pattern =
  ?AnyAtom(orKind: AnyAtomKind.`int`, int: x)

proc `?`*(s: sink string): Pattern =
  ?AnyAtom(orKind: AnyAtomKind.`string`, string: s)

proc `?`*(x: sink seq[byte]): Pattern =
  ?AnyAtom(orKind: AnyAtomKind.`bytes`, bytes: x)

proc `?`*(x: sink Symbol): Pattern =
  ?AnyAtom(orKind: AnyAtomKind.`symbol`, symbol: x)

proc `?`*[T](pr: Preserve[T]): Pattern =
  assert not pr.embedded
  case pr.kind
  of pkBoolean:
    ?pr.bool
  of pkFloat:
    ?pr.float
  of pkDouble:
    ?pr.double
  of pkSignedInteger:
    ?(int pr.int)
  of pkString:
    ?pr.string
  of pkByteString:
    ?pr.bytes
  of pkSymbol:
    ?pr.symbol
  of pkRecord:
    ?DCompoundRec(label: pr.label,
                  fields: map[Preserve[T], Pattern](pr.fields, `?`[T]))
  of pkSequence:
    ?DCompoundArr(items: map(pr.sequence, `?`[T]))
  of pkSet:
    raise newException(ValueError,
                       "cannot construct a pattern over a set literal")
  of pkDictionary:
    var dict = DCompoundDict()
    for key, val in pr.pairs:
      dict.entries[key] = ?val
    ?dict
  of pkEmbedded:
    raiseAssert "cannot construct a pattern over a embedded literal"

proc `??`*(pat: Pattern): Pattern =
  ## Construct a `Pattern` that matches a `Pattern`.
  case pat.orKind
  of PatternKind.DDiscard, PatternKind.DBind:
    result = pat
  of PatternKind.DLit:
    result = ?(pat.toPreserve(Ref))
  of PatternKind.DCompound:
    case pat.dcompound.orKind
    of DCompoundKind.rec:
      var fields = move pat.dcompound.rec.fields
      result = ?(pat.toPreserve(Ref))
      result.dcompound.rec.fields[1].dcompound.arr.items = fields
    of DCompoundKind.arr:
      var items = move pat.dcompound.arr.items
      result = ?(pat.toPreserve(Ref))
      result.dcompound.rec.fields[0].dcompound.arr.items = items
    of DCompoundKind.dict:
      stderr.writeLine "pattern construction from DCompoundKind not implemented"
      raiseAssert "not implemented"

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
