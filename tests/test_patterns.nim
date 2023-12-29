# SPDX-License-Identifier: MIT

import
  std / [tables, unittest]

import
  preserves, syndicate

import
  ./test_schema

test "patterns":
  let
    observerPat = ?Observe(pattern: !Foo) ?? {0: grab()}
    text = """<rec Observe [<rec rec [<lit foo> <arr [<bind <_>> <_> <_>]>]> <_>]>"""
  check($observerPat == text)
  let
    worte = @["alles", "in", "ordnung"]
    observer = Observe(pattern: inject(?:Foo, {0: ?worte})).toPreserve(Cap)
    have = capture(observerPat, observer).toPreserve(Cap).unpackLiterals
    want = [worte.toPreserve].toPreserve(Cap)
  check(have == want)
type
  Obj {.preservesDictionary.} = object
  
test "dictionaries":
  let pat = ?:Obj
  var source = initDictionary(Cap)
  source["b".toSymbol(Cap)] = 2.toPreserve(Cap)
  source["c".toSymbol(Cap)] = 3.toPreserve(Cap)
  source["a".toSymbol(Cap)] = 1.toPreserve(Cap)
  let values = capture(pat, source)
  check values.len == 3
  check values[0] == 1.toPreserve(Cap)
  check values[1] == 2.toPreserve(Cap)
  check values[2] == 3.toPreserve(Cap)
type
  File {.preservesDictionary.} = object
  
  Files = Table[Symbol, File]
  Fields = Table[Symbol, string]
  Request {.preservesRecord: "request".} = object
  
test "literals":
  const
    txt = """<rec request [<lit 3> <dict {artists: <lit "kyyyyym"> date: <lit "2023-10-14"> notes: <lit "Lots of stuff"> title: <lit "Domes show">}> <dict {front-cover: <dict {name: <lit "ADULT_TIME_Glielmi.jpg"> path: <lit "/tmp/652adad1b3d2b666dcc8d857.jpg"> size: <lit 255614> type: <lit "image/jpeg">}>}>]>"""
  var pr = parsePreserves(txt, Cap)
  var capture: Literal[Request]
  check capture.fromPreserve(pr)