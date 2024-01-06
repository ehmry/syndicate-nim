# SPDX-License-Identifier: MIT

import
  std / [options, tables, unittest]

import
  preserves, syndicate

import
  ./test_schema

test "patterns":
  let
    pat = ?Observe(pattern: !Foo) ?? {0: grab()}
    text = """<rec Observe [<rec rec [<lit foo> <arr [<bind <_>> <_> <_>]>]> <_>]>"""
  check($pat == text)
  let
    worte = @["alles", "in", "ordnung"]
    observer = Observe(pattern: inject(?:Foo, {0: ?worte})).toPreserves
    have = capture(pat, observer).toPreserves.unpackLiterals
    want = [worte.toPreserves].toPreserves
  check(have == want)
type
  Obj {.preservesDictionary.} = object
  
test "dictionaries":
  let pat = ?:Obj
  var source = initDictionary(Cap)
  source["b".toSymbol] = 2.toPreserves
  source["c".toSymbol] = 3.toPreserves
  source["a".toSymbol] = 1.toPreserves
  let values = capture(pat, source)
  check values.len == 3
  check values[0] == 1.toPreserves
  check values[1] == 2.toPreserves
  check values[2] == 3.toPreserves
type
  File {.preservesDictionary.} = object
  
  Files = Table[Symbol, File]
  Fields = Table[Symbol, string]
  Request {.preservesRecord: "request".} = object
  
test "literals":
  const
    txt = """<rec request [<lit 3> <dict {artists: <lit "kyyyyym"> date: <lit "2023-10-14"> notes: <lit "Lots of stuff"> title: <lit "Domes show">}> <dict {front-cover: <dict {name: <lit "ADULT_TIME_Glielmi.jpg"> path: <lit "/tmp/652adad1b3d2b666dcc8d857.jpg"> size: <lit 255614> type: <lit "image/jpeg">}>}>]>"""
  var pr = parsePreserves(txt)
  var capture: Literal[Request]
  check capture.fromPreserves(pr)
suite "captures":
  for txt in ["#f", "#t", "0", "-1", "foo", "<foo>", "[0, 1, 2]"]:
    test txt:
      let
        pr = parsePreserves txt
        pat = grab pr
      checkpoint $pat
      check pat.matches pr
suite "later-than":
  let
    obsA = parsePreserves"""<Observe <rec later-than [<lit 1704113731.419243>]> #!#f>"""
    obsB = parsePreserves"""<Observe <rec Observe [<rec rec [<lit later-than> <arr [<rec lit [<bind <_>>]>]>]> <_>]> #!#f>"""
    patA = """<rec later-than [<lit 1704113731.419243>]>""".parsePreserves.preservesTo(
        Pattern).get
    patB = """<rec Observe [<rec rec [<lit later-than> <arr [<rec lit [<bind <_>>]>]>]> <_>]>""".parsePreserves.preservesTo(
        Pattern).get
    patC = grab obsA
  test $patC:
    check patC.matches obsA
  test $patB:
    checkpoint $obsA
    check patB.matches obsA
suite "Observe":
  let pat = ?:Observe
  const
    text = """<rec Observe [<bind <_>> <bind <_>>]>"""
  check $pat == text