# SPDX-License-Identifier: MIT

import
  std / [tables, unittest]

import
  preserves, syndicate

from syndicate / protocols / dataspace import Observe

type
  Observe = dataspace.Observe[Cap]
import
  ./test_schema

test "patterns":
  let observerPat = ?Observe(pattern: !Foo) ?? {0: grab()}
  check($observerPat ==
      """<rec Observe [<rec rec [<lit foo> <arr [<bind <_>> <_> <_>]>]> <_>]>""")
  let
    value = @["alles", "in", "ordnung"]
    observer = toPreserve(Observe(pattern: inject(?Foo, {0: ?value})), Cap)
    have = capture(observerPat, observer).toPreserve(Cap).unpackLiterals
    want = [value.toPreserve(Cap)].toPreserve(Cap)
  check(have == want)
type
  Record {.preservesDictionary.} = object
  
test "dictionaries":
  let pat = ?Record
  echo pat
  var source = initDictionary(Cap)
  source["b".toSymbol(Cap)] = 2.toPreserve(Cap)
  source["c".toSymbol(Cap)] = 3.toPreserve(Cap)
  source["a".toSymbol(Cap)] = 1.toPreserve(Cap)
  let values = capture(pat, source)
  check $values == "@[1, 2, 3]"
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
  echo capture.value