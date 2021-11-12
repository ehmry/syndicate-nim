# SPDX-License-Identifier: MIT

import
  preserves

import
  ./protocols / sturdy, ./private / hmacs

from ./actors import Ref

type
  SturdyRef* = sturdy.SturdyRef[Ref]
proc mint*(key: openarray[byte]; oid: Preserve[Ref]): SturdyRef =
  SturdyRef(oid: oid, sig: hmacSha256(key, encode(oid), key.len))

proc mint*[T](key: openarray[byte]; oid: T): SturdyRef =
  let oidPr = toPreserve(oid, Ref)
  SturdyRef(oid: oidPr, sig: hmacSha256(key, encode(oidPr), key.len))

proc attenuate*(r: SturdyRef; caveats: Attenuation): SturdyRef =
  result = SturdyRef(oid: r.oid, caveatChain: r.caveatChain,
                     sig: hmacSha256(r.sig, caveats.encode))
  result.caveatChain.add caveats

proc validate*(key: openarray[byte]; r: SturdyRef): bool =
  var sig = hmacSha256(key, r.oid.encode, key.len)
  for a in r.caveatChain:
    sig = hmacSha256(sig, a.encode)
  r.sig == sig

when isMainModule:
  import
    unittest

  import
    preserves / parse

  test "sturdy":
    var
      key: array[16, byte]
      oid = "syndicate".toPreserve
      sRef = mint(key, oid)
      control = parsePreserves"""<ref "syndicate" [] #[pkgN9TBmEd3Q04grVG4Zdw]>"""
    check(sRef.toPreserve == control)
    let aRef = attenuate(sRef, newSeq[Caveat]())
    check validate(key, aRef)