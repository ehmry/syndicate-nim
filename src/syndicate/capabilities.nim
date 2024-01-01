# SPDX-License-Identifier: MIT

runnableExamples:
  from std / unittest import check

  let sturdy = mint()
  check $sturdy ==
      """<ref {oid: "syndicate" sig: #x"69ca300c1dbfa08fba692102dd82311a"}>"""
import
  std / [options, tables]

from std / sequtils import toSeq

import
  hashlib / misc / blake2

import
  preserves

import
  ./protocols / sturdy

export
  `$`

proc hmac(key, data: openarray[byte]): seq[byte] =
  count[Hmac[BLAKE2S_256]](key, data).data[0 .. 15].toSeq

proc mint*(key: openarray[byte]; oid: Value): SturdyRef =
  SturdyRef(parameters: {Symbol"oid": oid,
                         Symbol"sig": hmac(key, encode(oid)).toPreserves}.toTable)

proc mint*(): SturdyRef =
  var key: array[16, byte]
  mint(key, "syndicate".toPreserves)

proc attenuate*(r: SturdyRef; caveats: seq[Caveat]): SturdyRef =
  var sig = hmac(r.parameters[Symbol"sig"].bytes, caveats.toPreserves.encode)
  result = SturdyRef(parameters: {Symbol"oid": r.parameters[Symbol"oid"], Symbol"caveats": r.parameters[
      Symbol"caveats"] &
      caveats.toPreserves, Symbol"sig": sig.toPreserves}.toTable)

proc validate*(key: openarray[byte]; sturdy: SturdyRef): bool =
  let oid = sturdy.parameters[Symbol"oid"]
  let ctrl = sturdy.parameters[Symbol"sig"]
  var sig = hmac(key, oid.encode)
  let caveats = sturdy.parameters[Symbol"caveats"]
  for cav in caveats.sequence:
    sig = hmac(sig, encode cav)
  result = (sig == ctrl.bytes)
