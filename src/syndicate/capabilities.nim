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
  result.parameters.oid = oid
  result.parameters.sig = hmac(key, oid.encode)

proc mint*(): SturdyRef =
  var key: array[16, byte]
  mint(key, "syndicate".toPreserves)

proc attenuate*(r: SturdyRef; caveats: seq[Caveat]): SturdyRef =
  if r.parameters.caveats.isSome:
    result.parameters.caveats = some(r.parameters.caveats.get &
        caveats.toPreserves)
  result.parameters.oid = r.parameters.oid
  result.parameters.sig = hmac(r.parameters.sig, caveats.toPreserves.encode)

proc validate*(key: openarray[byte]; sturdy: SturdyRef): bool =
  var sig = hmac(key, sturdy.parameters.oid.encode)
  if sturdy.parameters.caveats.isSome:
    for cav in sturdy.parameters.caveats.get:
      sig = hmac(sig, encode cav)
  result = (sig == sturdy.parameters.sig)
