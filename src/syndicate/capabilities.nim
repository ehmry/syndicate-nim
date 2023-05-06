# SPDX-License-Identifier: MIT

import
  preserves

import
  ./protocols / sturdy, ./private / hmacs

from ./actors import Ref

export
  `$`

proc mint*[T](key: openarray[byte]; oid: Preserve[T]): SturdyRef[T] =
  SturdyRef[T](oid: oid, sig: hmacSha256(key, encode(oid), key.len))

proc mint*[T](key: openarray[byte]; oid: T; E = void): SturdyRef[E] =
  var oidPr = toPreserve(oid, E)
  SturdyRef[E](oid: oidPr, sig: hmacSha256(key, encode(oidPr), key.len))

proc mint*(): SturdyRef[Ref] =
  var key: array[16, byte]
  cast[SturdyRef[Ref]](mint(key, "syndicate", Ref))

proc attenuate*[T](r: SturdyRef[T]; caveats: Attenuation): SturdyRef[T] =
  result = SturdyRef[T](oid: r.oid, caveatChain: r.caveatChain,
                        sig: hmacSha256(r.sig, caveats.encode))
  result.caveatChain.add caveats

proc validate*[T](key: openarray[byte]; r: SturdyRef[T]): bool =
  var sig = hmacSha256(key, r.oid.encode, key.len)
  for a in r.caveatChain:
    sig = hmacSha256(sig, a.encode)
  r.sig == sig

when isMainModule:
  from os import commandLineParams

  var key: array[16, byte]
  case readBytes(stdin, key, 0, 16)
  of 16:
    discard
  of 0:
    stderr.writeLine "using null key"
  else:
    quit "expected sixteen bytes of key from stdin"
  var oids: seq[Preserve[void]]
  for p in commandLineParams():
    add(oids, parsePreserves p)
  if oids.len == 0:
    oids.add(toPreserve "syndicate")
  for oid in oids:
    let sturdy = mint(key, oid)
    doAssert validate(key, sturdy)
    stdout.writeLine(sturdy)