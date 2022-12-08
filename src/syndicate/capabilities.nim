# SPDX-License-Identifier: MIT

import
  preserves

import
  ./protocols / sturdy, ./private / hmacs

proc mint*[T](key: openarray[byte]; oid: Preserve[T]): SturdyRef[T] =
  SturdyRef[T](oid: oid, sig: hmacSha256(key, encode(oid), key.len))

proc mint*[T](key: openarray[byte]; oid: T; E = void): SturdyRef[E] =
  var oidPr = toPreserve(oid, E)
  SturdyRef[E](oid: oidPr, sig: hmacSha256(key, encode(oidPr), key.len))

proc attenuate*[T](r: SturdyRef[T]; caveats: Attenuation): SturdyRef[T] =
  result = SturdyRef[T](oid: r.oid, caveatChain: r.caveatChain,
                        sig: hmacSha256(r.sig, caveats.encode))
  result.caveatChain.add caveats

proc validate*[T](key: openarray[byte]; r: SturdyRef[T]): bool =
  var sig = hmacSha256(key, r.oid.encode, key.len)
  for a in r.caveatChain:
    sig = hmacSha256(sig, a.encode)
  r.sig == sig
