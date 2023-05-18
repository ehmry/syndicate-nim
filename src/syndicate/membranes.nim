# SPDX-License-Identifier: MIT

import
  std / [hashes, tables]

from ./actors import Ref, hash

from ./protocols / sturdy import Oid

proc hash(r: Ref): Hash =
  !$(r.relay.hash !& r.target.unsafeAddr.hash)

type
  Membrane* = object
    ## Bidirectional mapping between `Oid` and `Ref` values.
    ## https://synit.org/book/protocol.html#membranes
  
  WireSymbol* = ref object
  
proc oid*(sym: WireSymbol): Oid =
  sym.oid

proc `ref`*(sym: WireSymbol): Ref =
  sym.ref

proc grab*(mem: Membrane; key: Oid): WireSymbol =
  ## Grab a `WireSymbol` from a `Membrane`.
  mem.byOid.getOrDefault(key)

proc grab*(mem: Membrane; key: Ref): WireSymbol =
  ## Grab a `WireSymbol` from a `Membrane`.
  mem.byRef.getOrDefault(key)

proc drop*(mem: var Membrane; sym: WireSymbol) =
  ## Drop a `WireSymbol` from a `Membrane`.
  dec sym.count
  if sym.count < 1:
    mem.byOid.del sym.oid
    mem.byRef.del sym.`ref`

proc newWireSymbol*(mem: var Membrane; o: Oid; r: Ref): WireSymbol =
  ## Allocate a `WireSymbol` at a `Membrane`.
  result = WireSymbol(oid: o, `ref`: r, count: 1)
  mem.byOid[result.oid] = result
  mem.byRef[result.`ref`] = result
