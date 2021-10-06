# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves, sturdy

type
  Resolve*[E = void] {.record: "resolve".} = ref object ## ``<resolve @sturdyref sturdy.SturdyRef @observer #!#!any>``
  
  Bind*[E = void] {.record: "bind".} = ref object ## ``<bind @oid any @key bytes @target #!any>``
  
proc `resolve`*[E = void](`sturdyref`: sturdy.SturdyRef | Preserve[E];
                          `observer`: Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``Resolve``.
  initRecord[E](symbol[E]("resolve"), toPreserve(`sturdyref`, E),
                toPreserve(`observer`, E))

proc toPreserveHook*(`resolve`: Resolve; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("resolve"), toPreserve(`resolve`.`sturdyref`, E),
                toPreserve(`resolve`.`observer`, E))

proc `bind`*[E = void](`oid`: Preserve[E]; `key`: seq[byte] | Preserve[E];
                       `target`: Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``Bind``.
  initRecord[E](symbol[E]("bind"), toPreserve(`oid`, E), toPreserve(`key`, E),
                toPreserve(`target`, E))

proc toPreserveHook*(`bind`: Bind; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("bind"), toPreserve(`bind`.`oid`, E),
                toPreserve(`bind`.`key`, E), toPreserve(`bind`.`target`, E))

proc `$`*[E](x: Resolve[E] | Bind[E]): string =
  `$`(toPreserve(x, E))

proc `encode`*[E](x: Resolve[E] | Bind[E]): seq[byte] =
  encode(toPreserve(x, E))
