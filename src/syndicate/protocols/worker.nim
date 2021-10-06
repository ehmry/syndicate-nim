# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  Instance*[E = void] {.record: "Instance".} = ref object ## ``<Instance @name string @argument any>``
  
proc `instance`*[E = void](`name`: string | Preserve[E]; `argument`: Preserve[E]): Preserve[
    E] =
  ## Preserves constructor for ``Instance``.
  initRecord[E](symbol[E]("Instance"), toPreserve(`name`, E),
                toPreserve(`argument`, E))

proc toPreserveHook*(`instance`: Instance; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("Instance"), toPreserve(`instance`.`name`, E),
                toPreserve(`instance`.`argument`, E))

proc `$`*[E](x: Instance[E]): string =
  `$`(toPreserve(x, E))

proc `encode`*[E](x: Instance[E]): seq[byte] =
  encode(toPreserve(x, E))
