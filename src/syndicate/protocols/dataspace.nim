# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves, dataspacePatterns

type
  Observe*[E = void] {.record: "Observe".} = ref object ## ``<Observe @pattern dataspacePatterns.Pattern @observer #!any>``
  
proc `observe`*[E = void](`pattern`: dataspacePatterns.Pattern | Preserve[E];
                          `observer`: Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``Observe``.
  initRecord[E](symbol[E]("Observe"), toPreserve(`pattern`, E),
                toPreserve(`observer`, E))

proc toPreserveHook*(`observe`: Observe; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("Observe"), toPreserve(`observe`.`pattern`, E),
                toPreserve(`observe`.`observer`, E))

proc `$`*[E](x: Observe[E]): string =
  `$`(toPreserve(x, E))

proc `encode`*[E](x: Observe[E]): seq[byte] =
  encode(toPreserve(x, E))
