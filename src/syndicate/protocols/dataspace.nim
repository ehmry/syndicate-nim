# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves, dataspacePatterns

type
  Observe*[E] {.preservesRecord: "Observe".} = ref object
  
proc `$`*[E](x: Observe[E]): string =
  `$`(toPreserve(x, E))

proc encode*[E](x: Observe[E]): seq[byte] =
  encode(toPreserve(x, E))
