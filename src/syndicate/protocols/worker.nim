# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  Instance*[E] {.preservesRecord: "Instance".} = ref object
  
proc `$`*[E](x: Instance[E]): string =
  `$`(toPreserve(x, E))

proc encode*[E](x: Instance[E]): seq[byte] =
  encode(toPreserve(x, E))
