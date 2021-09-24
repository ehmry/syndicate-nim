# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  RacketEvent*[E] {.preservesRecord: "racket-event".} = ref object
  
proc `$`*[E](x: RacketEvent[E]): string =
  `$`(toPreserve(x, E))

proc encode*[E](x: RacketEvent[E]): seq[byte] =
  encode(toPreserve(x, E))
