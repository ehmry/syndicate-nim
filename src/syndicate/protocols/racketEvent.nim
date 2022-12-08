# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  RacketEvent* {.preservesRecord: "racket-event".} = object
  
proc `$`*(x: RacketEvent): string =
  `$`(toPreserve(x))

proc encode*(x: RacketEvent): seq[byte] =
  encode(toPreserve(x))
