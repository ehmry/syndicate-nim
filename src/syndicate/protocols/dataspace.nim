# SPDX-License-Identifier: MIT

import
  preserves, dataspacePatterns

type
  Observe* {.preservesRecord: "Observe".} = object
  
proc `$`*(x: Observe): string =
  `$`(toPreserves(x))

proc encode*(x: Observe): seq[byte] =
  encode(toPreserves(x))
