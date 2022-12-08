# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves, dataspacePatterns

type
  Observe*[Cap] {.preservesRecord: "Observe".} = ref object
  
proc `$`*[Cap](x: Observe[Cap]): string =
  `$`(toPreserve(x, Cap))

proc encode*[Cap](x: Observe[Cap]): seq[byte] =
  encode(toPreserve(x, Cap))
