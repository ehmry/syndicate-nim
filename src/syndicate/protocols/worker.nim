# SPDX-License-Identifier: MIT

import
  preserves

type
  Instance* {.preservesRecord: "Instance".} = object
  
proc `$`*(x: Instance): string =
  `$`(toPreserves(x))

proc encode*(x: Instance): seq[byte] =
  encode(toPreserves(x))
