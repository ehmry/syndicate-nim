# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  Says* {.preservesRecord: "Says".} = object
  
  Present* {.preservesRecord: "Present".} = object
  
proc `$`*(x: Says | Present): string =
  `$`(toPreserve(x))

proc encode*(x: Says | Present): seq[byte] =
  encode(toPreserve(x))
