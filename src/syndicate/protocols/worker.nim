# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  Instance*[Cap] {.preservesRecord: "Instance".} = object
  
proc `$`*[Cap](x: Instance[Cap]): string =
  `$`(toPreserve(x, Cap))

proc encode*[Cap](x: Instance[Cap]): seq[byte] =
  encode(toPreserve(x, Cap))
