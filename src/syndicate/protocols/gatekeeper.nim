# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves, sturdy

type
  Bind*[Cap] {.preservesRecord: "bind".} = object
  
  Resolve*[Cap] {.preservesRecord: "resolve".} = ref object
  
proc `$`*[Cap](x: Bind[Cap] | Resolve[Cap]): string =
  `$`(toPreserve(x, Cap))

proc encode*[Cap](x: Bind[Cap] | Resolve[Cap]): seq[byte] =
  encode(toPreserve(x, Cap))
