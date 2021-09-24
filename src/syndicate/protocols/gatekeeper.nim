# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves, sturdy

type
  Bind*[E] {.preservesRecord: "bind".} = ref object
  
  Resolve*[E] {.preservesRecord: "resolve".} = ref object
  
proc `$`*[E](x: Bind[E] | Resolve[E]): string =
  `$`(toPreserve(x, E))

proc encode*[E](x: Bind[E] | Resolve[E]): seq[byte] =
  encode(toPreserve(x, E))
