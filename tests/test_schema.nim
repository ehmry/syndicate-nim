# SPDX-License-Identifier: MIT

import
  preserves

type
  Foo* {.preservesRecord: "foo".} = object
  
proc `$`*(x: Foo): string =
  `$`(toPreserve(x))

proc encode*(x: Foo): seq[byte] =
  encode(toPreserve(x))
