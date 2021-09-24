# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  WebSocket* {.preservesRecord: "ws".} = object
  
  Stdio* {.preservesRecord: "stdio".} = object
  Unix* {.preservesRecord: "unix".} = object
  
  Tcp* {.preservesRecord: "tcp".} = object
  
proc `$`*(x: WebSocket | Stdio | Unix | Tcp): string =
  `$`(toPreserve(x))

proc encode*(x: WebSocket | Stdio | Unix | Tcp): seq[byte] =
  encode(toPreserve(x))
