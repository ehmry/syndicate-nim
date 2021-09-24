# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  TcpLocal* {.preservesRecord: "tcp-local".} = object
  
  TcpPeerInfo*[E] {.preservesRecord: "tcp-peer".} = ref object
  
  TcpRemote* {.preservesRecord: "tcp-remote".} = object
  
proc `$`*[E](x: TcpPeerInfo[E]): string =
  `$`(toPreserve(x, E))

proc encode*[E](x: TcpPeerInfo[E]): seq[byte] =
  encode(toPreserve(x, E))

proc `$`*(x: TcpLocal | TcpRemote): string =
  `$`(toPreserve(x))

proc encode*(x: TcpLocal | TcpRemote): seq[byte] =
  encode(toPreserve(x))
