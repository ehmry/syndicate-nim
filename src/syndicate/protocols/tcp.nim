# SPDX-License-Identifier: MIT

import
  preserves

type
  TcpLocal* {.preservesRecord: "tcp-local".} = object
  
  TcpPeerInfo* {.preservesRecord: "tcp-peer".} = object
  
  TcpRemote* {.preservesRecord: "tcp-remote".} = object
  
proc `$`*(x: TcpLocal | TcpPeerInfo | TcpRemote): string =
  `$`(toPreserves(x))

proc encode*(x: TcpLocal | TcpPeerInfo | TcpRemote): seq[byte] =
  encode(toPreserves(x))
