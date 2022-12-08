# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  TcpLocal* {.preservesRecord: "tcp-local".} = object
  
  TcpPeerInfo*[Cap] {.preservesRecord: "tcp-peer".} = object
  
  TcpRemote* {.preservesRecord: "tcp-remote".} = object
  
proc `$`*[Cap](x: TcpPeerInfo[Cap]): string =
  `$`(toPreserve(x, Cap))

proc encode*[Cap](x: TcpPeerInfo[Cap]): seq[byte] =
  encode(toPreserve(x, Cap))

proc `$`*(x: TcpLocal | TcpRemote): string =
  `$`(toPreserve(x))

proc encode*(x: TcpLocal | TcpRemote): seq[byte] =
  encode(toPreserve(x))
