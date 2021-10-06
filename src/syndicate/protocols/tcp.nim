# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  TcpRemote* {.record: "tcp-remote".} = ref object ## ``<tcp-remote @host string @port int>``
  
  TcpLocal* {.record: "tcp-local".} = ref object ## ``<tcp-local @host string @port int>``
  
  TcpPeerInfo*[E = void] {.record: "tcp-peer".} = ref object ## ``<tcp-peer @handle #!any @local TcpLocal @remote TcpRemote>``
  
proc `tcpRemote`*[E = void](`host`: string | Preserve[E];
                            `port`: BiggestInt | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``TcpRemote``.
  initRecord[E](symbol[E]("tcp-remote"), toPreserve(`host`, E),
                toPreserve(`port`, E))

proc toPreserveHook*(`tcpremote`: TcpRemote; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("tcp-remote"), toPreserve(`tcpremote`.`host`, E),
                toPreserve(`tcpremote`.`port`, E))

proc `tcpLocal`*[E = void](`host`: string | Preserve[E];
                           `port`: BiggestInt | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``TcpLocal``.
  initRecord[E](symbol[E]("tcp-local"), toPreserve(`host`, E),
                toPreserve(`port`, E))

proc toPreserveHook*(`tcplocal`: TcpLocal; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("tcp-local"), toPreserve(`tcplocal`.`host`, E),
                toPreserve(`tcplocal`.`port`, E))

proc `tcpPeerInfo`*[E = void](`handle`: Preserve[E];
                              `local`: TcpLocal | Preserve[E];
                              `remote`: TcpRemote | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``TcpPeerInfo``.
  initRecord[E](symbol[E]("tcp-peer"), toPreserve(`handle`, E),
                toPreserve(`local`, E), toPreserve(`remote`, E))

proc toPreserveHook*(`tcppeerinfo`: TcpPeerInfo; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("tcp-peer"), toPreserve(`tcppeerinfo`.`handle`, E),
                toPreserve(`tcppeerinfo`.`local`, E),
                toPreserve(`tcppeerinfo`.`remote`, E))

proc `$`*[E](x: TcpRemote | TcpLocal | TcpPeerInfo[E]): string =
  `$`(toPreserve(x, E))

proc `encode`*[E](x: TcpRemote | TcpLocal | TcpPeerInfo[E]): seq[byte] =
  encode(toPreserve(x, E))
