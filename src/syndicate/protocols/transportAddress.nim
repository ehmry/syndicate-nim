# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  Tcp* {.record: "tcp".} = ref object ## ``<tcp @host string @port int>``
  
  Unix* {.record: "unix".} = ref object ## ``<unix @path string>``
  
  WebSocket* {.record: "ws".} = ref object ## ``<ws @url string>``
  
  Stdio* {.record: "stdio".} = object ## ``<stdio>``
    nil

proc `tcp`*[E = void](`host`: string | Preserve[E];
                      `port`: BiggestInt | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``Tcp``.
  initRecord[E](symbol[E]("tcp"), toPreserve(`host`, E), toPreserve(`port`, E))

proc toPreserveHook*(`tcp`: Tcp; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("tcp"), toPreserve(`tcp`.`host`, E),
                toPreserve(`tcp`.`port`, E))

proc `unix`*[E = void](`path`: string | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``Unix``.
  initRecord[E](symbol[E]("unix"), toPreserve(`path`, E))

proc toPreserveHook*(`unix`: Unix; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("unix"), toPreserve(`unix`.`path`, E))

proc `webSocket`*[E = void](`url`: string | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``WebSocket``.
  initRecord[E](symbol[E]("ws"), toPreserve(`url`, E))

proc toPreserveHook*(`websocket`: WebSocket; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("ws"), toPreserve(`websocket`.`url`, E))

proc `stdio`*[E = void](): Preserve[E] =
  ## Preserves constructor for ``Stdio``.
  initRecord[E](symbol[E]("stdio"))

proc toPreserveHook*(`stdio`: Stdio; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("stdio"))

proc `$`*[E](x: Tcp | Unix | WebSocket | Stdio): string =
  `$`(toPreserve(x, E))

proc `encode`*[E](x: Tcp | Unix | WebSocket | Stdio): seq[byte] =
  encode(toPreserve(x, E))
