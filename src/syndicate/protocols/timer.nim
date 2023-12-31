# SPDX-License-Identifier: MIT

import
  preserves

type
  TimerExpired* {.preservesRecord: "timer-expired".} = object
  
  SetTimer* {.preservesRecord: "set-timer".} = object
  
  `TimerKind`* {.preservesOr, pure.} = enum
    `relative`, `absolute`, `clear`
  LaterThan* {.preservesRecord: "later-than".} = object
  
proc `$`*(x: TimerExpired | SetTimer | LaterThan): string =
  `$`(toPreserves(x))

proc encode*(x: TimerExpired | SetTimer | LaterThan): seq[byte] =
  encode(toPreserves(x))
