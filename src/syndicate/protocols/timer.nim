# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  SetTimer*[E = void] {.record: "set-timer".} = ref object ## ``<set-timer @label any @msecs double @kind TimerKind>``
  
  TimerExpired*[E = void] {.record: "timer-expired".} = ref object ## ``<timer-expired @label any @msecs double>``
  
  TimerKind* {.pure.} = enum ## ``/ =<<lit>relative> / =<<lit>absolute> / =<<lit>clear>``
    `relative`, `absolute`, `clear`
  LaterThan* {.record: "later-than".} = ref object ## ``<later-than @msecs double>``
  
proc `setTimer`*[E = void](`label`: Preserve[E]; `msecs`: float64 | Preserve[E];
                           `kind`: TimerKind | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``SetTimer``.
  initRecord[E](symbol[E]("set-timer"), toPreserve(`label`, E),
                toPreserve(`msecs`, E), toPreserve(`kind`, E))

proc toPreserveHook*(`settimer`: SetTimer; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("set-timer"), toPreserve(`settimer`.`label`, E),
                toPreserve(`settimer`.`msecs`, E),
                toPreserve(`settimer`.`kind`, E))

proc `timerExpired`*[E = void](`label`: Preserve[E];
                               `msecs`: float64 | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``TimerExpired``.
  initRecord[E](symbol[E]("timer-expired"), toPreserve(`label`, E),
                toPreserve(`msecs`, E))

proc toPreserveHook*(`timerexpired`: TimerExpired; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("timer-expired"),
                toPreserve(`timerexpired`.`label`, E),
                toPreserve(`timerexpired`.`msecs`, E))

proc toPreserveHook*(v: TimerKind; E: typedesc): Preserve[E] =
  case v
  of TimerKind.`relative`:
    symbol[E]("relative")
  of TimerKind.`absolute`:
    symbol[E]("absolute")
  of TimerKind.`clear`:
    symbol[E]("clear")

proc fromPreserveHook*[E](v: var TimerKind; pr: Preserve[E]): bool =
  if isSymbol(pr):
    case pr.symbol
    of "relative":
      v = TimerKind.`relative`
      result = true
    of "absolute":
      v = TimerKind.`absolute`
      result = true
    of "clear":
      v = TimerKind.`clear`
      result = true

proc `laterThan`*[E = void](`msecs`: float64 | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``LaterThan``.
  initRecord[E](symbol[E]("later-than"), toPreserve(`msecs`, E))

proc toPreserveHook*(`laterthan`: LaterThan; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("later-than"), toPreserve(`laterthan`.`msecs`, E))

proc `$`*[E](x: SetTimer[E] | TimerExpired[E] | TimerKind | LaterThan): string =
  `$`(toPreserve(x, E))

proc `encode`*[E](x: SetTimer[E] | TimerExpired[E] | TimerKind | LaterThan): seq[
    byte] =
  encode(toPreserve(x, E))
