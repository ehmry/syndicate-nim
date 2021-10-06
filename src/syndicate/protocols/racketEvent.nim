# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  RacketEvent*[E = void] {.record: "racket-event".} = ref object ## ``<racket-event @source #!any @event #!any>``
  
proc `racketEvent`*[E = void](`source`: Preserve[E]; `event`: Preserve[E]): Preserve[
    E] =
  ## Preserves constructor for ``RacketEvent``.
  initRecord[E](symbol[E]("racket-event"), toPreserve(`source`, E),
                toPreserve(`event`, E))

proc toPreserveHook*(`racketevent`: RacketEvent; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("racket-event"),
                toPreserve(`racketevent`.`source`, E),
                toPreserve(`racketevent`.`event`, E))

proc `$`*[E](x: RacketEvent[E]): string =
  `$`(toPreserve(x, E))

proc `encode`*[E](x: RacketEvent[E]): seq[byte] =
  encode(toPreserve(x, E))
