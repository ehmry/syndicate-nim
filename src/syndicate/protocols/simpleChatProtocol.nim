# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves

type
  Present* {.record: "Present".} = ref object ## ``<Present @username string>``
  
  Says* {.record: "Says".} = ref object ## ``<Says @who string @what string>``
  
proc `present`*[E = void](`username`: string | Preserve[E]): Preserve[E] =
  ## Preserves constructor for ``Present``.
  initRecord[E](symbol[E]("Present"), toPreserve(`username`, E))

proc toPreserveHook*(`present`: Present; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("Present"), toPreserve(`present`.`username`, E))

proc `says`*[E = void](`who`: string | Preserve[E]; `what`: string | Preserve[E]): Preserve[
    E] =
  ## Preserves constructor for ``Says``.
  initRecord[E](symbol[E]("Says"), toPreserve(`who`, E), toPreserve(`what`, E))

proc toPreserveHook*(`says`: Says; E: typedesc): Preserve[E] =
  initRecord[E](symbol[E]("Says"), toPreserve(`says`.`who`, E),
                toPreserve(`says`.`what`, E))

proc `$`*[E](x: Present | Says): string =
  `$`(toPreserve(x, E))

proc `encode`*[E](x: Present | Says): seq[byte] =
  encode(toPreserve(x, E))
