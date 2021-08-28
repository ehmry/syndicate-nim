# SPDX-License-Identifier: MIT

## Date of generation: 2021-08-28 10:14
import
  std / typetraits, preserves

type
  BoxState* {.record: "box-state".} = object ## ``<box-state @value int>``
  
  SetBox* {.record: "set-box".} = object ## ``<set-box @value int>``
  
proc prsBoxState*(value: Preserve | BiggestInt): Preserve =
  initRecord(symbol("box-state"), value)

proc prsSetBox*(value: Preserve | BiggestInt): Preserve =
  initRecord(symbol("set-box"), value)
