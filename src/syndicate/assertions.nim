# SPDX-License-Identifier: MIT

import
  preserves, preserves / records

const
  Discard* = RecordClass(label: symbol"discard", arity: 0)
  Capture* = RecordClass(label: symbol"capture", arity: 1)
  Observe* = RecordClass(label: symbol"observe", arity: 1)
  `? _`* = initRecord("discard")
  `?*`* = Capture % `? _`
proc captureCount*(pattern: Preserve): int =
  if Capture.isClassOf pattern:
    result = 1
  else:
    for e in pattern.items:
      result.inc captureCount(e)
