# SPDX-License-Identifier: MIT

import
  preserves, preserves / records

const
  Discard* = RecordClass(label: symbol"discard", arity: 0)
  Capture* = RecordClass(label: symbol"capture", arity: 1)
  Observe* = RecordClass(label: symbol"observe", arity: 1)
  `? _`* = initRecord("discard")
  `?*`* = Capture % `? _`