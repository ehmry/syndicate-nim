# SPDX-License-Identifier: MIT

import
  preserves

const
  Discard* = RecordClass(label: symbol"discard", arity: 0)
  Capture* = RecordClass(label: symbol"capture", arity: 1)
  Observe* = RecordClass(label: symbol"observe", arity: 1)
  Inbound* = RecordClass(label: symbol"inbound", arity: 1)
  Outbound* = RecordClass(label: symbol"outbound", arity: 1)
  Instance* = RecordClass(label: symbol"instance", arity: 1)