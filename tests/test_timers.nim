# SPDX-License-Identifier: MIT

import
  std / times, pkg / preserves, syndicate, syndicate / drivers / timers

var passCount = 0
runActor("timer-test")do (turn: Turn):
  let timers = newDataspace(turn)
  spawnTimerDriver(turn, timers)
  onPublish(turn, timers, ?LaterThan(seconds: 1356100000)):
    echo "now in 13th bʼakʼtun"
    inc passCount
  after(turn, timers, initDuration(seconds = 3))do (turn: Turn):
    echo "third timer expired"
    assert passCount == 3
    inc passCount
  after(turn, timers, initDuration(seconds = 1))do (turn: Turn):
    echo "first timer expired"
    assert passCount == 1
    inc passCount
  after(turn, timers, initDuration(seconds = 2))do (turn: Turn):
    echo "second timer expired"
    assert passCount == 2
    inc passCount
doAssert passCount == 4, $passCount