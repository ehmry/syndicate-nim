# SPDX-License-Identifier: MIT

import
  std / times

import
  syndicate, syndicate / drivers / timers

runActor("timer-test")do (turn: var Turn):
  let timers = newDataspace(turn)
  spawnTimerDriver(turn, timers)
  onPublish(turn, timers, ?LaterThan(seconds: 1356100000)):
    echo "now in 13th bʼakʼtun"
  after(turn, timers, initDuration(seconds = 3))do (turn: var Turn):
    echo "third timer expired"
    stopActor(turn)
  after(turn, timers, initDuration(seconds = 1))do (turn: var Turn):
    echo "first timer expired"
  after(turn, timers, initDuration(seconds = 2))do (turn: var Turn):
    echo "second timer expired"