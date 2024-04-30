# SPDX-License-Identifier: MIT

import
  std / times

import
  solo5

import
  syndicate, syndicate / drivers / timers

acquireDevices()
runActor("timer-test")do (turn: Turn):
  let timers = newDataspace(turn)
  spawnTimerDriver(turn, timers)
  onPublish(turn, timers, ?LaterThan(seconds: 1356100000)):
    echo "now in 13th bʼakʼtun"
  after(turn, timers, initDuration(seconds = 3))do (turn: Turn):
    echo "third timer expired"
    stopActor(turn)
  after(turn, timers, initDuration(seconds = 1))do (turn: Turn):
    echo "first timer expired"
  after(turn, timers, initDuration(seconds = 2))do (turn: Turn):
    echo "second timer expired"