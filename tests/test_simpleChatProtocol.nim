# SPDX-License-Identifier: MIT

import
  std / asyncdispatch

import
  syndicate

import
  syndicate / protocols / simpleChatProtocol

bootDataspace("main")do (ds: Ref; turn: var Turn):
  let me = "user_a"
  let h = publish(turn, ds, Present(username: me))
  message(turn, ds, Says(who: me, what: "goodbye"))
  onPublish(turn, ds, ?Present)do (username: string):
    echo "presence of ", username, " asserted"
  onMessage(turn, ds, ?Says)do (who: string; what: string):
    echo who, " says ", what
    retract(turn, h)
    echo "retracted something"
  during(turn, ds, ?Present)do (username: string):
    echo username, " arrived"
    retract(turn, h)
  do:
    echo "someone departed"
    quit()
runForever()