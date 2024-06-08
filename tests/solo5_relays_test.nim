# SPDX-License-Identifier: MIT

import
  std / solo5, pkg / taps, pkg / preserves, syndicate, syndicate / relays

acquireDevices([("relay", netBasic)], netAcquireHook)
type
  Netif {.preservesRecord: "netif".} = object
  
proc spawnNetifActor(turn: Turn; ds: Cap) =
  spawnActor(turn, "netif")do (turn: Turn):
    let facet = turn.facet
    onInterfaceUpdo (device: string; ip: IpAddress):
      run(facet)do (turn: Turn):
        if not ip.isLinkLocal:
          discard publish(turn, ds, Netif(device: device, ipAddr: $ip))

runActor("relay-test")do (turn: Turn):
  let root = turn.facet
  onStop(turn)do (turn: Turn):
    quit()
  let ds = newDataspace(turn)
  spawnNetifActor(turn, ds)
  spawnRelays(turn, ds)
  var
    route: Route
    pr = parsePreserves $solo5_start_info.cmdline
  if route.fromPreserves pr:
    echo "parsed route ", route.toPreserves
    during(turn, ds, Netif ?: {1: grab()})do (ip: string):
      echo "Acquired address ", ip
      resolve(turn, ds, route)do (turn: Turn; ds: Cap):
        echo "route resolved!"
        echo "stopping root facet"
        stop(turn, root)