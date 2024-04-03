# SPDX-License-Identifier: MIT

import
  taps

import
  solo5

import
  syndicate, syndicate / relays

import
  preserves

acquireDevices([("relay", netBasic)], netAcquireHook)
type
  Netif {.preservesRecord: "netif".} = object
  
proc spawnNetifActor(turn: var Turn; ds: Cap) =
  spawnActor(turn, "netif")do (turn: var Turn):
    let facet = turn.facet
    onInterfaceUpdo (device: string; ip: IpAddress):
      run(facet)do (turn: var Turn):
        if not ip.isLinkLocal:
          discard publish(turn, ds, Netif(device: device, ipAddr: $ip))

runActor("relay-test")do (turn: var Turn):
  let root = turn.facet
  onStop(turn)do (turn: var Turn):
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
      resolve(turn, ds, route)do (turn: var Turn; ds: Cap):
        echo "route resolved!"
        echo "stopping root facet"
        stop(turn, root)