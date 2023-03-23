# SPDX-License-Identifier: MIT

## Module for peering with remote dataspaces over network.
import
  std / [asyncdispatch, net, options, streams, tables]

import
  preserves

import
  ./actors, ./durings, ./relays, ./protocols / protocol

import
  taps

export
  `$`

type
  Turn = actors.Turn
  Assertion = Preserve[Ref]
  Value = Preserve[void]
proc connectTcp(remote: RemoteSpecifier): Connection =
  var transport = newTransportProperties()
  transport.require("reliability")
  transport.require("preserve-order")
  var preConn = newPreConnection(transport = some transport, remote = @[remote])
  preconn.initiate()

proc connectNet*(turn: var Turn; remote: RemoteSpecifier; cap: SturdyRef;
                 bootProc: ConnectProc) =
  let
    facet = turn.facet
    reenable = facet.preventInertCheck()
    connectionClosedRef = newRef(turn, ShutdownEntity())
    conn = connectTcp(remote)
  conn.onReadydo :(discard bootActor("net")do (turn: var Turn):
    var shutdownRef: Ref
    proc tapsWriter(pkt: sink Packet): Future[void] =
      let fut = newFuture[void]("tapsWriter")
      send(conn, encode(pkt))
      onSent(conn)do (ctx: MessageContext):
        complete(fut)
      onSendError(conn)do (ctx: MessageContext; reason: ref Exception):
        fail(fut, reason)
      fut

    var ops = RelayActorOptions(packetWriter: tapsWriter, initialOid: 0.Oid.some)
    let relayFut = spawnRelay("net", turn, ops)do (turn: var Turn; relay: Relay):
      let facet = turn.facet
      facet.actor.atExitdo (turn: var Turn):
        close(conn)
      conn.onConnectionErrordo (reason: ref Exception):
        terminate(facet, reason)
      conn.onReceiveErrordo (ctx: MessageContext; reason: ref Exception):
        terminate(facet, reason)
      conn.onCloseddo :
        run(facet)do (turn: var Turn):
          stopActor(turn)
      var wireBuf = newBufferedDecoder()
      conn.onReceiveddo (buf: seq[byte]; ctx: MessageContext):
        feed(wireBuf, buf)
        var (success, pr) = decode(wireBuf)
        if success:
          dispatch(relay, pr)
        receive(conn)
      receive(conn)
      discard publish(turn, connectionClosedRef, true)
      shutdownRef = newRef(turn, ShutdownEntity())
    relayFut.addCallbackdo (refFut: Future[Ref]):
      let gatekeeper = read refFut
      run(gatekeeper.relay)do (turn: var Turn):
        reenable()
        discard publish(turn, shutdownRef, true)
        proc duringCallback(turn: var Turn; a: Assertion; h: Handle): TurnAction =
          let facet = facet(turn)do (turn: var Turn):
            bootProc(turn, unembed a)
          proc action(turn: var Turn) =
            stop(turn, facet)

          result = action

        var res = Resolve(sturdyref: cap,
                          observer: newRef(turn, during(duringCallback)))
        discard publish(turn, gatekeeper, res))

proc connectNet*(turn: var Turn; host: string; port: Port; cap: SturdyRef;
                 bootProc: ConnectProc) =
  var remote = newRemoteEndpoint()
  remote.with(port)
  if isIpAddress host:
    remote.with(parseIpAddress(host))
  else:
    remote.withHostname(host)
  connectNet(turn, remote, cap, bootProc)
