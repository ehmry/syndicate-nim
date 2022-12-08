# SPDX-License-Identifier: MIT

import
  std / [streams, strutils, unittest]

import
  preserves

import
  syndicate / relays

import
  syndicate / protocols / [protocol, sturdy]

type
  WireRef = sturdy.WireRef[void]
  WireAssertion = Preserve[WireRef]
  Event = protocol.Event[WireRef]
  TurnEvent = protocol.TurnEvent[WireRef]
  Packet = protocol.Packet[WireRef]
suite "protocols":
  test "PDiscard":
    var pd: PDiscard
    check $pd == "<_>"
  test "stuff":
    const
      data = parseHexStr "b5b590b4b306617373657274b4b3074f627365727665b4b303726563b3067274742d6d73b5b4b3036c697483406805604189374c84b4b3036c6974b3062e3134342e3184b4b30462696e64b4b3015f8484b4b30462696e64b4b3015f8484b4b30462696e64b4b3015f8484b4b30462696e64b4b3015f8484848486b590908484a2132403848484b5b590b4b30772657472616374a2132403848484"
    var str = newStringStream(data)
    while not str.atEnd:
      var pos = str.getPosition
      echo "decode position: ", pos
      try:
        var a = decodePreserves(str, WireRef)
        echo a
      except:
        str.setPosition pos
        echo str.readAll.toHex
        break