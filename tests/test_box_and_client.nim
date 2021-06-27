# SPDX-License-Identifier: MIT

import
  syndicate / assertions, syndicate / dataspaces, syndicate / events,
  syndicate / skeletons

import
  preserves, preserves / records

import
  asyncdispatch, tables, options

const
  N = 100000
const
  `? _` = init(Discard)
  `?$` = init(Capture, `? _`)
  BoxState = RecordClass(label: symbol"BoxState", arity: 1)
  SetBox = RecordClass(label: symbol"SetBox", arity: 1)
proc boot(facet: Facet) =
  facet.spawn("box")do (facet: Facet):
    facet.declareField(value, int, 0)
    discard facet.addEndpointdo (facet: Facet) -> EndpointSpec:
      let a = BoxState.init(facet.fields.value.toPreserve)
      result.assertion = some a
    discard facet.addDataflowdo (facet: Facet):
      if facet.fields.value == N:
        facet.stopdo (facet: Facet):
          echo "terminated box root facet"
    discard facet.addEndpointdo (facet: Facet) -> EndpointSpec:
      const
        a = SetBox.init(`?$`)
      result.analysis = some analyzeAssertion(a)
      proc cb(facet: Facet; evt: EventKind; vs: seq[Value]) =
        if evt == messageEvent:
          facet.scheduleScriptdo (facet: Facet):
            facet.fields.value = vs[0]

      result.analysis.get.callback = some (facet.wrap cb)
      const
        o = Observe.init(SetBox.init(`?$`))
      result.assertion = some o
  facet.spawn("client")do (facet: Facet):
    discard facet.addEndpointdo (facet: Facet) -> EndpointSpec:
      const
        a = BoxState.init(`?$`)
      result.analysis = some analyzeAssertion(a)
      proc cb(facet: Facet; evt: EventKind; vs: seq[Value]) =
        if evt == addedEvent:
          facet.scheduleScriptdo (facet: Facet):
            let v = SetBox.init(vs[0].int.succ.toPreserve)
            facet.send(v)

      result.analysis.get.callback = some (facet.wrap cb)
      const
        o = Observe.init(BoxState.init(`?$`))
      result.assertion = some o
    discard facet.addEndpointdo (facet: Facet) -> EndpointSpec:
      const
        a = BoxState.init(`? _`)
      result.analysis = some analyzeAssertion(a)
      proc cb(facet: Facet; evt: EventKind; vs: seq[Value]) =
        if evt == removedEvent:
          facet.scheduleScriptdo (facet: Facet):
            echo "box gone"

      result.analysis.get.callback = some (facet.wrap cb)
      const
        o = Observe.init(BoxState.init(`? _`))
      result.assertion = some o
  facet.actor.dataspace.ground.addStopHandlerdo (_: Dataspace):
    echo "stopping box-and-client"

waitFor bootModule(boot)