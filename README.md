# Syndicated Actors for Nim

> The [Syndicated Actor model](https://syndicate-lang.org/about/) is a new model of concurrency, closely related to the actor, tuplespace, and publish/subscribe models.

The Syndicate library provides a code-generator and DSL for writing idiomatic Nim within the excution flow of the Syndicated Actor Model. The library allows for structured conversations with other programs and other languages. It can be used to implement distributed systems but also makes it possible to create dynamically reconfigurable programs using a process controller and a configuration conversation.

## The Preserves Data Language

[Preserves](https://preserves.gitlab.io/preserves/) is a data model and serialization format that is used to represent data in Syndicate conversations. The Preserves model features the familar boolean, integer, float, string, sequence, dictionaries, and set types. Less familar to Nim are the symbol and record types. The symbol is a string that is elevated for use in type comparisions and the record is a labelled (usually by symbol) collection of fields.

The textual representation isn't necessary to study before using Preserves because the Preserves model is a subset of Nim types and a code-generator is available to convert Preserves schemas to Nim modules.

### Preserves schema

[Here](https://git.syndicate-lang.org/syndicate-lang/syndicate-protocols/src/commit/ca92d99c524d99b6d3be04a0ba5383ec5a65b550/schemas/simpleChatProtocol.prs) is an example schema for a chat protocol:

```
; Present is a record with symbol "Present" as record label
; and one string field referred to as "username".
Present = <Present @username string>.

; Says is a record with symbol "Says" as record label
; and two fields referred to as "who" and "what".
Says = <Says @who string @what string>.
```

### Code Generation

The [preserves_schema_nim]() utility would generate the following module for the preceding schema:
``` nim
type
  Says* {.preservesRecord: "Says".} = object
    `who`*: string
    `what`*: string

  Present* {.preservesRecord: "Present".} = object
    `username`*: string
```

There are two types corresponding to the two records defined in the schema. The `preservesRecord` pragma allows for a lossless conversion between the Nim type system and Preserves records.

``` nim
var
  present = Present(username: "Judy")
  pr = present.toPreserve()
assert $pr == """<Present "Judy">"""
assert present.fromPreserve(pr) == true
```

## The Syndicate DSL

The Syndicate DSL can be entered using `bootDataspace` which calls a Nim body with a [dataspace](https://synit.org/book/glossary.html#dataspace) [Ref](https://synit.org/book/glossary.html#reference) and a [turn](https://synit.org/book/glossary.html#turn). The `Ref` is something that we can observe and publish assertions at, and a `Turn` is special type for temporal scoping and transactional semantics. Assertions can be published using the `Assertion` or equivalent `Preserve[Ref]` type, but using Nim types is preferred because they can be reliably consistent with schema.

### Publish

``` nim
bootDataspace("main") do (dataspace: Ref; turn: var Turn):
  let presenceHandle = publish(turn, dataspace, Present(username: "Judy"))
    # publish <Present "Judy"> to the dataspace
    # the assertion can be later retracted by handle

  message(turn, dataspace, Says(who: "Judy", what: "greetings"))
```

### React

We can react to assertions and messages within dataspaces using [patterns](https://synit.org/book/glossary.html#dataspace-pattern). Patterns are constructed using a Nim type and the `?` operator. Again a Nim type is used rather than a raw Preserves for schema consistency.

``` nim
bootDataspace("main") do (dataspace: Ref; turn: var Turn):
  during(turn, dataspace, ?Present) do (who: string):
    # This body is active when the ?Present pattern is matched.
    # The Present type contains two atomic values that can be matched
    # within Syndicate model, and the Nim `during` macro matches those
    # values to the types of the `do` handler.
    stderr.writeLine("<", who, " joined>")
  do:
    # This body is active when the match is retracted
    stderr.writeLine("<", who, " left>")

  onMessage(turn, dataspace, ?Says) do (who: string, what: string):
    # messages are one-shot assertions and can also be matched
    stderr.writeLine(who, ": ", what)

  onMessage(turn, dataspace, Says ? {0: grab(), 1: drop()}) do (who: string):
    # patterns can also be selectively constructed
    stderr.writeLine("<", who, " says something>")
```

## Examples

- [src/syndicate/unix](./src/syndicate/unix)
- [erisresolver](https://codeberg.org/eris/nim-eris_utils#erisresolver) - dynamic configuration
- [xdg_open_ng](https://git.syndicate-lang.org/ehmry/xdg_open_ng) - messaging, UNIX sockets, dynamic configuration, [Syndicate server](https://synit.org/book/operation/system-bus.html) interaction.

---

This work has been supported by the [NLnet Foundation](https://nlnet.nl/) and the European Commission's [Next Generation Internet programme](https://www.ngi.eu/). The [Syndicate Actor Model](https://syndicate-lang.org/projects/2021/system-layer/) through the [NGI Zero PET](https://nlnet.nl/PET/) program and this library as a part of the [ERIS project](https://eris.codeberg.page/) through [NGI Assure](https://nlnet.nl/assure/).
