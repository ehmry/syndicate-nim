# SPDX-License-Identifier: MIT

import
  preserves

type
  Question* {.preservesRecord: "q".} = object
  
  Answer* {.preservesRecord: "a".} = object
  
  ResultKind* {.pure.} = enum
    `ok`, `error`
  ResultOk* {.preservesRecord: "ok".} = object
  
  ResultError* {.preservesRecord: "error".} = object
  
  `Result`* {.preservesOr.} = object
    case orKind*: ResultKind
    of ResultKind.`ok`:
      
    of ResultKind.`error`:
      
  
proc `$`*(x: Question | Answer | Result): string =
  `$`(toPreserves(x))

proc encode*(x: Question | Answer | Result): seq[byte] =
  encode(toPreserves(x))
