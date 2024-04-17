# SPDX-License-Identifier: MIT

import
  preserves, std / tables

type
  AnyAtomKind* {.pure.} = enum
    `bool`, `double`, `int`, `string`, `bytes`, `symbol`, `embedded`
  `AnyAtom`* {.preservesOr.} = object
    case orKind*: AnyAtomKind
    of AnyAtomKind.`bool`:
      
    of AnyAtomKind.`double`:
      
    of AnyAtomKind.`int`:
      
    of AnyAtomKind.`string`:
      
    of AnyAtomKind.`bytes`:
      
    of AnyAtomKind.`symbol`:
      
    of AnyAtomKind.`embedded`:
      
  
  GroupTypeKind* {.pure.} = enum
    `rec`, `arr`, `dict`
  GroupTypeRec* {.preservesRecord: "rec".} = object
  
  GroupTypeArr* {.preservesRecord: "arr".} = object
  GroupTypeDict* {.preservesRecord: "dict".} = object
  `GroupType`* {.preservesOr.} = object
    case orKind*: GroupTypeKind
    of GroupTypeKind.`rec`:
      
    of GroupTypeKind.`arr`:
      
    of GroupTypeKind.`dict`:
      
  
  PatternKind* {.pure.} = enum
    `discard`, `bind`, `lit`, `group`
  PatternDiscard* {.preservesRecord: "_".} = object
  PatternBind* {.preservesRecord: "bind".} = object
  
  PatternLit* {.preservesRecord: "lit".} = object
  
  PatternGroup* {.preservesRecord: "group".} = object
  
  `Pattern`* {.acyclic, preservesOr.} = ref object
    case orKind*: PatternKind
    of PatternKind.`discard`:
      
    of PatternKind.`bind`:
      
    of PatternKind.`lit`:
      
    of PatternKind.`group`:
      
  
proc `$`*(x: AnyAtom | GroupType | Pattern): string =
  `$`(toPreserves(x))

proc encode*(x: AnyAtom | GroupType | Pattern): seq[byte] =
  encode(toPreserves(x))
