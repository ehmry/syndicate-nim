# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves, std / tables, std / tables, std / tables

type
  CRec*[E] {.preservesRecord: "rec".} = ref object
  
  DLit*[E] {.preservesRecord: "lit".} = ref object
  
  DBind*[E] {.preservesRecord: "bind".} = ref object
  
  DDiscard* {.preservesRecord: "_".} = object
  CArr* {.preservesRecord: "arr".} = object
  
  DCompoundKind* {.pure.} = enum
    `rec`, `arr`, `dict`
  DCompoundRec*[E] {.preservesRecord: "compound".} = ref object
  
  DCompoundArr*[E] {.preservesRecord: "compound".} = ref object
  
  DCompoundDict*[E] {.preservesRecord: "compound".} = ref object
  
  `DCompound`*[E] {.preservesOr.} = ref object
    case orKind*: DCompoundKind
    of DCompoundKind.`rec`:
      
    of DCompoundKind.`arr`:
      
    of DCompoundKind.`dict`:
      
  
  CDict* {.preservesRecord: "dict".} = object
  PatternKind* {.pure.} = enum
    `DDiscard`, `DBind`, `DLit`, `DCompound`
  `Pattern`*[E] {.preservesOr.} = ref object
    case orKind*: PatternKind
    of PatternKind.`DDiscard`:
      
    of PatternKind.`DBind`:
      
    of PatternKind.`DLit`:
      
    of PatternKind.`DCompound`:
      
  
proc `$`*[E](x: CRec[E] | DLit[E] | DBind[E] | DCompound[E] | Pattern[E]): string =
  `$`(toPreserve(x, E))

proc encode*[E](x: CRec[E] | DLit[E] | DBind[E] | DCompound[E] | Pattern[E]): seq[
    byte] =
  encode(toPreserve(x, E))

proc `$`*(x: DDiscard | CArr | CDict): string =
  `$`(toPreserve(x))

proc encode*(x: DDiscard | CArr | CDict): seq[byte] =
  encode(toPreserve(x))
