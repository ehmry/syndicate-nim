# SPDX-License-Identifier: MIT

import
  preserves, std / tables

type
  AnyAtomKind* {.pure.} = enum
    `bool`, `float`, `double`, `int`, `string`, `bytes`, `symbol`, `embedded`
  AnyAtomBool* = bool
  AnyAtomFloat* = float32
  AnyAtomDouble* = float64
  AnyAtomInt* = BiggestInt
  AnyAtomString* = string
  AnyAtomBytes* = seq[byte]
  AnyAtomSymbol* = Symbol
  AnyAtomEmbedded* = Value
  `AnyAtom`* {.preservesOr.} = object
    case orKind*: AnyAtomKind
    of AnyAtomKind.`bool`:
      
    of AnyAtomKind.`float`:
      
    of AnyAtomKind.`double`:
      
    of AnyAtomKind.`int`:
      
    of AnyAtomKind.`string`:
      
    of AnyAtomKind.`bytes`:
      
    of AnyAtomKind.`symbol`:
      
    of AnyAtomKind.`embedded`:
      
  
  DLit* {.preservesRecord: "lit".} = object
  
  DBind* {.preservesRecord: "bind".} = object
  
  DDiscard* {.preservesRecord: "_".} = object
  DCompoundKind* {.pure.} = enum
    `rec`, `arr`, `dict`
  DCompoundRec* {.preservesRecord: "rec".} = object
  
  DCompoundArr* {.preservesRecord: "arr".} = object
  
  DCompoundDict* {.preservesRecord: "dict".} = object
  
  `DCompound`* {.preservesOr.} = object
    case orKind*: DCompoundKind
    of DCompoundKind.`rec`:
      
    of DCompoundKind.`arr`:
      
    of DCompoundKind.`dict`:
      
  
  PatternKind* {.pure.} = enum
    `DDiscard`, `DBind`, `DLit`, `DCompound`
  `Pattern`* {.acyclic, preservesOr.} = ref object
    case orKind*: PatternKind
    of PatternKind.`DDiscard`:
      
    of PatternKind.`DBind`:
      
    of PatternKind.`DLit`:
      
    of PatternKind.`DCompound`:
      
  
proc `$`*(x: AnyAtom | DLit | DBind | DDiscard | DCompound | Pattern): string =
  `$`(toPreserves(x))

proc encode*(x: AnyAtom | DLit | DBind | DDiscard | DCompound | Pattern): seq[
    byte] =
  encode(toPreserves(x))
