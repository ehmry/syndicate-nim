# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves, std / tables

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
  AnyAtomEmbedded*[Cap] = Cap
  `AnyAtom`*[Cap] {.preservesOr.} = object
    case orKind*: AnyAtomKind
    of AnyAtomKind.`bool`:
      
    of AnyAtomKind.`float`:
      
    of AnyAtomKind.`double`:
      
    of AnyAtomKind.`int`:
      
    of AnyAtomKind.`string`:
      
    of AnyAtomKind.`bytes`:
      
    of AnyAtomKind.`symbol`:
      
    of AnyAtomKind.`embedded`:
      
  
  DLit*[Cap] {.preservesRecord: "lit".} = object
  
  DBind*[Cap] {.preservesRecord: "bind".} = ref object
  
  DDiscard* {.preservesRecord: "_".} = object
  DCompoundKind* {.pure.} = enum
    `rec`, `arr`, `dict`
  DCompoundRec*[Cap] {.preservesRecord: "rec".} = ref object
  
  DCompoundArr*[Cap] {.preservesRecord: "arr".} = ref object
  
  DCompoundDict*[Cap] {.preservesRecord: "dict".} = ref object
  
  `DCompound`*[Cap] {.preservesOr.} = ref object
    case orKind*: DCompoundKind
    of DCompoundKind.`rec`:
      
    of DCompoundKind.`arr`:
      
    of DCompoundKind.`dict`:
      
  
  PatternKind* {.pure.} = enum
    `DDiscard`, `DBind`, `DLit`, `DCompound`
  `Pattern`*[Cap] {.preservesOr.} = ref object
    case orKind*: PatternKind
    of PatternKind.`DDiscard`:
      
    of PatternKind.`DBind`:
      
    of PatternKind.`DLit`:
      
    of PatternKind.`DCompound`:
      
  
proc `$`*[Cap](x: AnyAtom[Cap] | DLit[Cap] | DBind[Cap] | DCompound[Cap] |
    Pattern[Cap]): string =
  `$`(toPreserve(x, Cap))

proc encode*[Cap](x: AnyAtom[Cap] | DLit[Cap] | DBind[Cap] | DCompound[Cap] |
    Pattern[Cap]): seq[byte] =
  encode(toPreserve(x, Cap))

proc `$`*(x: DDiscard): string =
  `$`(toPreserve(x))

proc encode*(x: DDiscard): seq[byte] =
  encode(toPreserve(x))
