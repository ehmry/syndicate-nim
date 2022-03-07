# SPDX-License-Identifier: MIT

import
  std / typetraits, preserves, std / tables

type
  AnyAtomKind* {.pure.} = enum
    `bool`, `float`, `double`, `int`, `string`, `bytes`, `symbol`, `embedded`
  AnyAtomBool* = bool
  AnyAtomFloat* = float32
  AnyAtomDouble* = float64
  AnyAtomInt* = int
  AnyAtomString* = string
  AnyAtomBytes* = seq[byte]
  AnyAtomSymbol* = Symbol
  AnyAtomEmbedded*[E] = Preserve[E]
  `AnyAtom`*[E] {.preservesOr.} = ref object
    case orKind*: AnyAtomKind
    of AnyAtomKind.`bool`:
      
    of AnyAtomKind.`float`:
      
    of AnyAtomKind.`double`:
      
    of AnyAtomKind.`int`:
      
    of AnyAtomKind.`string`:
      
    of AnyAtomKind.`bytes`:
      
    of AnyAtomKind.`symbol`:
      
    of AnyAtomKind.`embedded`:
      
  
  DLit*[E] {.preservesRecord: "lit".} = ref object
  
  DBind*[E] {.preservesRecord: "bind".} = ref object
  
  DDiscard* {.preservesRecord: "_".} = object
  DCompoundKind* {.pure.} = enum
    `rec`, `arr`, `dict`
  DCompoundRec*[E] {.preservesRecord: "rec".} = ref object
  
  DCompoundArr*[E] {.preservesRecord: "arr".} = ref object
  
  DCompoundDict*[E] {.preservesRecord: "dict".} = ref object
  
  `DCompound`*[E] {.preservesOr.} = ref object
    case orKind*: DCompoundKind
    of DCompoundKind.`rec`:
      
    of DCompoundKind.`arr`:
      
    of DCompoundKind.`dict`:
      
  
  PatternKind* {.pure.} = enum
    `DDiscard`, `DBind`, `DLit`, `DCompound`
  `Pattern`*[E] {.preservesOr.} = ref object
    case orKind*: PatternKind
    of PatternKind.`DDiscard`:
      
    of PatternKind.`DBind`:
      
    of PatternKind.`DLit`:
      
    of PatternKind.`DCompound`:
      
  
proc `$`*[E](x: AnyAtom[E] | DLit[E] | DBind[E] | DCompound[E] | Pattern[E]): string =
  `$`(toPreserve(x, E))

proc encode*[E](x: AnyAtom[E] | DLit[E] | DBind[E] | DCompound[E] | Pattern[E]): seq[
    byte] =
  encode(toPreserve(x, E))

proc `$`*(x: DDiscard): string =
  `$`(toPreserve(x))

proc encode*(x: DDiscard): seq[byte] =
  encode(toPreserve(x))
