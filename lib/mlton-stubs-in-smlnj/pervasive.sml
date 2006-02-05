(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PERVASIVE_WORD = WORD
structure Pervasive =
   struct
      open Basis2002
      structure Array = Array
      structure Array2 = Array2
      structure Bool = Bool
      structure Byte = Byte
      structure Char = Char
      structure CharArray = CharArray
      structure CharVector = CharVector
      structure CommandLine = CommandLine
      structure Date = Date
      structure General = General
      structure IEEEReal = IEEEReal
      structure Int = Int
      structure Int32 = Int32
      structure IntInf = IntInf
      structure LargeInt = LargeInt
      structure LargeReal = LargeReal
      structure LargeWord = LargeWord
      structure ListPair = ListPair
      structure List = List
      structure Math = Math
      structure Option = Option
      structure OS = OS
      structure Position = Position
      structure Posix = Posix
      structure Real = Real
      structure RealArray = RealArray
      structure Real64Array = Real64Array
      structure SML90 = SML90
      structure SMLofNJ = SMLofNJ
      structure String = String
      structure StringCvt = StringCvt
      structure SysWord = SysWord
      structure TextIO = TextIO
      structure Time = Time
      structure Unix = Unix
      structure Vector = Vector
      structure Word = Word
      structure Word32 = Word32
      structure Word8 = Word8
      structure Word8Array = Word8Array
   end
