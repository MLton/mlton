(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PERVASIVE_REAL = REAL
structure Pervasive =
   struct
      structure Char = Char
      structure FixedInt = FixedInt
      structure IEEEReal = IEEEReal
      structure Int = Int
      structure Int31 = Int31
      structure Int32 = Int32
      structure Int64 = Int64
      structure IntInf = IntInf
      structure LargeInt = LargeInt
      structure LargeReal = LargeReal
      structure LargeWord = LargeWord
      structure Position = Position
      structure Real = Real
      structure Real64 = Real64
      structure Word = Word
      structure Word8 = Word8
      structure Word31 = Word31
      structure Word32 = Word32
      structure Word64 = Word64
      structure String = String
      structure SysWord = SysWord
   end
