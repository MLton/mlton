(* Copyright (C) 2003-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonPointer: MLTON_POINTER =
struct

open Primitive.MLton.Pointer

val add = fn (p, t) => add (p, C_Size.fromWord t)
val sub = fn (p, t) => sub (p, C_Size.fromWord t)
val diff = fn (p, p') => C_Size.toWord (diff (p, p'))

local
   fun wrap f (p, i) =
      f (p, C_Ptrdiff.fromInt i)
in
   val getInt8 = wrap getInt8
   val getInt16 = wrap getInt16
   val getInt32 = wrap getInt32
   val getInt64 = wrap getInt64
   val getPointer = wrap getCPointer
   val getReal32 = wrap getReal32
   val getReal64 = wrap getReal64
   val getWord8 = wrap getWord8
   val getWord16 = wrap getWord16
   val getWord32 = wrap getWord32
   val getWord64 = wrap getWord64
end

local
   fun wrap f (p, i, x) =
      f (p, C_Ptrdiff.fromInt i, x)
in
   val setInt8 = wrap setInt8
   val setInt16 = wrap setInt16
   val setInt32 = wrap setInt32
   val setInt64 = wrap setInt64
   val setPointer = wrap setCPointer
   val setReal32 = wrap setReal32
   val setReal64 = wrap setReal64
   val setWord8 = wrap setWord8
   val setWord16 = wrap setWord16
   val setWord32 = wrap setWord32
   val setWord64 = wrap setWord64
end

end
