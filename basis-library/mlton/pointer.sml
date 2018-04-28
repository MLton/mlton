(* Copyright (C) 2010 Matthew Fluet.
 * Copyright (C) 2003-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonPointer: MLTON_POINTER_EXTRA =
struct

open Primitive.MLton.Pointer

val sizeofPointer =
   Word.div (Word.fromInt C_Size.wordSize, 0w8)

val add = fn (p, t) =>
   add (p, C_Ptrdiff.fromLarge (Word.toLargeIntX t))
val sub = fn (p, t) =>
   sub (p, C_Ptrdiff.fromLarge (Word.toLargeIntX t))
val diff = fn (p, p') =>
   Word.fromLargeInt (C_Ptrdiff.toLarge (diff (p, p')))

local
   fun wrap f (p, i) =
      f (p, C_Ptrdiff.fromInt i)
in
   val getCPointer = wrap getCPointer
   val getInt8 = wrap getInt8
   val getInt16 = wrap getInt16
   val getInt32 = wrap getInt32
   val getInt64 = wrap getInt64
   val getObjptr = fn (p, i) => (wrap getObjptr) (p, i)
   val getReal32 = wrap getReal32
   val getReal64 = wrap getReal64
   val getWord8 = wrap getWord8
   val getWord16 = wrap getWord16
   val getWord32 = wrap getWord32
   val getWord64 = wrap getWord64
end
val getPointer = getCPointer

local
   fun wrap f (p, i, x) =
      f (p, C_Ptrdiff.fromInt i, x)
in
   val setCPointer = wrap setCPointer
   val setInt8 = wrap setInt8
   val setInt16 = wrap setInt16
   val setInt32 = wrap setInt32
   val setInt64 = wrap setInt64
   val setObjptr = fn (p, i, x) => (wrap setObjptr) (p, i, x)
   val setReal32 = wrap setReal32
   val setReal64 = wrap setReal64
   val setWord8 = wrap setWord8
   val setWord16 = wrap setWord16
   val setWord32 = wrap setWord32
   val setWord64 = wrap setWord64
end
val setPointer = setCPointer

end
