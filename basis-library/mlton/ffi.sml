(* Copyright (C) 2003-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonFFI: MLTON_FFI =
struct

structure Prim = Primitive.MLton.FFI

structure Pointer = Primitive.MLton.Pointer

local
   fun make (p: Pointer.t, get, set) =
      (fn i => get (p, C_Ptrdiff.fromInt i), 
       fn x => set (p, C_Ptrdiff.fromInt 0, x))
in
   fun getCPointer i = Pointer.getCPointer (Prim.cpointerArray, C_Ptrdiff.fromInt i)
   fun setCPointer x = Pointer.setCPointer (Prim.cpointerArray, C_Ptrdiff.fromInt 0, x)
   val (getInt8, setInt8) =
      make (Prim.int8Array, Pointer.getInt8, Pointer.setInt8)
   val (getInt16, setInt16) =
      make (Prim.int16Array, Pointer.getInt16, Pointer.setInt16)
   val (getInt32, setInt32) =
      make (Prim.int32Array, Pointer.getInt32, Pointer.setInt32)
   val (getInt64, setInt64) =
      make (Prim.int64Array, Pointer.getInt64, Pointer.setInt64)
   fun getObjptr i = Pointer.getObjptr (Prim.objptrArray, C_Ptrdiff.fromInt i)
   fun setObjptr x = Pointer.setObjptr (Prim.objptrArray, C_Ptrdiff.fromInt 0, x)
   val (getReal32, setReal32) =
      make (Prim.real32Array, Pointer.getReal32, Pointer.setReal32)
   val (getReal64, setReal64) =
      make (Prim.real64Array, Pointer.getReal64, Pointer.setReal64)
   val (getWord8, setWord8) =
      make (Prim.word8Array, Pointer.getWord8, Pointer.setWord8)
   val (getWord16, setWord16) =
      make (Prim.word16Array, Pointer.getWord16, Pointer.setWord16)
   val (getWord32, setWord32) =
      make (Prim.word32Array, Pointer.getWord32, Pointer.setWord32)
   val (getWord64, setWord64) =
      make (Prim.word64Array, Pointer.getWord64, Pointer.setWord64)
end

val atomicBegin = MLtonThread.atomicBegin
val atomicEnd = MLtonThread.atomicEnd
val register = MLtonThread.register

(* To the C-world, chars are signed integers. *)
val getChar8 = Primitive.Char8.idFromInt8 o getInt8
(*
val getChar16 = Primitive.Char16.idFromInt16 o getInt16
val getChar32 = Primitive.Char32.idFromInt32 o getInt32
*)

val setChar8 = setInt8 o Primitive.Char8.idToInt8
(*
val setChar16 = setInt16 o Primitive.Char16.idToInt16
val setChar32 = setInt32 o Primitive.Char32.idToInt32
*)

(* To the C-world, booleans are 32-bit integers. *)
fun intToBool (i: Int32.int): bool = i <> 0
val getBool = intToBool o getInt32
fun boolToInt (b: bool): Int32.int = if b then 1 else 0
val setBool = setInt32 o boolToInt

end
