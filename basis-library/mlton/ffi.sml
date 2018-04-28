(* Copyright (C) 2003-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonFFI: MLTON_FFI =
struct

val register = MLtonThread.register

local
   fun makeGet get (p,i) = get (MLtonPointer.getPointer (p, i), 0)
   fun makeSet set (p,i,x) = set (MLtonPointer.getPointer (p, i), 0, x)
   fun make (get,set) = (makeGet get, makeSet set)
in
   val (getCPointer, setCPointer) =
      make (MLtonPointer.getCPointer, MLtonPointer.setCPointer)
   val (getInt8, setInt8) =
      make (MLtonPointer.getInt8, MLtonPointer.setInt8)
   val (getInt16, setInt16) =
      make (MLtonPointer.getInt16, MLtonPointer.setInt16)
   val (getInt32, setInt32) =
      make (MLtonPointer.getInt32, MLtonPointer.setInt32)
   val (getInt64, setInt64) =
      make (MLtonPointer.getInt64, MLtonPointer.setInt64)
   val getObjptr = fn (p,i) => makeGet MLtonPointer.getObjptr (p,i)
   val setObjptr = fn (p,i,x) => makeSet MLtonPointer.setObjptr (p,i,x)
   val (getReal32, setReal32) =
      make (MLtonPointer.getReal32, MLtonPointer.setReal32)
   val (getReal64, setReal64) =
      make (MLtonPointer.getReal64, MLtonPointer.setReal64)
   val (getWord8, setWord8) =
      make (MLtonPointer.getWord8, MLtonPointer.setWord8)
   val (getWord16, setWord16) =
      make (MLtonPointer.getWord16, MLtonPointer.setWord16)
   val (getWord32, setWord32) =
      make (MLtonPointer.getWord32, MLtonPointer.setWord32)
   val (getWord64, setWord64) =
      make (MLtonPointer.getWord64, MLtonPointer.setWord64)
end

(* To the C-world, chars are unsigned integers. *)
val getChar8 = fn (p, i) => Primitive.Char8.idFromWord8 (getWord8 (p, i))
(*
val getChar16 = fn (p, i) => Primitive.Char16.idFromWord16 (getWord16 (p, i))
val getChar32 = fn (p, i) => Primitive.Char32.idFromWord32 (getWord32 (p, i))
*)

val setChar8 = fn (p, i, x) => setWord8 (p, i, Primitive.Char8.idToWord8 x)
(*
val setChar16 = fn (p, i, x) => setWord16 (p, i, Primitive.Char16.idToWord16 x)
val setChar32 = fn (p, i, x) => setWord32 (p, i, Primitive.Char32.idToWord32 x)
*)

(* To the C-world, booleans are 32-bit integers. *)
fun intToBool (i: Int32.int): bool = i <> 0
val getBool = fn (p, i) => intToBool(getInt32 (p, i))
fun boolToInt (b: bool): Int32.int = if b then 1 else 0
val setBool = fn (p, i, x) => setInt32 (p, i, boolToInt x)

end
