structure MLtonFFI: MLTON_FFI =
struct

structure Prim = Primitive.FFI

structure Pointer = Primitive.Pointer

local
   fun make (p: Pointer.t, get, set) =
      (fn i => get (p, i), fn x => set (p, 0, x))
in
   val (getInt8, setInt8) =
      make (Prim.int8Array, Pointer.getInt8, Pointer.setInt8)
   val (getInt16, setInt16) =
      make (Prim.int16Array, Pointer.getInt16, Pointer.setInt16)
   val (getInt32, setInt32) =
      make (Prim.int32Array, Pointer.getInt32, Pointer.setInt32)
   val (getInt64, setInt64) =
      make (Prim.int64Array, Pointer.getInt64, Pointer.setInt64)
   fun getPointer i = Pointer.getPointer (Prim.pointerArray, i)
   fun setPointer x = Pointer.setPointer (Prim.pointerArray, 0, x)
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

(* To the C-world, booleans and chars are signed integers. *)
fun intToBool (i: int): bool = i <> 0
   
val getBool = intToBool o getInt32

val getChar8 = Primitive.Char.fromInt8 o getInt8
val getChar16 = Primitive.Char2.fromInt16 o getInt16
val getChar32 = Primitive.Char4.fromInt32 o getInt32
	       
fun boolToInt (b: bool): int = if b then 1 else 0

val setBool = setInt32 o boolToInt

val setChar8 = setInt8 o Primitive.Char.toInt8
val setChar16 = setInt16 o Primitive.Char2.toInt16
val setChar32 = setInt32 o Primitive.Char4.toInt32

end
