structure MLtonFFI: MLTON_FFI =
struct

structure Prim = Primitive.FFI

open Prim

val atomicBegin = MLtonThread.atomicBegin
val atomicEnd = MLtonThread.atomicEnd
val register = MLtonThread.register

fun intToBool (i: int): bool = i <> 0
   
val getBool = intToBool o getInt32

val getChar = Byte.byteToChar o getWord8
	       
fun boolToInt (b: bool): int = if b then 1 else 0

val setBool = setInt32 o boolToInt

val setChar = setWord8 o Byte.charToByte

end
