functor PackReal (S: sig
			type real
			val bytesPerElem: int
			val isBigEndian: bool
			val subVec: word8 vector * int -> real
			val subVecRev: word8 vector * int -> real
			val update: word8 array * int * real -> unit
			val updateRev: word8 array * int * real -> unit
		     end): PACK_REAL =
struct

open S

val (sub, up) =
   if isBigEndian = Primitive.MLton.Platform.Arch.isBigEndian
      then (subVec, update)
   else (subVecRev, updateRev)

fun update (a, i, r) =
   (Array.checkSlice (a, i, SOME bytesPerElem)
    ; up (a, i, r))
   
local
   val a = Word8Array.array (bytesPerElem, 0w0)
in
   fun toBytes (r: real): Word8Vector.vector =
      (up (a, 0, r)
       ; Byte.stringToBytes (Byte.unpackString (Word8ArraySlice.full a)))
end

fun subVec (v, i) =
   (Vector.checkSlice (v, i, SOME bytesPerElem)
    ; sub (v, i))

fun fromBytes v = subVec (v, 0)

fun subArr (a, i) = subVec (Primitive.Vector.fromArray a, i)
   
end

structure PackReal32Big: PACK_REAL =
   PackReal (val bytesPerElem: int = 4
	     val isBigEndian = true
	     open Primitive.PackReal32)
structure PackReal32Little: PACK_REAL =
   PackReal (val bytesPerElem: int = 4
	     val isBigEndian = false
	     open Primitive.PackReal32)
structure PackReal64Big: PACK_REAL =
   PackReal (val bytesPerElem: int = 8
	     val isBigEndian = true
	     open Primitive.PackReal64)
structure PackReal64Little: PACK_REAL =
   PackReal (val bytesPerElem: int = 8
	     val isBigEndian = false
	     open Primitive.PackReal64)

structure PackRealBig = PackReal64Big
structure PackRealLittle = PackReal64Little
