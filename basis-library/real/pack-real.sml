structure PackReal64Little: PACK_REAL =
struct

structure Prim = Primitive.PackReal

type real = real
   
val bytesPerElem: int = 8
val isBigEndian = false

fun update (a, i, r) =
   (Array.checkSlice (a, i, SOME bytesPerElem)
    ; Prim.update (a, i, r))
   
local
   val a = Word8Array.array (bytesPerElem, 0w0)
in
   fun toBytes (r: real): Word8Vector.vector =
      (Prim.update (a, 0, r)
       ; Byte.stringToBytes (Byte.unpackString (a, 0, NONE)))
end

fun subVec (v, i) =
   (Vector.checkSlice (v, i, SOME bytesPerElem)
    ; Prim.subVec (v, i))

fun fromBytes v = subVec (v, 0)

fun subArr (a, i) = subVec (Primitive.Vector.fromArray a, i)
   
end

structure PackRealLittle = PackReal64Little
