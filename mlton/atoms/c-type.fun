functor CType (S: C_TYPE_STRUCTS): C_TYPE = 
struct

open S

datatype t =
   Pointer
 | Real32
 | Real64
 | Word8
 | Word16
 | Word32
 | Word64

val all = [Pointer, Real32, Real64, Word8, Word16, Word32, Word64]

val bool = Word32

val char = Word8

val pointer = Pointer

val preThread = Pointer
   
val thread = Pointer

val equals: t * t -> bool = op =
   
fun memo (f: t -> 'a): t -> 'a =
   let
      val pointer = f Pointer
      val real32 = f Real32
      val real64 = f Real64
      val word8 = f Word8
      val word16 = f Word16
      val word32 = f Word32
      val word64 = f Word64
   in
      fn Pointer => pointer
       | Real32 => real32
       | Real64 => real64
       | Word8 => word8
       | Word16 => word16
       | Word32 => word32
       | Word64 => word64
   end

val toString =
   fn Pointer => "Pointer"
    | Real32 => "Real32"
    | Real64 => "Real64"
    | Word8 => "Word8"
    | Word16 => "Word16"
    | Word32 => "Word32"
    | Word64 => "Word64"

val layout = Layout.str o toString

fun size (t: t): Bytes.t =
   case t of
      Pointer => Bytes.inPointer
    | Real32 => Bytes.fromInt 4
    | Real64 => Bytes.fromInt 8
    | Word8 => Bytes.fromInt 1
    | Word16 => Bytes.fromInt 2
    | Word32 => Bytes.fromInt 4
    | Word64 => Bytes.fromInt 8

fun name t =
   case t of
      Pointer => "P"
    | Real32 => "R32"
    | Real64 => "R64"
    | Word8 => "W8"
    | Word16 => "W16"
    | Word32 => "W32"
    | Word64 => "W64"

fun align (t: t, b: Bytes.t): Bytes.t =
   Bytes.align (b, {alignment = size t})

end
