(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor CType (S: C_TYPE_STRUCTS): C_TYPE = 
struct

open S

datatype t =
   Int8
 | Int16
 | Int32
 | Int64
 | Pointer
 | Real32
 | Real64
 | Word8
 | Word16
 | Word32
 | Word64

val all = [Int8, Int16, Int32, Int64,
	   Pointer,
	   Real32, Real64,
	   Word8, Word16, Word32, Word64]

val bool = Int32

val char = Int8

val pointer = Pointer

val thread = Pointer

val equals: t * t -> bool = op =
   
fun memo (f: t -> 'a): t -> 'a =
   let
      val pointer = f Pointer
      val real32 = f Real32
      val real64 = f Real64
      val int8 = f Int8
      val int16 = f Int16
      val int32 = f Int32
      val int64 = f Int64
      val word8 = f Word8
      val word16 = f Word16
      val word32 = f Word32
      val word64 = f Word64
   in
      fn Int8 => int8
       | Int16 => int16
       | Int32 => int32
       | Int64 => int64
       | Pointer => pointer
       | Real32 => real32
       | Real64 => real64
       | Word8 => word8
       | Word16 => word16
       | Word32 => word32
       | Word64 => word64
   end

val toString =
   fn Int8 => "Int8"
    | Int16 => "Int16"
    | Int32 => "Int32"
    | Int64 => "Int64"
    | Pointer => "Pointer"
    | Real32 => "Real32"
    | Real64 => "Real64"
    | Word8 => "Word8"
    | Word16 => "Word16"
    | Word32 => "Word32"
    | Word64 => "Word64"

val layout = Layout.str o toString

fun size (t: t): Bytes.t =
   case t of
      Int8 => Bytes.fromInt 1
    | Int16 => Bytes.fromInt 2
    | Int32 => Bytes.fromInt 4
    | Int64 => Bytes.fromInt 8
    | Pointer => Bytes.inPointer
    | Real32 => Bytes.fromInt 4
    | Real64 => Bytes.fromInt 8
    | Word8 => Bytes.fromInt 1
    | Word16 => Bytes.fromInt 2
    | Word32 => Bytes.fromInt 4
    | Word64 => Bytes.fromInt 8

fun name t =
   case t of
      Int8 => "I8"
    | Int16 => "I16"
    | Int32 => "I32"
    | Int64 => "I64"
    | Pointer => "P"
    | Real32 => "R32"
    | Real64 => "R64"
    | Word8 => "W8"
    | Word16 => "W16"
    | Word32 => "W32"
    | Word64 => "W64"

fun align (t: t, b: Bytes.t): Bytes.t =
   Bytes.align (b, {alignment = size t})

fun real (s: RealSize.t): t =
   case Bits.toInt (RealSize.bits s) of
      32 => Real32
    | 64 => Real64
    | _ => Error.bug "CType.real"
   
fun word (s: WordSize.t, {signed: bool}): t =
   case (signed, Bits.toInt (WordSize.bits s)) of
      (false, 8) => Word8
    | (true, 8) => Int8
    | (false, 16) => Word16
    | (true, 16) => Int16
    | (false, 32) => Word32
    | (true, 32) => Int32
    | (false, 64) => Word64
    | (true, 64) => Int64
    | _ => Error.bug "CType.word"

end
