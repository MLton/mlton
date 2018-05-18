(* Copyright (C) 2014 Matthew Fluet.
 * Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor CType (S: C_TYPE_STRUCTS): C_TYPE = 
struct

open S

datatype t =
   CPointer
 | Int8
 | Int16
 | Int32
 | Int64
 | Objptr
 | Real32
 | Real64
 | Word8
 | Word16
 | Word32
 | Word64

val all = [CPointer,
           Int8, Int16, Int32, Int64,
           Objptr,
           Real32, Real64,
           Word8, Word16, Word32, Word64]

val cpointer = CPointer
val objptr = Objptr
val thread = objptr

val equals: t * t -> bool = op =

fun memo (f: t -> 'a): t -> 'a =
   let
      val cpointer = f CPointer
      val int8 = f Int8
      val int16 = f Int16
      val int32 = f Int32
      val int64 = f Int64
      val objptr = f Objptr
      val real32 = f Real32
      val real64 = f Real64
      val word8 = f Word8
      val word16 = f Word16
      val word32 = f Word32
      val word64 = f Word64
   in
      fn CPointer => cpointer
       | Int8 => int8
       | Int16 => int16
       | Int32 => int32
       | Int64 => int64
       | Objptr => objptr
       | Real32 => real32
       | Real64 => real64
       | Word8 => word8
       | Word16 => word16
       | Word32 => word32
       | Word64 => word64
   end

val toString =
   fn CPointer => "CPointer"
    | Int8 => "Int8"
    | Int16 => "Int16"
    | Int32 => "Int32"
    | Int64 => "Int64"
    | Objptr => "Objptr" (* CHECK *)
    | Real32 => "Real32"
    | Real64 => "Real64"
    | Word8 => "Word8"
    | Word16 => "Word16"
    | Word32 => "Word32"
    | Word64 => "Word64"

val layout = Layout.str o toString

fun size (t: t): Bytes.t =
   case t of
      CPointer => Bits.toBytes (Control.Target.Size.cpointer ())
    | Int8 => Bytes.fromInt 1
    | Int16 => Bytes.fromInt 2
    | Int32 => Bytes.fromInt 4
    | Int64 => Bytes.fromInt 8
    | Objptr => Bits.toBytes (Control.Target.Size.objptr ())
    | Real32 => Bytes.fromInt 4
    | Real64 => Bytes.fromInt 8
    | Word8 => Bytes.fromInt 1
    | Word16 => Bytes.fromInt 2
    | Word32 => Bytes.fromInt 4
    | Word64 => Bytes.fromInt 8

fun name t =
   case t of
      CPointer => "Q" (* CHECK *)
    | Int8 => "I8"
    | Int16 => "I16"
    | Int32 => "I32"
    | Int64 => "I64"
    | Objptr => "P" (* CHECK *)
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

fun word' (b: Bits.t, {signed: bool}): t =
   case (signed, Bits.toInt b) of
      (false, 8) => Word8
    | (true, 8) => Int8
    | (false, 16) => Word16
    | (true, 16) => Int16
    | (false, 32) => Word32
    | (true, 32) => Int32
    | (false, 64) => Word64
    | (true, 64) => Int64
    | _ => Error.bug "CType.word'"

fun word (s: WordSize.t, {signed: bool}): t =
   word' (WordSize.bits s, {signed = signed})

val cint =
   Promise.lazy
   (fn () => word' (Control.Target.Size.cint (),
                    {signed = true}))
val csize =
   Promise.lazy
   (fn () => word' (Control.Target.Size.csize (),
                    {signed = false}))

val seqIndex =
   Promise.lazy
   (fn () => word' (Control.Target.Size.seqIndex (),
                    {signed = true}))

val objptrHeader =
   Promise.lazy
   (fn () => word' (Control.Target.Size.header (),
                    {signed = false}))

val bool = word (WordSize.bool, {signed = true})
val compareRes = word (WordSize.compareRes, {signed = true})
val shiftArg = word (WordSize.shiftArg, {signed = false})

end
