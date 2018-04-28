(* Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor CharSize (S: CHAR_SIZE_STRUCTS): CHAR_SIZE = 
struct

open S

datatype t = C8 | C16 | C32

val all = [C8, C16, C32]

fun bits s =
   Bits.fromInt
   (case s of
       C8 => 8
     | C16 => 16
     | C32 => 32)

val equals = op =

fun fromBits b =
   case Bits.toInt b of
      8 => C8
    | 16 => C16
    | 32 => C32
    | _ => Error.bug "CharSize.frombits"

val memoize =
   fn f =>
   let
      val c8 = f C8
      val c16 = f C16
      val c32 = f C32
   in
      fn C8 => c8
       | C16 => c16
       | C32 => c32
   end

val cardinality = memoize (fn s => IntInf.pow (2, Bits.toInt (bits s)))

fun isInRange (s, i) = 0 <= i andalso i < cardinality s

end
