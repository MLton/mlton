(* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor CharSize (S: CHAR_SIZE_STRUCTS): CHAR_SIZE = 
struct

open S

datatype t = C1 | C2 | C4

val all = [C1, C2, C4]

fun bits s =
   Bits.fromInt
   (case s of
       C1 => 8
     | C2 => 16
     | C4 => 32)

val default = C1

val equals = op =

fun fromBits b =
   case Bits.toInt b of
      8 => C1
    | 16 => C2
    | 32 => C4
    | _ => Error.bug "CharSize.frombits"

val memoize =
   fn f =>
   let
      val c1 = f C1
      val c2 = f C2
      val c4 = f C4
   in
      fn C1 => c1
       | C2 => c2
       | C4 => c4
   end

val cardinality = memoize (fn s => IntInf.pow (2, Bits.toInt (bits s)))

fun isInRange (s, i) = 0 <= i andalso i < cardinality s

end
