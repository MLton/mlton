(* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor RealSize (S: REAL_SIZE_STRUCTS): REAL_SIZE = 
struct

open S

datatype t = R32 | R64

val all = [R32, R64]

val compare =
   fn (R32, R32) => EQUAL
    | (R32, _) => LESS
    | (R64, R64) => EQUAL
    | _ => GREATER

val equals: t * t -> bool = op =

val memoize: (t -> 'a) -> t -> 'a =
   fn f =>
   let
      val r32 = f R32
      val r64 = f R64
   in
      fn R32 => r32
       | R64 => r64
   end

val toString =
   fn R32 => "32"
    | R64 => "64"

val layout = Layout.str o toString

val bytes: t -> Bytes.t =
   fn R32 => Bytes.fromInt 4
    | R64 => Bytes.fromInt 8

val bits: t -> Bits.t = Bytes.toBits o bytes

end
