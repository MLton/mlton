(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor IntSize (S: INT_SIZE_STRUCTS): INT_SIZE =
struct

open S

datatype t = T of {bits: Bits.t}

fun bits (T {bits, ...}) = bits

fun compare (s, s') = Bits.compare (bits s, bits s')

val {equals, ...} = Relation.compare compare

fun isValidSize (i: int) =
   (1 <= i andalso i <= 32) orelse i = 64

val sizes: Bits.t list =
   Vector.toList
   (Vector.keepAllMap
    (Vector.tabulate (65, fn i => if isValidSize i
                                     then SOME (Bits.fromInt i)
                                  else NONE),
     fn i => i))

fun make i = T {bits = i}

val allVector = Vector.tabulate (65, fn i =>
                                  if isValidSize i
                                     then SOME (make (Bits.fromInt i))
                                  else NONE)

fun fromBits (b: Bits.t): t =
   case Vector.sub (allVector, Bits.toInt b) handle Subscript => NONE of
      NONE => Error.bug (concat ["IntSize.fromBits: strange int size: ", Bits.toString b])
    | SOME s => s

val all = List.map (sizes, fromBits)

val memoize: (t -> 'a) -> t -> 'a =
   fn f =>
   let
      val v = Vector.map (allVector, fn opt => Option.map (opt, f))
   in
      fn T {bits = b, ...} => valOf (Vector.sub (v, Bits.toInt b))
   end

end
