(* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor WordSize (S: WORD_SIZE_STRUCTS): WORD_SIZE =
struct

open S

datatype t = T of Bits.t

fun bits (T b) = b

val toString = Bits.toString o bits

val layout = Layout.str o toString

fun compare (s, s') = Bits.compare (bits s, bits s')

val {equals, ...} = Relation.compare compare

fun fromBits (b: Bits.t): t =
   if Bits.>= (b, Bits.zero)
      then T b
   else Error.bug (concat ["WordSize.fromBits: strange word size: ", Bits.toString b])

fun isValidSize (i: int) =
   (1 <= i andalso i <= 32) orelse i = 64

val one = fromBits (Bits.fromInt 1)
   
val byte = fromBits (Bits.fromInt 8)

val allVector = Vector.tabulate (65, fn i =>
                                  if isValidSize i
                                     then SOME (fromBits (Bits.fromInt i))
                                  else NONE)

val all: t list = Vector.toList (Vector.keepAllMap (allVector, fn so => so))

val prims = List.map ([8, 16, 32, 64], fromBits o Bits.fromInt)

val default = fromBits Bits.inWord

fun pointer () = fromBits Bits.inWord

val memoize: (t -> 'a) -> t -> 'a =
   fn f =>
   let
      val v = Vector.map (allVector, fn opt => Option.map (opt, f))
   in
      fn s => valOf (Vector.sub (v, Bits.toInt (bits s)))
   end

fun roundUpToPrim s =
   let
      val bits = Bits.toInt (bits s)
      val bits =
         if bits <= 8
            then 8
         else if bits <= 16
                 then 16
              else if bits <= 32
                      then 32
                   else if bits = 64
                           then 64
                        else Error.bug "WordSize.roundUpToPrim"
   in
      fromBits (Bits.fromInt bits)
   end

val bytes: t -> Bytes.t = Bits.toBytes o bits

fun cardinality s = IntInf.<< (1, Bits.toWord (bits s))

fun range (s, {signed}) =
   if signed
      then
         let
            val pow = IntInf.pow (2, Bits.toInt (bits s) - 1)
         in
            (~ pow, pow - 1)
         end
   else (0, cardinality s - 1)

val min = #1 o range
val max = #2 o range

fun isInRange (s, i, sg) =
   let
      val (min, max) = range (s, sg)
   in
      min <= i andalso i <= max
   end
   
datatype prim = W8 | W16 | W32 | W64

fun primOpt (s: t): prim option =
   let
      val b = Bits.toInt (bits s)
   in
      List.peekMap ([(8, W8), (16, W16), (32, W32), (64, W64)],
                    fn (b', p) => if b = b' then SOME p else NONE)
   end

fun prim s =
   case primOpt s of
      NONE => Error.bug "WordSize.prim"
    | SOME p => p

fun s + s' = fromBits (Bits.+ (bits s, bits s'))
   
end
