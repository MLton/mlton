(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor IntSize (S: INT_SIZE_STRUCTS): INT_SIZE =
struct

open S

datatype t = T of {bits: Bits.t}

fun bits (T {bits, ...}) = bits

val toString = Bits.toString o bits

val layout = Layout.str o toString

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

fun I (b: Bits.t): t =
   case Vector.sub (allVector, Bits.toInt b) handle Subscript => NONE of
      NONE => Error.bug (concat ["strange int size: ", Bits.toString b])
    | SOME s => s

val all = List.map (sizes, I)

val prims = List.map ([8, 16, 32, 64], I o Bits.fromInt)

val default = I Bits.inWord

val memoize: (t -> 'a) -> t -> 'a =
   fn f =>
   let
      val v = Vector.map (allVector, fn opt => Option.map (opt, f))
   in
      fn T {bits = b, ...} => valOf (Vector.sub (v, Bits.toInt b))
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
			else Error.bug "IntSize.roundUpToPrim"
   in
      I (Bits.fromInt bits)
   end

val bytes: t -> Bytes.t = Bits.toBytes o bits

val cardinality = memoize (fn s => IntInf.pow (2, Bits.toInt (bits s)))

datatype prim = I8 | I16 | I32 | I64

val primOpt =
   memoize (fn T {bits, ...} =>
	    List.peekMap ([(8, I8), (16, I16), (32, I32), (64, I64)],
			  fn (b, p) =>
			  if b = Bits.toInt bits then SOME p else NONE))

fun prim s =
   case primOpt s of
      NONE => Error.bug "IntSize.prim"
    | SOME p => p

end
