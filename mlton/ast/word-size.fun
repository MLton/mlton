functor WordSize (S: WORD_SIZE_STRUCTS): WORD_SIZE =
struct

open S

datatype t = T of {bits: int}

fun bits (T {bits, ...}) = bits

val toString = Int.toString o bits

val layout = Layout.str o toString

val equals: t * t -> bool = op =

val sizes: int list =
   List.tabulate (31, fn i => i + 2)
   @ [64]

fun isValidSize (i: int) =
   (2 <= i andalso i <= 32) orelse i = 64

fun make i = T {bits = i}

val allVector = Vector.tabulate (65, fn i =>
				  if isValidSize i
				     then SOME (make i)
				  else NONE)

fun W i =
   case Vector.sub (allVector, i) handle Subscript => NONE of
      NONE => Error.bug (concat ["strange word size: ", Int.toString i])
    | SOME s => s

val all = List.map (sizes, W)

val prims = [W 8, W 16, W 32, W 64]

val default = W 32

fun pointer () = W 32

val memoize: (t -> 'a) -> t -> 'a =
   fn f =>
   let
      val v = Vector.map (allVector, fn opt => Option.map (opt, f))
   in
      fn T {bits = i, ...} => valOf (Vector.sub (v, i))
   end

fun roundUpToPrim s =
   let
      val bits = bits s
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
      W bits
   end

val bytes: t -> int = memoize (fn s => bits (roundUpToPrim s) div 8)

val max: t -> IntInf.t =
   memoize (fn s => IntInf.<< (1, Word.fromInt (bits s)) - 1)
   
val cardinality = memoize (fn s => IntInf.pow (2, bits s))

datatype prim = W8 | W16 | W32 | W64

val primOpt = memoize (fn T {bits, ...} =>
		       List.peekMap ([(8, W8), (16, W16), (32, W32), (64, W64)],
				     fn (b, p) =>
				     if b = bits then SOME p else NONE))

fun prim s =
   case primOpt s of
      NONE => Error.bug "WordSize.prim"
    | SOME p => p

end
