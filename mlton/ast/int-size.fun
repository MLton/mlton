functor IntSize (S: INT_SIZE_STRUCTS): INT_SIZE =
struct

open S

datatype t = T of {precision: int}

fun bits (T {precision = p, ...}) = p

val equals: t * t -> bool = op =

val sizes: int list =
   List.tabulate (31, fn i => i + 2)
   @ [64]

fun isValidSize (i: int) =
   (2 <= i andalso i <= 32) orelse i = 64

fun make i = T {precision = i}

val allVector = Vector.tabulate (65, fn i =>
				  if isValidSize i
				     then SOME (make i)
				  else NONE)
				
fun I i =
   case Vector.sub (allVector, i) handle Subscript => NONE of
      NONE => Error.bug (concat ["strange int size: ", Int.toString i])
    | SOME s => s
   
val all = List.map (sizes, I)

val prims = [I 8, I 16, I 32, I 64]

val default = I 32
 
val memoize: (t -> 'a) -> t -> 'a =
   fn f =>
   let
      val v = Vector.map (allVector, fn opt => Option.map (opt, f))
   in
      fn T {precision = i, ...} => valOf (Vector.sub (v, i))
   end

val bytes: t -> int =
   memoize
   (fn T {precision, ...} =>
    if precision <= 8
       then 1
    else if precision <= 16
	    then 2
	 else if precision <= 32
		 then 4
	      else 8)

val toString = Int.toString o bits

val layout = Layout.str o toString

val cardinality = memoize (fn s => IntInf.pow (2, bits s))

val range =
   memoize
   (fn s =>
    let
       val pow = IntInf.pow (2, bits s - 1)
    in
       (~ pow, pow - 1)
    end)

fun isInRange (s, i) =
   let
      val (min, max) = range s
   in
      min <= i andalso i <= max
   end

val min = #1 o range

val max = #2 o range

datatype prim = I8 | I16 | I32 | I64

val primOpt = memoize (fn T {precision = i, ...} =>
		       List.peekMap ([(8, I8), (16, I16), (32, I32), (64, I64)],
				     fn (i', p) =>
				     if i = i' then SOME p else NONE))

fun prim s =
   case primOpt s of
      NONE => Error.bug "IntSize.prim"
    | SOME p => p

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
      I bits
   end

end
