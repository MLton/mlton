functor IntX (S: INT_X_STRUCTS): INT_X = 
struct

open S

datatype t = T of {int: IntInf.t,
		   size: IntSize.t}
   
local
   fun make f (T r) = f r
in
   val int = make #int
   val size = make #size
end

fun equals (T {int = i, size = s, ...}, 
	    T {int = i', size = s', ...}) = 
   i = i' andalso s = s'

fun toString (T {int = i, ...}) = IntInf.toString i

val layout = Layout.str o toString

fun format (T {int = i, ...}, r) = IntInf.format (i, r)

fun make (i: IntInf.t, s: IntSize.t): t =
   if IntSize.isInRange (s, i)
      then T {int = i,
	      size = s}
   else raise Overflow

fun defaultInt (i: int): t = make (IntInf.fromInt i, IntSize.default)

val toIntInf = int

val toInt = IntInf.toInt o toIntInf

val toChar = Char.fromInt o toInt

val hash = IntInf.hash o toIntInf

local
   val make: (IntInf.t * IntInf.t -> IntInf.t) -> t * t -> t =
      fn f => fn (i, i') => make (f (int i, int i'), size i)
in
   val op + = make IntInf.+
   val op - = make IntInf.-
   val op * = make IntInf.*
   val quot = make IntInf.quot
   val rem = make IntInf.rem
end

fun ~ i = make (IntInf.~ (int i), size i)

local
   fun is i i' = int i' = IntInf.fromInt i
in
   val isNegOne = is ~1
   val isOne = is 1
   val isZero = is 0
end

local
   fun is f i = int i = f (size i)
in
   val isMax = is IntSize.max
   val isMin = is IntSize.min
end

fun one s = make (1, s)
   
fun zero s = make (0, s)

fun max s = make (IntSize.max s, s)

fun min s = make (IntSize.min s, s)

local
   fun make (f: IntInf.t * IntInf.t -> 'a): t * t -> 'a =
      fn (i, i') =>
      if IntSize.equals (size i, size i')
	 then f (int i, int i')
      else Error.bug "IntX binary failure"
in
   val op < = make IntInf.<
   val op <= = make IntInf.<=
   val op > = make IntInf.>
   val op >= = make IntInf.>=
   val compare = make IntInf.compare
end

end
