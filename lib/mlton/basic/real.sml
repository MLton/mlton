structure Real: REAL =
struct

type int = Int.t

structure In = In0

structure R = 
   OrderedRing(structure R = 
		  RingWithIdentity(structure R =
				      Ring(type t = real
					   open Real
					   val zero = 0.0
					   val layout = Layout.str o toString
					   val equals = Pervasive.Real.==)
				   open R Real
				   val one = 1.0)
	       open R Real
	       val {compare, ...} =
		  Relation.lessEqual{< = op <, equals = equals})

structure F = OrderedField(open R Real
			   fun inverse x = 1.0 / x)
open F Real
open Math

exception Input 
fun input i =
   case fromString(In.inputToSpace i) of
      SOME x => x
    | NONE => raise Input
	 
val fromInt = Pervasive.Real.fromInt

structure Format =
   struct
      open StringCvt
      type t = realfmt
      val sci = SCI
      val fix = FIX
      val gen = GEN
   end

fun format(x, f) = Pervasive.Real.fmt f x

fun choose(n, k) =
   let val k = max(k, n - k)
   in prodFromTo{from = add1 k, to = n, term = fn i => i} / factorial(n - k)
   end

fun log(base, arg) = ln arg / ln base
   
val ln2 = ln two
   
fun log2 x = ln x / ln2

fun realPower(m, n) = exp(n * ln m)

val ceiling = ceil
   
end
