val x = 1.0/0.0 + ~1.0/0.0
val y = ~1.0/0.0
val z = 1.0/0.0
val l = [x,y,~2.0,~1.0,~0.0,0.0,1.0,2.0,z]

fun cross' ([],l) = []
  | cross' (h::t,l) = (map (fn x => (h,x)) l) @ (cross' (t,l))
fun cross l = cross'(l,l)

val _ = List.app
        (fn (p as (x,y))
	  => let
	       val xstr = Real.toString x
	       val ystr = Real.toString y
	       val pstr = concat["(", xstr, ",", ystr, ")"]
	     in
	       List.app
	       (fn (oper,operstr)
		 => print (concat[operstr,
				  pstr,
				  " = ",
				  Bool.toString (oper p),
				  "\n"]))
	       [(Real.<,"Real.<"),
		(Real.<=,"Real.<="),
		(Real.>,"Real.>"),
		(Real.>=,"Real.>="),
		(Real.==,"Real.=="),
		(Real.!=,"Real.!="),
		(Real.?=,"Real.?=")]
	     end)
	(cross l)
