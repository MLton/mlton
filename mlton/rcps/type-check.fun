
functor TypeCheck(S: TYPE_CHECK_STRUCTS): TYPE_CHECK =
struct

  open S

  structure RTypes =
    struct
(*      
      val rec wf
	= fn Basic _ => true
	   | Tycon t => 
	   | Pointer t => wf t
	   | Aggregate tas => Vector.forall(tas, fn (t, a) => inAgg t)
	   | Sum ts => Vector.forall(ts, fn t => wf t)
	   | Array (a, t) => inArr t
      and inAgg
	= fn Basic _ => true
	   | Tycon t => 
	   | 
*)
    end

  fun typeCheck (Program.T {datatypes, ...})
    = let

      in
	()
      end
end