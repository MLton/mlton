functor Queue(Q: BASIC_QUEUE): QUEUE =
struct

val {error, ...} = Error.errors("queue", "queue")
   
structure Q' = Sequence(structure I = Integer
			structure S = Q)
   
open Q Q'

fun deque q =
   case destruct q of
      SOME xq => xq
    | NONE => error "deque"

end
