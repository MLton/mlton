(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
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
