(*-------------------------------------------------------------------*)
(*                          PersistentQueue                          *)
(*-------------------------------------------------------------------*)

functor PersistentQueue(Q: BASIC_PERSISTENT_QUEUE)
  : PERSISTENT_QUEUE =
struct
   
open Q

exception Deque
fun deque q =
   case destruct q of
      SOME xq => xq
    | NONE => raise Deque

end
