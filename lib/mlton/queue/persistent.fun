(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
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
