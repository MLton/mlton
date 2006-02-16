(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
