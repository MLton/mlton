(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.t
   
signature ENGINE =
   sig
      type 'a t

      datatype 'a res =
         Done of 'a
       | Raise of exn
       | TimeOut of 'a t

      val new: (unit -> 'a) -> 'a t
      val repeat: {thunk: unit -> 'a,
                   limit: Time.t,
                   tries: int} -> 'a option
      val run: 'a t * Time.t -> 'a res
      val timeLimit: Time.t * (unit -> 'a) -> 'a option
   end
