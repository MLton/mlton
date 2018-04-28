(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

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
