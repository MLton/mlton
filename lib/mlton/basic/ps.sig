(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PS =
   sig
      structure State =
         struct
            datatype t =
               Running | Sleeping
         end

      val ps: unit -> {pid: Pid.t,
                       commandName: string,
                       args: string list,
                       state: State.t} list
   end
