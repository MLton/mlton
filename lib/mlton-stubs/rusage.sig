(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_RUSAGE =
   sig
      type t = {utime: Time.time, (* user time *)
                stime: Time.time  (* system time *)
                }
         
      val rusage: unit -> {children: t,
                           gc: t,
                           self: t}
   end
