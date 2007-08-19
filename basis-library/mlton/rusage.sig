(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_RUSAGE =
   sig
      type t = {utime: Time.time, (* user time *)
                stime: Time.time}  (* system time *)

      val measureGC: bool -> unit
      val rusage: unit -> {children: t, gc: t, self: t}
   end
