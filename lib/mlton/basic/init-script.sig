(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature INIT_SCRIPT =
   sig
      val startStop: {name: string,
                      action: string,
                      log: File.t,
                      thunk: unit -> unit,
                      usage: string -> unit} -> unit
   end
