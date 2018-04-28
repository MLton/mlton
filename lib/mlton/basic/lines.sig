(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2005, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature LINES =
   sig
      (* Print out starting at line start and dropping the last lines. *)
      val dropLast: In.t * Out.t * {start: int, last: int} -> unit
      (* Print out lines start through stop. *)
      val startStop: In.t * Out.t * {start: int, stop: int} -> unit
   end
