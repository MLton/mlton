(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature FIXED_POINT = 
   sig
      val fix: {start: 'a,
                step: 'a -> 'a,
                equals: 'a * 'a -> bool} -> 'a

      val fix': ((unit -> unit) -> unit) -> unit
   end
