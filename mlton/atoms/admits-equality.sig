(* Copyright (C) 2003-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ADMITS_EQUALITY_STRUCTS = 
   sig
   end

signature ADMITS_EQUALITY = 
   sig
      include ADMITS_EQUALITY_STRUCTS

      datatype t = Always | Never | Sometimes

      val <= : t * t -> bool
      val layout: t -> Layout.t
      val or: t * t -> t
      val toString: t -> string
   end
