(* Copyright (C) 1999-2005, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ORDERED_FIELD_STRUCTS =
   sig
      include ORDERED_RING

      val inverse: t -> t
   end

signature ORDERED_FIELD =
   sig
      include ORDERED_FIELD_STRUCTS

      val / : t * t -> t
   end
