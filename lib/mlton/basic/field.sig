(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature FIELD_STRUCTS =
   sig
      include RING

      val inverse: t -> t
   end

signature FIELD =
   sig
      include FIELD_STRUCTS

      val / : t * t -> t
   end

