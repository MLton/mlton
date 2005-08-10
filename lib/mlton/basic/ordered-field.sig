(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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

