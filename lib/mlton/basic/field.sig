(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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

