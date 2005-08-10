(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature ORDER0 =
   sig
      type t

      val compare: t * t -> Relation.t
      val equals: t * t -> bool
      val < : t * t -> bool
      val <= : t * t -> bool
      val > : t * t -> bool
      val >= : t * t -> bool
      val max: t * t -> t
      val min: t * t -> t
   end
