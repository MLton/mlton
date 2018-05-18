(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature DISJOINT_COLLECTION =
   sig
      structure S :
         sig
            type 'a t

            val value: 'a t -> 'a
            val setValue: 'a t * 'a -> unit
            val equals: 'a t * 'a t -> bool
         end

      type 'a t

      val empty: unit -> 'a t
      val new: 'a list -> 'a t * 'a S.t list

      val addSingleton: 'a t * 'a -> 'a S.t

      val numSets: 'a t -> int

      val randomSet: 'a t -> 'a S.t
      val random: 'a t -> 'a

      val union: 'a t * 'a S.t * 'a S.t -> unit
   end
