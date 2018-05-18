(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature DISJOINT_SET =
   sig
      type 'a t

      (* Each set is associated with a single value, like a ref cell. *)
      val := : 'a t * 'a -> unit
      val ! : 'a t -> 'a

      val canUnion: 'a t * 'a t * ('a * 'a -> 'a option) -> bool
      val equals: 'a t * 'a t -> bool
      val isRepresentative: 'a t -> bool
      val representative: 'a t -> 'a t
      val singleton: 'a -> 'a t
      (* When unions occur, one of the values is chosen. *)
      val union: 'a t * 'a t -> unit
   end
