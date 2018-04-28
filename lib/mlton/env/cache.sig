(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CACHE =
   sig
      structure Domain: T

      type 'a t
      val new: unit -> 'a t
      val peek: 'a t * Domain.t -> 'a option
      val addNew: 'a t * Domain.t * 'a -> unit
      val getOrAdd: 'a t * Domain.t * (unit -> 'a) -> 'a
      val toList: 'a t -> (Domain.t * 'a) list
   end 
