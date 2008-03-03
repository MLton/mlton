(* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.t

signature OBJPTR_TYCON_STRUCTS =
   sig
   end

signature OBJPTR_TYCON =
   sig
      include OBJPTR_TYCON_STRUCTS

      type t

      val <= : t * t -> bool
      val compare: t * t -> Relation.t
      val equals: t * t -> bool
      val fromIndex: int -> t
      val index: t -> int (* index into objectTypes array *)
      val layout: t -> Layout.t
      val new: unit -> t
      val setIndex: t * int -> unit
      val toString: t -> string

      (* See gc/object.h. *) 
      val stack: t
      val thread: t
      val weakGone: t
      val wordVector: Bits.t -> t
      val headerOnly: t
      val fill: t
   end
