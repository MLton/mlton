(* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.t
   
signature POINTER_TYCON_STRUCTS =
   sig
   end

signature POINTER_TYCON =
   sig
      include POINTER_TYCON_STRUCTS
	 
      type t

      val <= : t * t -> bool
      val compare: t * t -> Relation.t
      val equals: t * t -> bool
      val fromIndex: Int.t -> t
      val index: t -> Int.t (* index into objectTypes array *)
      val layout: t -> Layout.t
      val new: unit -> t
      val setIndex: t * int -> unit
      val stack: t
      val thread: t
      val toString: t -> string
      val weakGone: t
      val wordVector: Bits.t -> t
   end
