(* Copyright (C) 2009,2019 Matthew Fluet.
 * Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature OBJPTR_TYCON_STRUCTS =
   sig
      structure RealSize: REAL_SIZE
      structure WordSize: WORD_SIZE
   end

signature OBJPTR_TYCON =
   sig
      include OBJPTR_TYCON_STRUCTS

      type t

      val <= : t * t -> bool
      val compare: t * t -> Relation.t
      val equals: t * t -> bool
      (* This hash may change if setIndex is called *)
      val hash: t -> word
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
      val realVector: RealSize.t -> t
      val wordVector: WordSize.t -> t
   end
