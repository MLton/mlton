(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SET_STRUCTS =
   sig
      structure Element: T
   end

signature SET =
   sig
      structure Element: T

      type t

      val + : t * t -> t
      val - : t * t -> t
      val < : t * t -> bool
      val <= : t * t -> bool
      val > : t * t -> bool
      val >= : t * t -> bool
      val add: t * Element.t -> t
      val areDisjoint: t * t -> bool
      val contains: t * Element.t -> bool
      val empty: t
      val equals: t * t -> bool
      val exists: t * (Element.t -> bool) -> bool
      val fold: t * 'a * (Element.t * 'a -> 'a) -> 'a
      val forall: t * (Element.t -> bool) -> bool
      val foreach: t * (Element.t -> unit) -> unit
      (* list must contain no duplicates *)
      val fromList: Element.t list -> t
      val intersect: t * t -> t
      val isEmpty: t -> bool
      val layout: t -> Layout.t
      val map: t * (Element.t -> Element.t) -> t
      val partition: t * (Element.t -> bool) -> {yes: t, no: t}
      val power: t -> t list
      val replace: t * (Element.t -> Element.t option) -> t
      val remove: t * Element.t -> t
      val singleton: Element.t -> t
      val size: t -> int
      val subset: t * (Element.t -> bool) -> t
      val subsets: t * int -> t list
      val subsetSize: t * (Element.t -> bool) -> int
      val toList: t -> Element.t list
      val union: t * t -> t
      val unions: t list -> t
   end
