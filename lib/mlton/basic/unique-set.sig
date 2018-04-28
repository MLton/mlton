(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature UNIQUE_SET_STRUCTS = 
   sig
      structure Element:
         sig
            include T
            val hash: t -> Word.t
         end

      (* How many binary operations to cache. *)
      val cacheSize: int

      (* 2^bits buckets in the hash table *)
      val bits: int
   end

signature UNIQUE_SET =
   sig
      include UNIQUE_SET_STRUCTS

      type t

      val + : t * t -> t
      val - : t * t -> t
      val empty: t
      val equals: t * t -> bool
      val foreach: t * (Element.t -> unit) -> unit
      val fromList: Element.t list -> t
      val intersect: t * t -> t
      val isEmpty: t -> bool
      val layout: t -> Layout.t
      val plist: t -> PropertyList.t
      val reset: unit -> unit
      val singleton: Element.t -> t
      val stats: unit -> {hits: int, misses: int}
      val toList: t -> Element.t list
   end
