(* Copyright (C) 2019 Matthew Fluet.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature POWERSET_LATTICE_STRUCTS =
   sig
      structure Element:
         sig
            type t

            val equals: t * t -> bool
            val hash: t -> Word.t
            val layout: t -> Layout.t
         end
   end

signature POWERSET_LATTICE =
   sig
      include POWERSET_LATTICE_STRUCTS

      type t

      val << : Element.t * t -> unit (* force set to contain elt *)
      val <= : t * t -> unit (* force rhs to be superset of lhs *)
      (* handler will be run once for each element *)
      val addHandler: t * (Element.t -> unit) -> unit
      val empty: unit -> t
      val fromList: Element.t list -> t
      val getElements: t -> Element.t list
      val layout: t -> Layout.t
      val singleton: Element.t -> t
   end
