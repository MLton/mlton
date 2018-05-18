(* Copyright (C) 2013,2017 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature TREE_STRUCTS =
   sig
      structure Seq:
         sig
            type 'a t

            val fold: 'a t * 'b * ('a * 'b -> 'b) -> 'b
            val foreach: 'a t * ('a -> unit) -> unit
            val layout: 'a t * ('a -> Layout.t) -> Layout.t
            val map: 'a t * ('a -> 'b) -> 'b t
         end
   end

signature TREE =
   sig
      include TREE_STRUCTS

      datatype 'a t = T of 'a * 'a t Seq.t

      structure Forest:
         sig
            type 'a t = 'a t Seq.t

            val layout: 'a t * ('a -> Layout.t) -> Layout.t
            val layoutDot:
               'a t * {nodeOptions: 'a -> Dot.NodeOption.t list,
                       options: Dot.GraphOption.t list,
                       title: string}
               -> Layout.t
         end

      val children: 'a t -> 'a Forest.t
      val foldPre: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val foldPost: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val foreachPre: 'a t * ('a -> unit) -> unit (* preorder traversal *)
      val foreachPost: 'a t * ('a -> unit) -> unit (* postorder traversal *)
      val layout: 'a t * ('a -> Layout.t) -> Layout.t
      val layoutDot:
         'a t * {nodeOptions: 'a -> Dot.NodeOption.t list,
                 options: Dot.GraphOption.t list,
                 title: string}
         -> Layout.t
      val map: 'a t * ('a -> 'b) -> 'b t
      val traverse: 'a t * ('a -> unit -> unit) -> unit
   end
