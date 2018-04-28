(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ENV_STRUCTS =
   sig
      structure Domain: T
   end

signature ENV =
   sig
      include ENV_STRUCTS

      type 'a t

      val + : 'a t * 'a t -> 'a t
      val domain: 'a t -> Domain.t list
      val empty: unit -> 'a t
      val equals: ('a * 'a -> bool) -> 'a t * 'a t -> bool
      val extend: 'a t * Domain.t * 'a -> 'a t
      val fold: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val foldi: 'a t * 'b * (Domain.t * 'a * 'b -> 'b) -> 'b
      val forall: 'a t * ('a -> bool) -> bool
      val foralli: 'a t * (Domain.t * 'a -> bool) -> bool
      val foreach: 'a t * ('a -> unit) -> unit
      val foreachi: 'a t * (Domain.t * 'a -> unit) -> unit
      val fromList: (Domain.t * 'a) list -> 'a t
      val isEmpty: 'a t -> bool
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
      val lookup: 'a t * Domain.t -> 'a
      val map: 'a t * ('a -> 'b) -> 'b t
      val mapi: 'a t * (Domain.t * 'a -> 'b) -> 'b t
      val maybeLayout: string * ('a -> Layout.t) -> 'a t -> Layout.t
      val multiExtend: 'a t * Domain.t list * 'a list -> 'a t
      val new: Domain.t list * (Domain.t -> 'a) -> 'a t
      val peek: 'a t * Domain.t -> 'a option
      val plus: 'a t list -> 'a t
      val remove: 'a t * Domain.t -> 'a t
      val restrict: 'a t * Domain.t list -> 'a t
      val single: Domain.t * 'a -> 'a t
      val singleton: Domain.t * 'a -> 'a t
      val size: 'a t -> int
      val toList: 'a t -> (Domain.t * 'a) list
   end
