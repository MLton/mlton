(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MONO_ENV_STRUCTS =
   sig
      structure Domain: T
      structure Range: T
   end

signature BASIC_MONO_ENV =
   sig
      include MONO_ENV_STRUCTS

      type t
      val extend: t * Domain.t * Range.t -> t
      val fromList: (Domain.t * Range.t) list -> t
      val peek: t * Domain.t -> Range.t option
      val toList: t -> (Domain.t * Range.t) list
   end

signature MONO_ENV = 
   sig
      include BASIC_MONO_ENV

      val + : t * t -> t
      val domain: t -> Domain.t list
      val empty: t
      val equals: t * t -> bool
      val fold: t * 'a * (Range.t * 'a -> 'a) -> 'a
      val foldi: t * 'a * (Domain.t * Range.t * 'a -> 'a) -> 'a
      val foreach: t * (Range.t -> unit) -> unit
      val foreachi: t * (Domain.t * Range.t -> unit) -> unit
      val isEmpty: t -> bool
      val layout: t -> Layout.t
      val lookup: t * Domain.t -> Range.t
      val map: t * (Range.t -> Range.t) -> t
      val mapi: t * (Domain.t * Range.t -> Range.t) -> t
      val multiExtend: t * Domain.t list * Range.t list -> t
      val new: Domain.t list * (Domain.t -> Range.t) -> t
      val plus: t list -> t
      val remove: t * Domain.t -> t
      val restrict: t * Domain.t list -> t
      val single: Domain.t * Range.t -> t
      val size: t -> int
   end
