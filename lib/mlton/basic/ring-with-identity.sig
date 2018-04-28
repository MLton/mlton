(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature RING_WITH_IDENTITY_STRUCTS = 
   sig
      include RING

      val one: t
   end

signature RING_WITH_IDENTITY = 
   sig
      include RING_WITH_IDENTITY_STRUCTS

      val add1: t -> t
      val dec: t ref -> unit
      (* fromInt n = 1 + ... + 1, n times. *)
      val fromInt: Pervasive.Int.int -> t
      val fromIntInf: Pervasive.IntInf.int -> t
      val inc: t ref -> unit
      val negOne: t
      val pow: t * Pervasive.Int.int -> t
      val powInf : t * Pervasive.IntInf.int -> t
      val pows: (t * Pervasive.Int.int) list -> t (* simultaneous exponentiation *)
      val powsInf: (t * Pervasive.IntInf.int) list -> t
      val prod: t list -> t
      val sub1: t -> t
      val three: t
      val two: t
   end
