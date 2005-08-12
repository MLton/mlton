(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PRIM_CONS_STRUCTS =
   sig
      type t
      val equals: t * t -> bool
      val fromString: string -> t
   end

signature PRIM_CONS =
   sig
      type con

      val bind: con
      val cons: con
      val falsee: con
      val match: con
      val nill: con
      val overflow: con
      val reff: con
      val truee: con
   end
