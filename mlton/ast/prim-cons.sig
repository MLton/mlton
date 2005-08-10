(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
