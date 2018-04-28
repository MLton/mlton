(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature UNARY_TYCON_STRUCTS = 
   sig
      structure Tycon: TYCON
   end

signature UNARY_TYCON = 
   sig
      include UNARY_TYCON_STRUCTS

      datatype t = Ref | Array | Vector

      val toTycon: t -> Tycon.t
      val toString: t -> string
      val equals: t * t -> bool
      val layout: t -> Layout.t
   end
