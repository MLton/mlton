(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
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
