(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
