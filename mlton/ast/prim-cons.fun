(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor PrimCons(S: PRIM_CONS_STRUCTS) :> PRIM_CONS where type con = S.t =
   struct
      open S

      type con = t
	 
      val cons = fromString "::"
      val falsee = fromString "false"
      val nill = fromString "nil"
      val reff = fromString "ref"
      val truee = fromString "true"

      (* exception constructors *)
      val bind = fromString "Bind"
      val match = fromString "Match"
      val overflow = fromString "Overflow"
   end
