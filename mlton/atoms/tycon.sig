(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature TYCON_STRUCTS = 
   sig
      structure AstId: AST_ID
   end

signature TYCON =
   sig
      include HASH_ID
      include PRIM_TYCONS where type tycon = t

      val stats: unit -> Layout.t
   end
