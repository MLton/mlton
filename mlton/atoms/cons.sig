(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature CON_STRUCTS = 
   sig
      structure AstId: AST_ID
   end

signature CON = 
   sig
      include HASH_ID
      include PRIM_CONS where type con = t

      val fromBool: bool -> t
      val stats: unit -> Layout.t
   end
