(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature TYCON_STRUCTS = 
   sig
      structure AstId: AST_ID
      structure IntSize: INT_SIZE
      structure RealSize: REAL_SIZE
      structure WordSize: WORD_SIZE
   end

signature TYCON =
   sig
      include HASH_ID
      include PRIM_TYCONS where type tycon = t

      val stats: unit -> Layout.t
   end
