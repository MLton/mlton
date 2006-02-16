(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature TYCON_STRUCTS = 
   sig
      structure CharSize: CHAR_SIZE
      structure IntSize: INT_SIZE
      structure RealSize: REAL_SIZE
      structure WordSize: WORD_SIZE
   end

signature TYCON =
   sig
      include ID
      include PRIM_TYCONS        
      sharing type t = tycon

      val stats: unit -> Layout.t
   end
