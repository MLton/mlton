(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CONST_TYPE_STRUCTS =
   sig
      structure RealSize: REAL_SIZE
      structure WordSize: WORD_SIZE
   end

signature CONST_TYPE =
   sig
      include CONST_TYPE_STRUCTS

      datatype t = Bool | Real of RealSize.t | String | Word of WordSize.t
   end
