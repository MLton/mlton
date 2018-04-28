(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SSA2_STRUCTS = 
   sig
      include SSA_TREE2_STRUCTS
   end

signature SSA2 = 
   sig
      include SIMPLIFY2
   end
