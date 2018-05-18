(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Ssa2 (S: SSA2_STRUCTS): SSA2 = 
   Simplify2 (Shrink2 (PrePasses2 (
   TypeCheck2 (Analyze2 (SsaTree2 (S))))))
