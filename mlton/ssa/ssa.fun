(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Ssa (S: SSA_STRUCTS): SSA = 
   Simplify (Restore (Shrink (PrePasses (
   TypeCheck (Analyze (DirectExp (SsaTree (S))))))))
