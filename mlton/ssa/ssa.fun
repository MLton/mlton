(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Ssa (S: SSA_STRUCTS): SSA = 
   Simplify (Restore (Shrink (TypeCheck (Analyze (DirectExp (SsaTree (S)))))))
