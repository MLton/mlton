(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Cps (S: CPS_STRUCTS): CPS = 
   Shrink (TypeCheck (Analyze (CpsTree (S))))
