(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature SSA_STRUCTS = 
   sig
      include SSA_TREE_STRUCTS
   end

signature SSA = 
   sig
      include SIMPLIFY
   end
