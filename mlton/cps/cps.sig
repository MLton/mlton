(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature CPS_STRUCTS = 
   sig
      include CPS_TREE_STRUCTS
   end

signature CPS = 
   sig
      include SHRINK
   end
