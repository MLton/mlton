(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature SSA2_STRUCTS = 
   sig
      include SSA_TREE2_STRUCTS
   end

signature SSA2 = 
   sig
      include SIMPLIFY2
   end
