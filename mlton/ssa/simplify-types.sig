(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature SIMPLIFY_TYPES_STRUCTS = 
   sig
      include SHRINK
   end

signature SIMPLIFY_TYPES = 
   sig
      include SIMPLIFY_TYPES_STRUCTS
      
      val simplify: Program.t -> Program.t
   end
