(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature SIMPLIFY_STRUCTS = 
   sig
      include SHRINK
   end

signature SIMPLIFY = 
   sig
      include SIMPLIFY_STRUCTS
      
      val simplify: Program.t -> Program.t
   end
