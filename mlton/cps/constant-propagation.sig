(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature CONSTANT_PROPAGATION_STRUCTS = 
   sig
      include SHRINK
   end

signature CONSTANT_PROPAGATION = 
   sig
      include CONSTANT_PROPAGATION_STRUCTS
      
      val simplify: Program.t -> Program.t
   end
