(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature LOOP_INVARIANT_STRUCTS = 
   sig
      include SHRINK
   end

signature LOOP_INVARIANT = 
   sig
      include LOOP_INVARIANT_STRUCTS
      
      val loopInvariant: Program.t -> Program.t
   end
