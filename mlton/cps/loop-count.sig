(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature LOOP_COUNT_STRUCTS = 
   sig
      include CPS_TREE
   end

signature LOOP_COUNT = 
   sig
      include LOOP_COUNT_STRUCTS
      
      val instrument: Program.t -> Program.t
   end
