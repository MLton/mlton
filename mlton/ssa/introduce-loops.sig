(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature INTRODUCE_LOOPS_STRUCTS = 
   sig
      include SHRINK
   end

signature INTRODUCE_LOOPS = 
   sig
      include INTRODUCE_LOOPS_STRUCTS
      
      val introduceLoops: Program.t -> Program.t
   end
