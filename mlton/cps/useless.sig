(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature USELESS_STRUCTS = 
   sig
      include SHRINK
   end

signature USELESS = 
   sig
      include USELESS_STRUCTS
      
      val useless: Program.t -> Program.t
   end
