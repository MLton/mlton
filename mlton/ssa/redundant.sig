(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature REDUNDANT_STRUCTS = 
   sig
      include SHRINK
   end

signature REDUNDANT = 
   sig
      include REDUNDANT_STRUCTS
      
      val redundant: Program.t -> Program.t
   end
