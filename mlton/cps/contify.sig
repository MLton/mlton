(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature CONTIFY_STRUCTS = 
   sig
      include SHRINK
   end

signature CONTIFY = 
   sig
      include CONTIFY_STRUCTS
      
      val contify: Program.t -> Program.t
   end
