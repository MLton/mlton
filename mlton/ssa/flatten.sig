(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature FLATTEN_STRUCTS = 
   sig
      include SHRINK
   end

signature FLATTEN = 
   sig
      include FLATTEN_STRUCTS
      
      val flatten: Program.t -> Program.t
   end
