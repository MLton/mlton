(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
   
signature POLYVARIANCE_STRUCTS = 
   sig
      include SXML
   end

signature POLYVARIANCE = 
   sig
      include POLYVARIANCE_STRUCTS
      
      val duplicate: Program.t -> Program.t
   end
