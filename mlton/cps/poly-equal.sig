(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)

type int = Int.t
   
signature POLY_EQUAL_STRUCTS = 
   sig
      include SHRINK
   end

signature POLY_EQUAL = 
   sig
      include POLY_EQUAL_STRUCTS
      
      val polyEqual: Program.t -> Program.t
   end
