(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature UNCURRY_STRUCTS = 
   sig
      structure	Sxml: SXML
   end

signature UNCURRY = 
   sig
      include UNCURRY_STRUCTS
	 
      val uncurry: Sxml.Program.t -> Sxml.Program.t
   end
