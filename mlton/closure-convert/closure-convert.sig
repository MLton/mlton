(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature CLOSURE_CONVERT_STRUCTS = 
   sig
      structure Sxml: SXML
      structure Cps: CPS
      sharing Sxml.Atoms = Cps.Atoms
   end

signature CLOSURE_CONVERT = 
   sig
      include CLOSURE_CONVERT_STRUCTS
      
      val closureConvert: Sxml.Program.t -> Cps.Program.t
   end
