(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature CLOSURE_CONVERT_STRUCTS = 
   sig
      structure Sxml: SXML
      structure Ssa: SSA
      sharing Sxml.Atoms = Ssa.Atoms
   end

signature CLOSURE_CONVERT = 
   sig
      include CLOSURE_CONVERT_STRUCTS
      
      val closureConvert: Sxml.Program.t -> Ssa.Program.t
   end
