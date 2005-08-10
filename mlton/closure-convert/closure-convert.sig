(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature CLOSURE_CONVERT_STRUCTS = 
   sig
      structure Ssa: SSA
      structure Sxml: SXML
      sharing Sxml.Atoms = Ssa.Atoms
   end

signature CLOSURE_CONVERT = 
   sig
      include CLOSURE_CONVERT_STRUCTS
      
      val closureConvert: Sxml.Program.t -> Ssa.Program.t
   end
