(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature SSA_TO_SSA2_STRUCTS = 
   sig
      structure Ssa: SSA
      structure Ssa2: SSA2
      sharing Ssa.Atoms = Ssa2.Atoms
   end

signature SSA_TO_SSA2 = 
   sig
      include SSA_TO_SSA2_STRUCTS
      
      val convert: Ssa.Program.t -> Ssa2.Program.t
   end
