(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature BACKEND_STRUCTS = 
   sig
      structure Machine: MACHINE
      structure Ssa: SSA2
      sharing Machine.Atoms = Ssa.Atoms

      val funcToLabel: Ssa.Func.t -> Machine.Label.t
   end

signature BACKEND = 
   sig
      include BACKEND_STRUCTS
      
      val toMachine:
	 Ssa.Program.t
	 * {codegenImplementsPrim: Machine.Type.t Machine.Prim.t -> bool}
	 -> Machine.Program.t
   end
