(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)


signature SSA_TO_RSSA_STRUCTS =
   sig
      structure Rssa: RSSA
      structure Ssa: SSA2
      sharing Rssa.Atoms = Ssa.Atoms
   end

signature SSA_TO_RSSA =
   sig
      include SSA_TO_RSSA_STRUCTS

      val convert:
         Ssa.Program.t
         * {codegenImplementsPrim: Rssa.Type.t Rssa.Prim.t -> bool}
         -> Rssa.Program.t
   end
