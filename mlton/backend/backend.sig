(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
