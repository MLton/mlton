(* Copyright (C) 2009,2019 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature BACKEND_STRUCTS = 
   sig
      structure Machine: MACHINE
      structure Rssa: RSSA
      sharing Machine.BackendAtoms = Rssa.BackendAtoms

      val funcToLabel: Rssa.Func.t -> Machine.Label.t
   end

signature BACKEND = 
   sig
      include BACKEND_STRUCTS

      val toMachine: Rssa.Program.t -> Machine.Program.t
   end
