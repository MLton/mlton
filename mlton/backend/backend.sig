(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature BACKEND_STRUCTS = 
   sig
      structure Machine: MACHINE
      structure Ssa: SSA
      sharing Machine.Label = Ssa.Label
      sharing Machine.Prim = Ssa.Prim

      val funcToLabel: Ssa.Func.t -> Machine.Label.t
   end

signature BACKEND = 
   sig
      include BACKEND_STRUCTS
      
      val toMachine: Ssa.Program.t -> Machine.Program.t
   end
