(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature BACKEND_STRUCTS = 
   sig
      structure Ssa: SSA
      structure Machine: MACHINE
      sharing Ssa.Prim = Machine.Prim

      val funcToLabel: Ssa.Func.t -> Machine.Label.t
      val labelToLabel: Ssa.Label.t -> Machine.Label.t
   end

signature BACKEND = 
   sig
      include BACKEND_STRUCTS
      
      val generate: Ssa.Program.t -> Machine.Program.t
   end
