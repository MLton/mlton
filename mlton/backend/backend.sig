(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
   
signature BACKEND_STRUCTS = 
   sig
      structure Cps: CPS
      structure Machine: MACHINE
      sharing Cps.Prim = Machine.Prim
	 
      val funcToLabel: Cps.Func.t -> Machine.Label.t
      val jumpToLabel: Cps.Jump.t -> Machine.Label.t
   end

signature BACKEND = 
   sig
      include BACKEND_STRUCTS
      
      val generate: Cps.Program.t -> Machine.Program.t
   end
