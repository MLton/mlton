(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature SHRINK_STRUCTS = 
   sig
      include TYPE_CHECK
   end

signature SHRINK = 
   sig
      include SHRINK_STRUCTS
      
      val shrinkExp:
	 {var: Var.t, ty: Type.t, exp: PrimExp.t} vector -> Exp.t -> Exp.t
      val shrinkExpNoDelete: Exp.t -> Exp.t
      val shrink: Program.t -> Program.t

      val simplifyProgram: (Exp.t -> Exp.t) -> Program.t -> Program.t
   end
