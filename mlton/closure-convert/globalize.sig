(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature GLOBALIZE_STRUCTS = 
   sig
      include SXML
   end

signature GLOBALIZE = 
   sig
      include GLOBALIZE_STRUCTS
      
      val globalize: {
		      program: Program.t,
		      lambdaFree: Lambda.t -> Var.t vector,
		      varGlobal: Var.t -> bool ref
		     } -> unit
   end
