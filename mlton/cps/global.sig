(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature GLOBAL_STRUCTS = 
   sig
      include CPS_TREE
   end

signature GLOBAL = 
   sig
      include GLOBAL_STRUCTS
      
      val make:
	 unit -> {
		  new: Type.t * PrimExp.t -> Var.t,
		  all: unit -> {var: Var.t, ty: Type.t, exp: PrimExp.t} list
		 }
   end
