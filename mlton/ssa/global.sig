(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature GLOBAL_STRUCTS = 
   sig
      include SSA_TREE
   end

signature GLOBAL = 
   sig
      include GLOBAL_STRUCTS
      
      val make:
	 unit -> {
		  new: Type.t * Exp.t -> Var.t,
		  all: unit -> Statement.t vector
		 }
   end
