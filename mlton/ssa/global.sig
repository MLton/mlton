(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
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
