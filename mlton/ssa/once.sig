(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)

type int = Int.t

signature ONCE_STRUCTS = 
   sig
      include SSA_TREE
   end

signature ONCE = 
   sig
      include ONCE_STRUCTS

      (* Returns true for variables that are guaranteed to be bound no
       * more than once during the entire program.
       *)
      val once: Program.t -> Var.t -> bool
   end

signature NEW_ONCE_STRUCTS = 
   sig
      include SSA_TREE
   end

signature NEW_ONCE = 
   sig
      include NEW_ONCE_STRUCTS

      val once: Program.t -> {(* Returns true for functions that
			       *  are guaranteed to never have 
			       *  their stack frame duplicated
			       *  at a thread copy.
			       *)
			      funcIsSingleThreaded: Func.t -> bool,
			      (* Returns true for functions that
			       *  are guaranteed to be used no
			       *  more than once during the entire
			       *  program.
			       *)
			      funcIsUsedOnce: Func.t -> bool,
			      (* Returns true for labels that
			       *  are guaranteed to be used no
			       *  more than once during the entire
			       *  program.
			       *)
			      labelIsUsedOnce: Label.t -> bool,
			      (* Returns true for variables that
			       *  are guaranteed to be bound no
			       *  more than once during the entire
			       *  program.
			       *)
			      varIsBoundOnce: Var.t -> bool}
   end
