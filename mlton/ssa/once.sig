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

signature MULTI_STRUCTS = 
   sig
      include SSA_TREE
   end

signature MULTI = 
   sig
      include MULTI_STRUCTS

      val multi: Program.t -> {usesThreadsOrConts: bool,
			       funcDoesThreadCopyCurrent: Func.t -> bool,
			       funcIsMultiThreaded: Func.t -> bool,
			       funcIsMultiUsed: Func.t -> bool,
			       labelDoesThreadCopyCurrent: Label.t -> bool,
			       labelIsMultiThreaded: Label.t -> bool,
			       labelIsMultiUsed: Label.t -> bool,
			       varIsMultiDefed: Var.t -> bool}
   end
