(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature LIVE_STRUCTS = 
   sig
      include SSA
   end

signature LIVE = 
   sig
      include LIVE_STRUCTS

      val live:
	 Function.t * {isCont: Label.t -> bool,
		       shouldConsider: Var.t -> bool}
	 -> {
	     labelLive:
	     Label.t -> {
                         (* live at beginning of block. *)
                         begin: Var.t list,
			 (* live at the beginning of a block, except formals. *)
			 beginNoFormals: Var.t list,
			 (* live at frame corresponding to the block. *)
			 frame: Var.t list,
			 (* live handler slots at beginning of block. *)
			 handlerSlots: bool * bool
			 },
	     (* live variables at primitives that require live variables.
	      *)
	     primLive: Var.t -> {vars: Var.t list,
				 handlerSlots: bool * bool}
	     }
   end
