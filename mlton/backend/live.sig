(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature LIVE_STRUCTS = 
   sig
      include CPS
   end

signature LIVE = 
   sig
      include LIVE_STRUCTS

      type t

      val live:
	 {
	  exp: Exp.t,
	  formals: (Var.t * Type.t) vector,
	  jumpHandlers: Jump.t -> Jump.t list,
	  shouldConsider: Var.t -> bool
	 }
	 -> {
	     (* live variables at beginning of block 
	      *)
	     liveBegin: Jump.t -> Var.t list,
	     (* live variables at the beginning of a block, 
	      * excepting its formals
	      *)
	     liveBeginNoFormals: Jump.t -> Var.t list,
	     (* live variables at the frame corresponding to the block
	      *)
	     liveBeginFrame: Jump.t -> Var.t list,
	     (* live handler slots at beginning of block
	      *)
	     liveBeginHS: Jump.t -> (bool * bool),
	     (* live variables at primitives that require live variables.
	      *)
	     livePrim: Var.t -> Var.t list,
	     (* live handler slots at primitives that require live variables
	      *)
	     livePrimHS: Var.t -> (bool * bool),
	     destroy: unit -> unit
	     }
   end
