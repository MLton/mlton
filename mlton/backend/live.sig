(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature LIVE_STRUCTS = 
   sig
      include RSSA
   end

signature LIVE = 
   sig
      include LIVE_STRUCTS

      val live:
	 Function.t * {shouldConsider: Var.t -> bool}
	 -> Label.t -> {(* live at beginning of block. *)
			begin: Var.t list,
			(* live at the beginning of a block, except formals. *)
			beginNoFormals: Var.t list,
			(* live handler slots at beginning of block. *)
			handler: Label.t option,
			link: bool}
   end
