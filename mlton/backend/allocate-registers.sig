(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature ALLOCATE_REGISTERS_STRUCTS = 
   sig
      structure Rssa: RSSA
      structure Machine: MACHINE
      sharing Rssa.Runtime = Machine.Runtime
      sharing Rssa.Type = Machine.Type
   end

signature ALLOCATE_REGISTERS = 
   sig
      include ALLOCATE_REGISTERS_STRUCTS

      val allocate:
	 {argOperands: Machine.Operand.t vector,
	  function: Rssa.Function.t,
	  varInfo: Rssa.Var.t -> {
				  (* If (isSome operand) then a stack slot or
				   * register needs to be allocated for the
				   * variable.
				   *)
				  operand: Machine.Operand.t option ref option,
				  ty: Machine.Type.t
				  }
	  }
	 -> {(* If handlers are used, handlerLinkOffset gives the stack offsets
	      * where the handler and link (old exnStack) should be stored.
	      *)
	     handlerLinkOffset: {handler: int,
				 link: int} option,
	     labelInfo:
	     Rssa.Label.t -> {(* Live operands at the beginning of the block. *)
			      live: Machine.Operand.t vector,
			      (* Live operands at the beginning of the block, 
			       * excepting its formals.
			       *)
			      liveNoFormals: Machine.Operand.t vector,
			      (* Number of bytes in frame including return
			       * address.
			       *)
			      size: int
			      }}
   end
