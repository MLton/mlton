(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature ALLOCATE_REGISTERS_STRUCTS = 
   sig
      structure Rssa: RSSA
      structure Machine: MACHINE
      sharing Rssa.Type = Machine.Type
   end

signature ALLOCATE_REGISTERS = 
   sig
      include ALLOCATE_REGISTERS_STRUCTS

      val allocate:
	 {
	  function: Rssa.Function.t,
	  newRegister: (Rssa.Label.t option * int * Machine.Type.t
			-> Machine.Register.t),
	  varInfo: Rssa.Var.t -> {
                                 (* If (isSome operand) then a stack slot or
				  * register needs to be allocated for the
				  * variable.
				  *)
				 operand: Machine.Operand.t option ref option,
				 ty: Machine.Type.t
				 }
	  }
	 -> {(* If handlers are used, handlerOffset gives the stack offset where
	      * the old exnStack and handler should be stored.
	      *)
	     handlerOffset: int option,
	     labelInfo:
	     Rssa.Label.t -> {
			      (* Live operands at the beginning of the block. *)
			      live: Machine.Operand.t vector,
			      (* Live operands at the beginning of the block, 
			       * excepting its formals.
			       *)
			      liveNoFormals: Machine.Operand.t vector,
			      (* Number of bytes in frame including return
			       * address.
			       *)
			      size: int,
			      (* Adjust the number of bytes in a frame size. *)
			      adjustSize: int -> {size: int, shift: int}
			      }}
   end
