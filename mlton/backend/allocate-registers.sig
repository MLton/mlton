(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
   
signature ALLOCATE_REGISTERS_STRUCTS = 
   sig
      structure Ssa: SSA
      structure Machine: MACHINE
   end

signature ALLOCATE_REGISTERS = 
   sig
      include ALLOCATE_REGISTERS_STRUCTS

      structure Info:
	 sig
	    datatype t =
	       T of {
		     limitCheck: Machine.LimitCheck.t,
		     (* Live operands at the beginning of the block
		      *)
		     live: Machine.Operand.t list,
		     (* Live operands at the beginning of the block, 
		      * excepting its formals
		      *)
		     liveNoFormals: Machine.Operand.t list,
		     (* Live variables at the frame corresponding to the block
		      * only valid for continuations
		      *)
		     liveFrame: (Ssa.Label.t option * Machine.Operand.t list) list,
		     (* Number of bytes in frame including return address.
		      *)
		     size: int
		     }

	    val live: t -> Machine.Operand.t list
	 end
      
      (* Use Machine.Chunk.newRegister with the appropriate chunk to get a new
       * register.
       *)
      val allocate:
	 {
	  funcChunk: Ssa.Func.t -> Machine.Chunk.t,
	  isCont: Ssa.Label.t -> bool,
	  isHandler: Ssa.Label.t -> bool, 
	  labelChunk: Ssa.Label.t -> Machine.Chunk.t,
	  labelToLabel: Ssa.Label.t -> Machine.Label.t,
	  program: Ssa.Program.t,
	  varInfo: Ssa.Var.t -> {
                                 (* If (isSome operand) then a stack slot or
				  * register needs to be allocated for the
				  * variable.
				  *)
				 operand: Machine.Operand.t option ref option,
				 (* primInfo will be set for primitives that
				  * enter the runtime system.
				  *)
				 primInfo: Machine.PrimInfo.t ref,
				 ty: Machine.Type.t
				 }
	  }
	 -> Ssa.Function.t
	 -> {(* If handlers are used, this gives the stack
	      * offset where the old exnStack and handler
	      * should be stored.
	      *)
	     handlerOffset: int option,
	     labelInfo: Ssa.Label.t -> Info.t,
	     limitCheck: Machine.LimitCheck.t}
   end
