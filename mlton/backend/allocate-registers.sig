(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
   
signature ALLOCATE_REGISTERS_STRUCTS = 
   sig
      structure Cps: CPS
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
		     (* Live registers at the beginning of the block. *)
		     live: Machine.Register.t list,
		     liveNoFormals: Machine.Register.t list
		     }
	 end
      
      (* Use Machine.Chunk.newRegister with the appropriate chunk to get a new
       * register.
       *)
      val allocate:
	 {
	  funcChunk: Cps.Func.t -> Machine.Chunk.t,
	  jumpChunk: Cps.Jump.t -> Machine.Chunk.t,
	  jumpHandlers: Cps.Jump.t -> Cps.Jump.t list,
	  program: Cps.Program.t,
	  varInfo: Cps.Var.t -> {
                                 (* If (isSome operand) then a stack slot or
				  * register needs to be allocated for the
				  * variable.
				  *)
				 operand: Machine.Operand.t option ref option,
				 (* primInfo will be set for primitives that
				  * enter the runtime system.
				  *)
				 primInfo: Machine.GCInfo.t option ref,
				 ty: Machine.Type.t
				 }
	  }
	 ->
	 {
	  contInfo: Cps.Jump.t -> {
				   (* Number of bytes in frame including
				    * return address.
				    *)
				   size: int,
				   (* Offsets of live pointers in frame. *)
				   liveOffsets: int list
				   },
	  funcInfo: Cps.Func.t -> {
				   info: Info.t,
				   (* If handlers are used, this gives the stack
				    * offset where the old exnStack and handler
				    * should be stored.
				    *)
				   handlerOffset: int option
				   },
	  handlerInfo: Cps.Jump.t -> {
				      (* Amount to subtract to get to the next
				       * frame.
				       *)
				      size: int,
				      (* Offsets of live pointers in frame. *)
				      liveOffsets: int list
				      },
	  jumpInfo: Cps.Jump.t -> Info.t
	  }
   end
