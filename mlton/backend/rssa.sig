type int = Int.t
type word = Word.t
   
signature RSSA_STRUCTS = 
   sig
      include ATOMS

      structure Cases: MACHINE_CASES 
      structure Func: HASH_ID
      structure Label: HASH_ID
      structure Handler:
	 sig
	    datatype t =
	       CallerHandler
	     | None
	     | Handle of Label.t (* label must be of Handler kind *)

	    val foreachLabel: t * (Label.t -> unit) -> unit
	    val map: t * (Label.t -> Label.t) -> t
	 end
      structure Return:
	 sig
	    datatype t =
	       Dead
	     | HandleOnly
	     | NonTail of {cont: Label.t, (* label must be of Cont kind *)
			   handler: Handler.t} (* must agree with the handler
						* associated with the cont. *)
	     | Tail

	    val foreachLabel: t * (Label.t -> unit) -> unit
	 end
      structure Type: MTYPE
      sharing Label = Cases.Label
   end

signature RSSA = 
   sig
      include RSSA_STRUCTS

      structure Operand:
	 sig
	    datatype t =
	       CastInt of Var.t
	     | Const of Const.t
	     | Offset of {base: Var.t,
			  bytes: int,
			  ty: Type.t}
	     | OffsetScale of {base: Var.t,
			       index: Var.t,
			       ty: Type.t}
	     | Var of {var: Var.t,
		       ty: Type.t}

	    val int: int -> t
	    val layout: t -> Layout.t
	    val ty: t -> Type.t
	 end
	       
      structure Statement:
	 sig
	    datatype t =
	       Array of {dst: Var.t,
			 numBytesNonPointers: int,
			 numElts: Var.t,
			 numPointers: int}
	     | Move of {dst: Operand.t, (* If the dst is var, then it is the
					 * SSA defining occurrence.
					 *)
			src: Operand.t}
	     | Object of {dst: Var.t,
			  numPointers: int,
			  numWordsNonPointers: int,
			  stores: {offset: int, (* bytes *)
				   value: Operand.t} vector}
	     | PrimApp of {dst: (Var.t * Type.t) option,
			   prim: Prim.t,
			   args: Var.t vector}
	     | SetExnStackLocal
	     | SetExnStackSlot
	     | SetHandler of Label.t (* label must be of Handler kind. *)
	     | SetSlotExnStack

	    (* foldDef (s, a, f)
	     * If s defines a variable x, then return f (x, a), else return a.
	     *)
	    val foldDef: t * 'a * ({var: Var.t, ty: Type.t} * 'a -> 'a) -> 'a
	    (* forDef (s, f) = foldDef (s, (), fn (x, ()) => f x) *)
	    val forDef: t * ({var: Var.t, ty: Type.t} -> unit) -> unit
	    val layout: t -> Layout.t
	 end

      structure LimitCheck:
	 sig
	    datatype t =
	       Array of {bytesPerElt: int,
			 extraBytes: int, (* for subsequent allocation *)
			 numElts: Var.t,
			 stackToo: bool}
	     | Heap of {bytes: int,
			stackToo: bool}
	     | Signal
	     | Stack

	    val forVar: t * (Var.t -> unit) -> unit
	 end
      
      structure Transfer:
	 sig
	    datatype t =
	       Arith of {dst: Var.t,
			 prim: Prim.t,
			 args: Var.t vector,
			 overflow: Label.t, (* Must be nullary. *)
			 success: Label.t (* Must be nullary. *)
			}
	     | Bug  (* MLton thought control couldn't reach here. *)
	     | CCall of {args: Operand.t vector,
			 prim: Prim.t,
			 return: Label.t, (* return must be of CReturn kind.
					   * It should be nullary if the C
					   * function returns void.  Else, should
					   * be either nullary or unary with a
					   * var of the appropriate type to
					   * accept the result.
					   *)
			 returnTy: Type.t option}
	     | Call of {func: Func.t,
			args: Operand.t vector,
			return: Return.t}
	     | Goto of {dst: Label.t,
			args: Operand.t vector}
	     | LimitCheck of {kind: LimitCheck.t,
			      return: Label.t} (* return must be nullary and of
						* Cont kind.
						*)
	     (* Raise implicitly raises to the caller.  
	      * I.E. the local handler stack must be empty.
	      *)
	     | Raise of Operand.t vector
	     | Return of Operand.t vector
	     | Runtime of {args: Operand.t vector,
			   prim: Prim.t,
			   return: Label.t} (* Must be nullary. *)
	     | Switch of {cases: Cases.t,
			  default: Label.t option, (* Must be nullary. *)
			  test: Operand.t}
	     | SwitchIP of {int: Label.t,
			    pointer: Label.t,
			    test: Operand.t}

	    val layout: t -> Layout.t
	 end

      structure Block:
	 sig
	    structure Kind:
	       sig
		  datatype t =
		     Cont of {handler: Label.t option}
		   | CReturn
		   | Handler
		   | Normal
	       end

	    datatype t =
	       T of {
		     args: (Var.t * Type.t) vector,
		     kind: Kind.t,
		     label: Label.t,
		     statements: Statement.t vector,
		     transfer: Transfer.t
		     }

	    val args: t -> (Var.t * Type.t) vector
	    val clear: t -> unit
	    val kind: t -> Kind.t
	    val label: t -> Label.t
	    val statements: t -> Statement.t vector
	    val transfer: t -> Transfer.t
	 end

      structure Function:
	 sig
	    type t
	       
	    val blocks: t -> Block.t vector
	    val dest: t -> {args: (Var.t * Type.t) vector,
			    blocks: Block.t vector,
			    name: Func.t,
			    start: Label.t}
	    (* dfs (f, v) visits the blocks in depth-first order, applying v b
	     * for block b to yield v', then visiting b's descendents,
	     * then applying v' ().
	     *)
	    val dfs: t * (Block.t -> unit -> unit) -> unit
	    val name: t -> Func.t
	    val new: {args: (Var.t * Type.t) vector,
		      blocks: Block.t vector,
		      name: Func.t,
		      start: Label.t} -> t
	    val start: t -> Label.t
	 end
     
      structure Program:
	 sig
	    (* The main function defines the globals. *)
	    datatype t =
	       T of {
		     functions: Function.t list,
		     main: Func.t (* Must be nullary. *)
		    }
	 end
   end
