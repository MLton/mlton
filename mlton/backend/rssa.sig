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
	    val layout: t -> Layout.t
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

	    val foldLabel: t * 'a * (Label.t * 'a -> 'a) -> 'a
	    val foreachLabel: t * (Label.t -> unit) -> unit
	 end
      structure RuntimeOperand: GC_FIELD
      structure Type: MTYPE
      sharing Label = Cases.Label
   end

signature RSSA = 
   sig
      include RSSA_STRUCTS

      structure Operand:
	 sig
	    datatype t =
	       ArrayOffset of {base: Var.t,
			       index: Var.t,
			       ty: Type.t}
	     | CastInt of Var.t
	     | Const of Const.t
	     | Offset of {base: Var.t,
			  bytes: int,
			  ty: Type.t}
	     | Pointer of int (* the int must be nonzero mod Runtime.wordSize. *)
	     | Runtime of RuntimeOperand.t
	     | Var of {var: Var.t,
		       ty: Type.t}

	    val bool: bool -> t
	    val int: int -> t
	    val layout: t -> Layout.t
	    val foreachVar: t * (Var.t -> unit) -> unit
	    val ty: t -> Type.t
	    val word: word -> t
	 end
	       
      structure Statement:
	 sig
	    datatype t =
	       Array of {dst: Var.t,
			 (* Treat the array as an object for the purposes of
			  * limit check insertion.
			  *)
			 isObject: bool,
			 (* The number of bytes taken by the array, not
			  * including headers.
			  *)
			 numBytes: Operand.t,
			 numBytesNonPointers: int,
			 numElts: Operand.t,
			 numPointers: int}
	     | Bind of {isMutable: bool,
			oper: Operand.t,
			var: Var.t}
	     | Move of {dst: Operand.t,
			src: Operand.t}
	     | Object of {dst: Var.t,
			  numPointers: int,
			  numWordsNonPointers: int,
			  stores: {offset: int, (* bytes *)
				   value: Operand.t} vector}
	     | PrimApp of {args: Operand.t vector,
			   dst: (Var.t * Type.t) option,
			   prim: Prim.t}
	     | SetExnStackLocal
	     | SetExnStackSlot
	     | SetHandler of Label.t (* label must be of Handler kind. *)
	     | SetSlotExnStack

	    (* foldDef (s, a, f)
	     * If s defines a variable x, then return f (x, a), else return a.
	     *)
	    val foldDef: t * 'a * (Var.t * Type.t * 'a -> 'a) -> 'a
	    (* forDef (s, f) = foldDef (s, (), fn (x, ()) => f x) *)
	    val foreachDef: t * (Var.t * Type.t -> unit) -> unit
	    val foreachDefUse: t * {def: (Var.t * Type.t) -> unit,
				    use: Var.t -> unit} -> unit
	    val foldUse: t * 'a * (Var.t * 'a -> 'a) -> 'a
	    val foreachUse: t * (Var.t -> unit) -> unit
	    val layout: t -> Layout.t
	 end

      structure Transfer:
	 sig
	    datatype t =
	       Arith of {args: Operand.t vector,
			 dst: Var.t,
			 overflow: Label.t, (* Must be nullary. *)
			 prim: Prim.t,
			 success: Label.t, (* Must be nullary. *)
			 ty: Type.t}
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
	     | Call of {args: Operand.t vector,
			func: Func.t,
			return: Return.t}
	     | Goto of {args: Operand.t vector,
			dst: Label.t}
	     (* Raise implicitly raises to the caller.  
	      * I.E. the local handler stack must be empty.
	      *)
	     | Raise of Operand.t vector
	     | Return of Operand.t vector
	     | Runtime of {args: Operand.t vector,
			   prim: Prim.t,
			   return: Label.t} (* Must be nullary, Runtime. *)
	     | Switch of {cases: Cases.t,
			  default: Label.t option, (* Must be nullary. *)
			  test: Operand.t}
	     | SwitchIP of {int: Label.t,
			    pointer: Label.t,
			    test: Operand.t}

	    (* foldDef (t, a, f)
	     * If t defines a variable x, then return f (x, a), else return a.
	     *)
	    val foldDef: t * 'a * (Var.t * Type.t * 'a -> 'a) -> 'a
	    (* foreachDef (t, f) = foldDef (t, (), fn (x, ()) => f x) *)
	    val foreachDef: t * (Var.t * Type.t -> unit) -> unit
	    val foreachDefLabelUse: t * {def: Var.t * Type.t -> unit,
					 label: Label.t -> unit,
					 use: Var.t -> unit} -> unit
	    val foreachLabel: t * (Label.t -> unit) -> unit
	    val foreachUse: t * (Var.t -> unit) -> unit
	    val iff: Operand.t * {falsee: Label.t, truee: Label.t} -> t
	    val layout: t -> Layout.t
	 end

      structure Kind:
	 sig
	    datatype t =
	       Cont of {handler: Label.t option}
	     | CReturn of {prim: Prim.t}
	     | Handler
	     | Jump
	     | Runtime of {prim: Prim.t}

	    val isOnStack: t -> bool
	 end

      structure Block:
	 sig
	    datatype t =
	       T of {args: (Var.t * Type.t) vector,
		     kind: Kind.t,
		     label: Label.t,
		     profileInfo: {ssa: {func: string, label: string}},
		     statements: Statement.t vector,
		     transfer: Transfer.t}

	    val allocTooLarge: t list ref -> (unit -> unit) * (unit -> Label.t)
	    val args: t -> (Var.t * Type.t) vector
	    val clear: t -> unit
	    val kind: t -> Kind.t
	    val label: t -> Label.t
	    val statements: t -> Statement.t vector
	    val profileInfo: t -> {ssa: {func: string, label: string}}
	    val transfer: t -> Transfer.t
	 end

      structure Function:
	 sig
	    type t
	       
	    val blocks: t -> Block.t vector
	    val clear: t -> unit
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
	    datatype t =
	       T of {functions: Function.t list,
		     (* main must be nullary and should not be called by other
		      * functions. It defines global variables that are in scope
		      * for the rest of the program.
		      *)
		     main: Function.t}

	    val clear: t -> unit
	    val handlesSignals: t -> bool
	    val hasPrim: t * (Prim.t -> bool) -> bool
	    val layouts: t * (Layout.t -> unit) -> unit
	    val typeCheck: t -> unit
	 end
   end
