(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature SSA_TREE_STRUCTS = 
   sig
      include ATOMS
   end

signature SSA_TREE = 
   sig
      include SSA_TREE_STRUCTS

      structure Type:
	 sig
	    include HASH_TYPE
	       
	    datatype dest =
	       Char
	     | Int
	     | IntInf
	     | Pointer
	     | Word
	     | Word8
	     | Real
	     | String
	     | Thread
	     | Array of t
	     | Ref of t
	     | Datatype of Tycon.t
	     | Tuple of t vector
	     | Vector of t

	    val dest: t -> dest
	    val tyconArgs: t -> Tycon.t * t vector
	 end
      sharing Atoms = Type.Atoms

      structure Func: HASH_ID
      structure Label: HASH_ID

      structure Exp:
	 sig
	    datatype t =
	       ConApp of {con: Con.t,
			  args: Var.t vector}
	     | Const of Const.t
	     (* HandlerPush and HandlerPop are used throughout the simplification
	      * pipeline, but are replaced with SetExnStackLocal and friends
	      * at the very end of the pipeline, so that the backend never
	      * sees HandlerPush and Pop.
	      *)
	     | HandlerPop of Label.t (* the label is redundant, but useful *)
	     | HandlerPush of Label.t
	     | PrimApp of {prim: Prim.t,
			   targs: Type.t vector,
			   args: Var.t vector}
	     | Select of {tuple: Var.t,
			  offset: int}
	     | SetExnStackLocal
	     | SetExnStackSlot
	     | SetHandler of Label.t
	     | SetSlotExnStack
	     | Tuple of Var.t vector
	     | Var of Var.t

	    val equals: t * t -> bool
	    val foreachVar: t * (Var.t -> unit) -> unit
	    val hash: t -> Word.t
	    val layout: t -> Layout.t
	    val mayAllocate: t -> bool
	    val maySideEffect: t -> bool
	    val replaceVar: t * (Var.t -> Var.t) -> t
	    val unit: t
	 end

      structure Statement:
	 sig
	    datatype t = T of {var: Var.t option,
			       ty: Type.t,
			       exp: Exp.t}

	    val clear: t -> unit (* clear the var *)
	    val exp: t -> Exp.t
	    val handlerPop: Label.t -> t
	    val handlerPush: Label.t -> t
	    val layout: t -> Layout.t
	    val mayAllocate: t -> bool
	    val prettifyGlobals: t vector -> (Var.t -> string option)
	    val setExnStackLocal: t
	    val setExnStackSlot: t
	    val setHandler: Label.t -> t
	    val setSlotExnStack: t
	    val var: t -> Var.t option
	 end
      
      structure Cases: CASES sharing type Cases.con = Con.t

      structure Handler:
	 sig
	    datatype t =
	       CallerHandler
	     | None
	     | Handle of Label.t

	    val equals: t * t -> bool
	    val foreachLabel: t * (Label.t -> unit) -> unit
	    val layout: t -> Layout.t
	    val map: t * (Label.t -> Label.t) -> t
	 end

      (*
       * These correspond to 6 of the possible 9 combinations of continuation and
       * handler each being one of {None, Caller, Some l}.  None means that it
       * doesn't matter what the continuation (handler) is since the caller never
       * returns (raises).  Caller means to keep the continuation (handler) the same
       * as in the caller.  Some l means a nontail call in the case of continuations
       * and an installed handler in the case of handlers.
       *
       * 3 of the 9 possibilities are disallowed, and the correspondence is as below.
       *
       * Cont    Handler         equivalent
       * ------  -------         ---------------------------------------
       * None    None            Dead
       * None    Caller          HandleOnly
       * None    Some h          *disallowed*
       * Caller  None            *disallowed*
       * Caller  Caller          Tail
       * Caller  Some h          *disallowed*
       * Some l  None            Nontail {cont = l, handler = None}
       * Some l  Caller          Nontail {cont = l, handler = Caller}
       * Some l  Some h          Nontail {cont = l, handler = Handle l}
       *
       * We could have allowed the (None, Some h) and (Caller, Some h) cases, and
       * put some code in the backend to generate stubs, since if there is a handler
       * there must be some continuation.  But I decided it was easier to just rule
       * them out, essentially meaning that remove-unused, or any other optimization
       * pass, needs to make stubs itself.
       *)
      structure Return:
	 sig
	    datatype t =
	       Dead
	     | HandleOnly
	     | NonTail of {cont: Label.t, handler: Handler.t}
	     | Tail

	    val compose: t * t -> t
	    val foreachHandler: t * (Label.t -> unit) -> unit
	    val foreachLabel: t * (Label.t -> unit) -> unit
	    val isNonTail: t -> bool
	    val layout: t -> Layout.t
	    val map: t * (Label.t -> Label.t) -> t
	 end
      
      structure Transfer:
	 sig
	    datatype t =
	       Bug  (* MLton thought control couldn't reach here. *)
	     | Call of {func: Func.t,
			args: Var.t vector,
			return: Return.t}
	     | Case of {
			test: Var.t,
			cases: Label.t Cases.t,
			default: Label.t option  (* Must be nullary. *)
		       }
	     | Goto of {
			dst: Label.t,
			args: Var.t vector
			}
	     | Prim of {prim: Prim.t,
			args: Var.t vector,
			failure: Label.t, (* Must be nullary. *)
			success: Label.t (* Must be unary. *)
			}
	       (* Raise implicitly raises to the caller.  I.E. the local handler
		* stack must be empty.
		*)
	     | Raise of Var.t vector
	     | Return of Var.t vector

	    val foreachLabel: t * (Label.t -> unit) -> unit
	    val foreachLabelVar: t * (Label.t -> unit) * (Var.t -> unit) -> unit
	    val foreachVar: t * (Var.t -> unit) -> unit
	    val layout: t -> Layout.t
	    val replaceLabel: t * (Label.t -> Label.t) -> t
	    val replaceVar: t * (Var.t -> Var.t) -> t
	 end

      structure Block:
	 sig
	    datatype t =
	       T of {
		     label: Label.t,
		     args: (Var.t * Type.t) vector,
		     statements: Statement.t vector,
		     transfer: Transfer.t
		     }

	    val args: t -> (Var.t * Type.t) vector
	    val clear: t -> unit
	    val label: t -> Label.t
	    val layout: t -> Layout.t
	    val statements: t -> Statement.t vector
	    val transfer: t -> Transfer.t
	 end

      structure Datatype:
	 sig
	    datatype t =
	       T of {
		     tycon: Tycon.t,
		     cons: {
			    con: Con.t,
			    args: Type.t vector
			    } vector
		     }

	    val layout: t -> Layout.t
	 end

      structure Function:
	 sig
	    type t

	    val alphaRename: t -> t
	    val blocks: t -> Block.t vector
	    val checkHandlers: t -> unit
	    (* clear the plists for all bound variables and labels that appear
	     * in the function, but not the function name's plist.
	     *)
	    val clear: t -> unit
	    val controlFlow: t -> {graph: DirectedGraph.t,
				   labelNode: Label.t -> DirectedGraph.Node.t,
				   nodeBlock: DirectedGraph.Node.t -> Block.t}
	    val dest: t -> {args: (Var.t * Type.t) vector,
			    blocks: Block.t vector,
			    name: Func.t,
			    raises: Type.t vector option,
			    returns: Type.t vector option,
			    start: Label.t}
	    (* dfs (f, v) visits the blocks in depth-first order, applying v b
	     * for block b to yield v', then visiting b's descendents,
	     * then applying v' ().
	     *)
	    val dfs: t * (Block.t -> unit -> unit) -> unit
	    val dominatorTree: t -> Block.t Tree.t
	    val foreachVar: t * (Var.t * Type.t -> unit) -> unit
	    (* inferHandlers uses the HandlerPush and HandlerPop statements
	     * to infer the handler stack at the beginning of each block.
	     *)
	    val inferHandlers: t -> Label.t list option array
	    val layout: t * (Var.t -> string option) -> Layout.t
	    val layoutDot:
	       t * (Var.t -> string option) -> {graph: Layout.t,
						tree: unit -> Layout.t}
	    val name: t -> Func.t
	    val new: {args: (Var.t * Type.t) vector,
		      blocks: Block.t vector,
		      name: Func.t,
		      raises: Type.t vector option,
		      returns: Type.t vector option,
		      start: Label.t} -> t
	    val start: t -> Label.t
	 end
     
      structure Program:
	 sig
	    datatype t =
	       T of {
		     datatypes: Datatype.t vector,
		     globals: Statement.t vector,
		     functions: Function.t list,
		     main: Func.t (* Must be nullary. *)
		    } 

	    val checkHandlers: t -> unit
	    val clear: t -> unit
	    val clearTop: t -> unit
	    val foreachVar: t * (Var.t * Type.t -> unit) -> unit
	    val hasPrim: t * (Prim.t -> bool) -> bool
	    val layouts: t * (Layout.t -> unit) -> unit
	    val layoutStats: t -> Layout.t
	 end
   end
