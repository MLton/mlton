(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
type word = Word.t

signature SSA_TREE_STRUCTS = 
   sig
      include ATOMS
   end

signature LABEL = HASH_ID

signature HANDLER =
   sig
      structure Label: LABEL

      datatype t =
	 Caller
       | Dead
       | Handle of Label.t

      val equals: t * t -> bool
      val foldLabel: t * 'a * (Label.t * 'a -> 'a) -> 'a
      val foreachLabel: t * (Label.t -> unit) -> unit
      val layout: t -> Layout.t
      val map: t * (Label.t -> Label.t) -> t
   end

signature RETURN =
   sig
      structure Label: LABEL

      structure Handler: HANDLER
      sharing Label = Handler.Label

      datatype t =
	 Dead
       | NonTail of {cont: Label.t,
		     handler: Handler.t}
       | Tail
	       
      val compose: t * t -> t
      val foldLabel: t * 'a * (Label.t * 'a -> 'a) -> 'a
      val foreachHandler: t * (Label.t -> unit) -> unit
      val foreachLabel: t * (Label.t -> unit) -> unit
      val layout: t -> Layout.t
      val map: t * (Label.t -> Label.t) -> t
   end

signature SSA_TREE = 
   sig
      include SSA_TREE_STRUCTS

      structure Type:
	 sig
	    include HASH_TYPE
	       
	    datatype dest =
	       Array of t
	     | Char
	     | Datatype of Tycon.t
	     | Int
	     | IntInf
	     | Pointer
	     | PreThread
	     | Real
	     | Ref of t
	     | Thread
	     | Tuple of t vector
	     | Vector of t
	     | Word
	     | Word8

	    val dest: t -> dest
	    val tyconArgs: t -> Tycon.t * t vector
	 end
      sharing Atoms = Type.Atoms

      structure Func: HASH_ID
      structure Label: LABEL
      
      structure Exp:
	 sig
	    datatype t =
	       ConApp of {con: Con.t,
			  args: Var.t vector}
	     | Const of Const.t
	     | PrimApp of {prim: Prim.t,
			   targs: Type.t vector,
			   args: Var.t vector}
	     | Profile of ProfileExp.t
	     | Select of {tuple: Var.t,
			  offset: int}
	     | Tuple of Var.t vector
	     | Var of Var.t

	    val equals: t * t -> bool
	    val foreachVar: t * (Var.t -> unit) -> unit
	    val isProfile: t -> bool
	    val hash: t -> Word.t
	    val layout: t -> Layout.t
	    val maySideEffect: t -> bool
	    val replaceVar: t * (Var.t -> Var.t) -> t
	    val toString: t -> string
	    val unit: t
	 end

      structure Statement:
	 sig
	    datatype t = T of {var: Var.t option,
			       ty: Type.t,
			       exp: Exp.t}

	    val clear: t -> unit (* clear the var *)
	    val equals: t * t -> bool
	    val exp: t -> Exp.t
	    val layout: t -> Layout.t
	    val prettifyGlobals: t vector -> (Var.t -> string option)
	    val profile: ProfileExp.t -> t
	    val var: t -> Var.t option
	 end
      
      structure Cases: CASES sharing type Cases.con = Con.t

      structure Handler: HANDLER
      sharing Handler.Label = Label

      structure Return: RETURN
      sharing Return.Handler = Handler

      structure Transfer:
	 sig
	    datatype t =
	       Arith of {prim: Prim.t,
			 args: Var.t vector,
			 overflow: Label.t, (* Must be nullary. *)
			 success: Label.t, (* Must be unary. *)
			 ty: Type.t} (* int or word *)
	     | Bug  (* MLton thought control couldn't reach here. *)
	     | Call of {args: Var.t vector,
			func: Func.t,
			return: Return.t}
	     | Case of {test: Var.t,
			cases: Label.t Cases.t,
			default: Label.t option (* Must be nullary. *)
		       }
	     | Goto of {dst: Label.t,
			args: Var.t vector
			}
	     (* Raise implicitly raises to the caller.  
	      * I.E. the local handler stack must be empty.
	      *)
	     | Raise of Var.t vector
	     | Return of Var.t vector
	     | Runtime of {prim: Prim.t,
			   args: Var.t vector,
			   return: Label.t (* Must be nullary. *)
			  }

	    val equals: t * t -> bool
	    val foreachFunc : t * (Func.t -> unit) -> unit
	    val foreachLabel: t * (Label.t -> unit) -> unit
	    val foreachLabelVar: t * (Label.t -> unit) * (Var.t -> unit) -> unit
	    val foreachVar: t * (Var.t -> unit) -> unit
	    val hash: t -> Word.t 
	    val iff: Var.t * {falsee: Label.t, truee: Label.t} -> t
	    val layout: t -> Layout.t
	    val replaceLabelVar: t * (Label.t -> Label.t) * (Var.t -> Var.t) -> t
	    val replaceLabel: t * (Label.t -> Label.t) -> t
	    val replaceVar: t * (Var.t -> Var.t) -> t
	 end

      structure Block:
	 sig
	    datatype t =
	       T of {
		     args: (Var.t * Type.t) vector,
		     label: Label.t,
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
	    (* clear the plists for all bound variables and labels that appear
	     * in the function, but not the function name's plist.
	     *)
	    val clear: t -> unit
	    val controlFlow:
	       t -> {graph: unit DirectedGraph.t,
		     labelNode: Label.t -> unit DirectedGraph.Node.t,
		     nodeBlock: unit DirectedGraph.Node.t -> Block.t}
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
	    val layout: t -> Layout.t
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
	    val profile: t * SourceInfo.t -> t
	    val start: t -> Label.t
	 end
     
      structure Program:
	 sig
	    datatype t =
	       T of {
		     datatypes: Datatype.t vector,
		     functions: Function.t list,
		     globals: Statement.t vector,
		     main: Func.t (* Must be nullary. *)
		    } 

	    val clear: t -> unit
	    val clearTop: t -> unit
	    val foreachVar: t * (Var.t * Type.t -> unit) -> unit
	    val hasPrim: t * (Prim.t -> bool) -> bool
	    val layouts: t * (Layout.t -> unit) -> unit
	    val layoutStats: t -> Layout.t
	 end
   end
