(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature SSA_TREE_STRUCTS = 
   sig
      include ATOMS

      structure Cps: CPS
      sharing Atoms = Cps.Atoms
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
	     | PrimApp of {prim: Prim.t,
			   targs: Type.t vector,
			   args: Var.t vector}
	     | RestoreExnStack
	     | SaveExnStack
	     | Select of {tuple: Var.t,
			  offset: int}
	     | SetHandler of Label.t
	     | Tuple of Var.t vector
	     | Var of Var.t

	    val layout: t -> Layout.t
	 end

      structure Statement:
	 sig
	    datatype t = T of {var: Var.t option, (* NONE iff ty = unit. *)
			       ty: Type.t,
			       exp: Exp.t}

	    val lastHandler: t vector * Label.t option -> Label.t option
	    val layout: t -> Layout.t
	    val restoreExnStack: t
	    val saveExnStack: t
	    val setHandler: Label.t -> t
	    val var: t -> Var.t option
	 end
      
      structure Cases: CASES sharing type Cases.con = Con.t

      structure Transfer:
	 sig
	    datatype t =
	       Bug  (* MLton thought control couldn't reach here. *)
	     | Call of {
			func: Func.t,
			args: Var.t vector,
			return: Label.t option (* NONE means tail-call *)
		       }
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
	     | Raise of Var.t vector
	     | Return of Var.t vector

	    val layout: t -> Layout.t
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

	    val label: t -> Label.t
	    val layout: t -> Layout.t
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
	    datatype t =
	       T of {
		     name: Func.t,
		     args: (Var.t * Type.t) vector,
		     start: Label.t, (* Must be nullary. *)
		     blocks: Block.t vector,
		     returns: Type.t vector
		     }

	    val controlFlowGraph:
	       t * {labelHandler: Label.t -> Label.t option}
	       -> {graph: DirectedGraph.t,
		   labelNode: Label.t -> DirectedGraph.Node.t,
		   dominatorTree: unit -> Block.t Tree.t}
	    val dominatorTree:
	       t * {labelHandler: Label.t -> Label.t option}
	       -> Block.t Tree.t
	 end
      
      structure Program:
	 sig
	    datatype t =
	       T of {
		     datatypes: Datatype.t vector,
		     globals: Statement.t vector,
		     functions: Function.t vector,
		     main: Func.t (* Must be nullary. *)
		    } 

	    val clear: t -> unit
	    val fromCps: Cps.Program.t * {jumpToLabel: Cps.Jump.t -> Label.t,
					  funcToFunc: Cps.Func.t -> Func.t} -> t
	    val inferHandlers: t -> Label.t -> Label.t option
	    val layouts: t * (Layout.t -> unit) -> unit
	    val layoutStats: t -> Layout.t
	 end
   end
