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
	 end
      sharing Atoms = Type.Atoms

      structure Func: HASH_ID
      structure Label: HASH_ID

      structure PrimInfo:
	 sig
	    datatype t =
	       None
	     | Overflow of Label.t (* Must be nullary. *)
	 end

      structure Exp:
	 sig
	    datatype t =
	       ConApp of {con: Con.t,
			  args: Var.t vector}
	     | Const of Const.t
	     | PrimApp of {prim: Prim.t,
			   info: PrimInfo.t,
			   targs: Type.t vector,
			   args: Var.t vector}
	     | RestoreExnStack
	     | SaveExnStack
	     | Select of {tuple: Var.t,
			  offset: int}
	     | SetHandler of Label.t
	     | Tuple of Var.t vector
	     | Var of Var.t
	 end

      structure Statement:
	 sig
	    datatype t = T of {var: Var.t option, (* NONE iff ty = unit. *)
			       ty: Type.t,
			       exp: Exp.t}
	 end
      
      structure Cases: CASES sharing type Cases.con = Con.t
	 
      structure Transfer:
	 sig
	    datatype t =
	       Bug  (* MLton thought control couldn't reach here. *)
	     | Call of {
			func: Func.t,
			args: Var.t vector,
			cont: Label.t option (* NONE means tail-call *)
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
	     | Raise of Var.t vector
	     | Return of Var.t vector
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
	 end
      
      structure Program:
	 sig
	    datatype t =
	       T of {
		     datatypes: {
				 tycon: Tycon.t,
				 cons: {
					con: Con.t,
					args: Type.t vector
				       } vector
				} vector,
		     globals: Statement.t vector,
		     functions: Function.t vector,
		     main: Func.t (* Must be nullary. *)
		    } 
	 end
   end
