(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
(*
 * Here are the rules for changing control transfers within f
 * when f is inlined at a call within g.
 *
 *				   Call to f within g
 *
 *				Tail		NonTail C
 * inside f			---------	---------
 *	Return			Return		Jump L
 *	Tail			Tail		NonTail C
 *	NonTail C'		NonTail C'	NonTail C'
 *)

(* All labels, variables, etc. in the program must be defined exactly once. *)

(* All raises must raise the same number of args and handlers must take this
 * same number of args.
 *)

(* For case statements, the type-checker allows a branch on a value carrying
 * constructor to go to a nullary jump, in this case the value is ignored.
 *)

type int = Int.t
type word = Word.t
   
signature CPS_TREE_STRUCTS = 
   sig
      include ATOMS
   end

signature CPS_TREE = 
   sig
      include CPS_TREE_STRUCTS

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

      structure Func: ID
      structure Jump: ID

      structure PrimInfo:
	 sig
	    datatype t =
	       None
	     | Overflow of Jump.t

	    val foreachJump: t * (Jump.t -> unit) -> unit
	    val replaceJump: t * (Jump.t -> Jump.t) -> t
	 end

      structure PrimExp:
	 sig
	    datatype t =
	       ConApp of {con: Con.t,
			  args: Var.t vector}
	     | Const of Const.t
	     | PrimApp of {prim: Prim.t,
			   info: PrimInfo.t,
			   targs: Type.t vector,
			   args: Var.t vector}
	     | Select of {tuple: Var.t,
			  offset: int}
	     | Tuple of Var.t vector
	     | Var of Var.t
	       
	    val foreachJumpVar: t * (Jump.t -> unit) * (Var.t -> unit) -> unit
	    val foreachVar: t * (Var.t -> unit) -> unit
	    val layout: t -> Layout.t
	    val maySideEffect: t -> bool
	    val replaceJumpVar: t * (Jump.t -> Jump.t) * (Var.t -> Var.t) -> t
	    val replaceVar: t * (Var.t -> Var.t) -> t
	    val unit: t
	 end

      structure Dec:
	 sig
	    type exp
	    datatype t =
	       Bind of {
			var: Var.t,
			ty: Type.t,
			exp: PrimExp.t
		       }
	     | Fun of {
		       name: Jump.t,
		       args: (Var.t * Type.t) vector,
		       body: exp
		      }
	     | HandlerPush of Jump.t
	     | HandlerPop

	    val layout: t -> Layout.t
	    val layoutBind: {var: Var.t, ty: Type.t, exp: PrimExp.t} -> Layout.t
	 end

      structure Cause:
	 sig
	    datatype t = User | Dispatch | PolyEqual | Coerce
	 end
      
      structure Cases: CASES sharing type Cases.con = Con.t
	 
      structure Transfer:
	 sig
	    datatype t =
	       Bug  (* MLton thought control couldn't reach here. *)
	     | Call of {
			func: Func.t,
			args: Var.t vector,
			cont: Jump.t option (* NONE means tail-call *)
		       }
	     | Case of {
			cause: Cause.t,
			test: Var.t,
			cases: Jump.t Cases.t,
			default: Jump.t option  (* jump is nullary *)
		       }
	     | Jump of {
			dst: Jump.t,
			args: Var.t vector
		       }
	     | Raise of Var.t vector
	     | Return of Var.t vector

	    val foreachFuncJumpVar:
	       t * (Func.t -> unit) * (Jump.t -> unit) * (Var.t -> unit) -> unit
	    val foreachJumpVar: t * (Jump.t -> unit) * (Var.t -> unit) -> unit
	    val foreachJump: t * (Jump.t -> unit) -> unit
	    val foreachVar: t * (Var.t -> unit) -> unit
	    val layout: t -> Layout.t
	    val layoutCase: {
			     cause: Cause.t,
			     test: Var.t,
			     cases: Jump.t Cases.t,
			     default: Jump.t option  (* jump is nullary *)
			     } -> Layout.t
	    val replaceVar: t * (Var.t -> Var.t) -> t
	 end

      structure Exp:
	 sig
	    type t
	       
	    (* alphaRename renames all bound variables and jumps in exp.
	     * Each free occurrence of a formal in the exp will be replaced
	     * by the corresponding actual.
	     * handleJump is applied to each jump that is renamed.
	     *)
	    val alphaRename: {exp: t,
			      substitution: {formal: Var.t,
					     actual: Var.t} list,
			      handleJump: {old: Jump.t,
					   new: Jump.t} -> unit
			      } -> t
	    val bug: t
	    val clear: t -> unit
	    val decs: t -> Dec.t list
	    val dest: t -> {decs: Dec.t list,
			    transfer: Transfer.t}
	    (* foreach' is different from foreach in that it only visits a dec
	     * before its scope and not after.
	     *)
	    val foreach': t * {handleDec: Dec.t -> unit,
			       handleTransfer: Transfer.t -> unit
			       } -> unit
	    (* foreach visits binding occurrences before uses.
	     * handleDec is applied to a dec before visiting everything that
	     * is in the scope of the dec.  It produces a function
	     * f: unit -> unit
	     * that is applied after everything in the scope has been visited.
	     *)
	    val foreach: t * {handleDec: Dec.t -> unit -> unit,
			      handleTransfer: Transfer.t -> unit
			      } -> unit
	    val foreachBind: t * ({var: Var.t,
				   ty: Type.t,
				   exp: PrimExp.t} -> unit) -> unit
	    val foreachCall: t * ({func: Func.t,
				   args: Var.t vector,
				   cont: Jump.t option} -> unit) -> unit
	    val foreachDec: t * (Dec.t -> unit) -> unit
	    val foreachTransfer: t * (Transfer.t -> unit) -> unit
	    (* foreachVar == foreach BOUND var *)
	    val foreachVar: t * (Var.t * Type.t -> unit) -> unit
	    val fromTransfer: Transfer.t -> t
	    val layout: t -> Layout.t
	    val make: {decs: Dec.t list,
		       transfer: Transfer.t} -> t
	    val prefix: t * Dec.t -> t
	    val prefixs: t * Dec.t list -> t
	    val transfer: t -> Transfer.t
	 end
      sharing type Dec.exp = Exp.t

      structure DirectExp:
	 sig
	    type t

	    val + : t * t -> t (* no overflow checking *)
	    val call: {func: Func.t,
		       args: t vector,
		       ty: Type.t} -> t
	    datatype cases =
	       Char of (char * t) vector
	     | Con of {con: Con.t,
		       args: (Var.t * Type.t) vector,
		       body: t} vector
	     | Int of (int * t) vector
	     | Word of (word * t) vector
	     | Word8 of (Word8.t * t) vector
	    val casee: {
			cause: Cause.t,
			test: t,
			cases: cases,
			default: t option,
			ty: Type.t (* of the entire case expression *)
			} -> t
	    val conApp: {con: Con.t,
			 args: t vector,
			 ty: Type.t} -> t
	    val const: Const.t -> t
	    val detuple: {tuple: t,
			  body: Var.t vector -> t} -> t
	    val detupleBind: {tuple: t,
			      components: Var.t vector,
			      body: t} -> t
	    val eq: t * t * Type.t -> t
	    val falsee: t
	    val handlee: {try: t,
			  ty: Type.t,
			  catch: Var.t * Type.t,
			  handler: t} -> t
	    val int: int -> t
	    val layout: t -> Layout.t
	    val lett: {decs: {
			      var: Var.t,
			      ty: Type.t,
			      exp: t
			      } list,
		       body: t} -> t
	    val name2: t * t * (Var.t * Var.t -> t) -> t
	    val primApp: {prim: Prim.t,
			  targs: Type.t vector,
			  args: t vector,
			  ty: Type.t} -> t
	    val primApp': {prim: Prim.t,
			   overflow: t option,
			   targs: Type.t vector,
			   args: t vector,
			   ty: Type.t} -> t
	    val raisee: t -> t
	    val select: {tuple: t,
			 offset: int,
			 ty: Type.t} -> t
	    val send: t * (PrimExp.t * Type.t -> Exp.t) -> Exp.t
	    val seq: t * t -> t
	    val toExp: t -> Exp.t
	    val truee: t
	    val tuple: {exps: t vector, ty: Type.t} -> t
	    val var: Var.t * Type.t -> t
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
		     globals: {
			       var: Var.t,
			       ty: Type.t,
			       exp: PrimExp.t
			      } vector,
		     functions: {
				 name: Func.t,
				 args: (Var.t * Type.t) vector,
				 body: Exp.t,
				 returns: Type.t vector
				} vector,
		     main: Func.t (* must be nullary *)
		    } 

	    val clear: t -> unit
	    val foreachVar: t * (Var.t * Type.t -> unit) -> unit
	    val hasPrim: t * (Prim.t -> bool) -> bool
	    val layout: t -> Layout.t
	    val layouts: t * (Layout.t -> unit) -> unit
	    val layoutStats: t -> Layout.t
	 end

      val deltaHandlers: Dec.t * Jump.t list -> Jump.t list
      val inferHandlers: Program.t -> Jump.t -> Jump.t list
   end
