
type int = Int.t
type word = Word.t
  
signature RCPS_TREE_STRUCTS =
  sig
    include ATOMS
  end

signature RCPS_TREE =
  sig
    include ATOMS

    structure Func: ID
    structure Jump: ID
      
    structure Rtype:
      sig
	structure Basic:
	  sig
	    type char = Char.t
	    type byte = Word8.t
	    type int = Int32.int
	    type word = Word32.word
	      
	    datatype t
	      = Void of int
	      | Char of char vector option
	      | Byte of byte vector option
	      | Int of int vector option
	      | Word of word vector option
	      | Float
	  end

	structure Access:
	  sig
	    type t = {read: bool, write: bool}
	    val x: t
	    val r: t
	    val w: t
	    val rw: t
	  end

	datatype t
	  = Basic of Basic.t
	  | Tycon of Tycon.t
	  | Pointer of t
	  | Aggregate of (t * Access.t) vector
	  | Sum of t vector
	  | Array of Access.t * t
      end

(*
    structure Operand:
      sig
*)
	structure Loc:
	  sig
	    datatype t
	      = Select of {var: Var.t, offset: int}
	      | DeSelect of {var: Var.t, offset: int}
	      | ArrSelect of {var: Var.t, index: Var.t}
	  end

(*
	structure RValue:
	  sig
	    datatype t'
	      = Const of Const.t
	      | Var of Var.t
	    and t
	      = Allocate of t' vector
	      | Loc of Loc.t
	      | PrimApp of {prim: Prim.t,
			    info: unit,
			    targs: Rtype.t vector,
			    args: t' vector}
	  end
*)
(*
      end
*)
    structure PrimExp:
      sig
	datatype t 
	  = Const of Const.t
          | Var of Var.t
(*
          | PrimApp of {prim: Prim.t,
			info: unit,
			targs: Rtype.t vector,
			args: Var.t vector}
*)
	  | Allocate of Var.t vector
          | Loc of Loc.t
      end
    
    structure Stmt:
      sig
	datatype t
	  = Bind of {var: Var.t,
		     exp: PrimExp.t,
		     ty: Rtype.t}
	  | Update of {src: Loc.t,
		       dst: Loc.t}
	  | HandlerPush of Jump.t
	  | HandlerPop
      end

    structure Transfer:
      sig
	datatype t
	  = Bug (* MLton thought control couldn't reach here. *)
	  | Call of {func: Func.t,
		     args: Var.t vector,
		     cont: Jump.t option (* NONE means tail-call *)}
(*
	  | Case of {test: Var.t,
		     cases: Jump.t Cases.t,
		     default: Jump.t option (* jump is nullary *)}
*)
	  | Raise of Var.t vector
	  | Return of Var.t vector
      end

    structure Program:
      sig
	datatype t 
	  = T of {datatypes: {tycon: Tycon.t,
			      rep: Rtype.t} vector,
		  inits: {strings: (Var.t * string) list,
			  intInfs: (Var.t * string) list},
		  globals: {var: Var.t,
			    ty: Rtype.t,
			    exp: PrimExp.t} vector,
		  functions: {name: Func.t,
			      args: (Var.t * Rtype.t) vector,
			      returns: Rtype.t vector,
			      blocks: {name: Jump.t,
				       args: (Var.t * Rtype.t) vector,
				       body: {stmts: Stmt.t list,
					      transfer: Transfer.t}} vector,
			      start: Jump.t} vector,
		  main: Func.t}

(*
	val layout: t -> Layout.t
	val layoutStats: t -> Layout.t
*)
	val layouts: t * (Layout.t -> unit) -> unit
      end
    
  end
