
functor RcpsTree (S: RCPS_TREE_STRUCTS): RCPS_TREE =
struct

open S

structure Func =
   struct
      open Var (* Id (structure AstId = Ast.Var) *)
      fun newNoname () = newString "F"
   end

structure Jump =
   struct
      open Func
      fun newNoname () = newString "L"
   end

structure Rtype =
  struct
    structure Basic =
      struct
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

	local
	  open Layout
	  fun enum(e, lay)
	    = case e
		of NONE => str ""
		 | SOME es 
		 => seq [str "[",
			 seq (separate (Vector.toListMap(es, lay), "|")),
			 str "]"]
	in
	  fun layout b
	    = case b
		of Void(n) => seq [str "void", 
				   if n = 0
				     then str ""
				     else seq [str "{", Int.layout n, str "}"]]
		 | Char e => seq [str "char", enum(e, Char.layout)]
		 | Byte e => seq [str "byte", enum(e, Word8.layout)]
		 | Int e => seq [str "int", enum(e, Int.layout)]
		 | Word e => seq [str "word", enum(e, Word.layout)]
		 | Float => str "float"
	end
      end

    structure Access =
      struct
	type t = {read: bool, write: bool}

	val x = {read = false, write = false}
	val r = {read = true, write = false}
	val w = {read = false, write = true}
	val rw = {read = true, write = true}
 
	local
	  open Layout
	in
	  fun layout (t as {read, write})
	    = seq [str "<",
		   case (read, write)
		     of (true, true) => str "rw"
		      | (true, false) => str "r"
		      | (false, true) => str "w"
		      | (false, false) => str "",
		   str ">"]
	end
      end
 
    datatype t
      = Basic of Basic.t
      | Tycon of Tycon.t
      | Pointer of t
      | Aggregate of (t * Access.t) vector
      | Sum of t vector
      | Array of Access.t * t

    local 
      open Layout
    in
      val rec layout
	= fn Basic b => Basic.layout b
	   | Tycon t => Tycon.layout t
	   | Pointer t => seq [str "*", paren (layout t)]
	   | Aggregate tas
	   => (paren o seq)
	      (separateRight(Vector.toListMap(tas,
					      fn (t,access) 
					       => seq [layout t, 
						       Access.layout access]),
			     ", "))
	   | Sum ts
	   => (paren o mayAlign)
	      (separateRight(Vector.toListMap(ts, layout), " +"))
	   | Array (access, t) 
	   => seq [str "array", Access.layout access, paren (layout t)]
    end

  end

structure Loc =
  struct
    datatype t
      = Select of {var: Var.t, offset: int}
      | DeSelect of {var: Var.t, offset: int}
      | ArrSelect of {var: Var.t, index: Var.t}

    local
      open Layout
    in
      val rec layout
	= fn Select {var, offset} => seq [str "#", 
					  Int.layout offset,
					  str " ", 
					  Var.layout var]
           | DeSelect {var, offset} => seq [str "#", 
					    Int.layout offset,
					    str " ", 
					    str "*",
					    Var.layout var]
	   | ArrSelect {var, index} => seq [Var.layout var,
					    str "[",
					    Var.layout index,
					    str "]"]
    end
  end
    
structure PrimExp =
  struct
    datatype t 
      = Const of Const.t
      | Var of Var.t
(*
      | PrimApp of {prim: Prim.t,
	targs: }
*)
      | Allocate of Var.t vector
      | Loc of Loc.t

    local
      open Layout
    in
      val rec layout
	= fn Const c => Const.layout c
           | Var v => Var.layout v
	   | Loc l => Loc.layout l
	   | Allocate vs 
	   => seq [str "$",
		   (paren o seq)
		   (separateRight(Vector.toListMap(vs,
						   fn v => Var.layout v), 
				  ", "))]
    end
  end
    
structure Stmt =
  struct
    datatype t
      = Bind of {var: Var.t,
		 ty: Rtype.t,
		 exp: PrimExp.t}
      | Update of {src: Loc.t,
		   dst: Loc.t}
      | HandlerPush of Jump.t
      | HandlerPop
  end

structure Transfer =
  struct
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

structure Datatype =
  struct
    type t = {tycon: Tycon.t, rep: Rtype.t}

    fun layout (d as {tycon, rep}: t)
      = let open Layout
	in
	  seq [Tycon.layout tycon,
	       str " = ",
	       Rtype.layout rep]
	end
  end

structure Global =
  struct
    type t = {var: Var.t, ty: Rtype.t, exp: PrimExp.t}

    fun layout (g as {var, ty, exp}: t)
      = let open Layout
	in
	  seq [str "val ",
	       Var.layout var,
	       if !Control.showTypes
		 then seq [str ": ", Rtype.layout ty]
		 else empty,
	       str " = ", 
	       PrimExp.layout exp]
	end
  end



structure Program =
  struct
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

    fun layouts (T {datatypes, inits, globals, functions, main},
		 output': Layout.t -> unit)
      = let
	  open Layout
	  val output = output'
	in
	  output (align ((str "Datatypes:")::
			 Vector.toListMap (datatypes, Datatype.layout)));
	  output (str "\n\n");
	  output (align ((str "Globals:")::
			 Vector.toListMap (globals, Global.layout)));
	  output (str "\n\n")
	end
  end

end