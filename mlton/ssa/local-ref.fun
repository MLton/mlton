

functor LocalRef (S: LOCAL_REF_STRUCTS): LOCAL_REF = 
struct

open S
open Exp Transfer

type int = Int.t
type word = Word.t

structure Graph = DirectedGraph
structure Node = Graph.Node

structure FuncInfo =
  struct

    datatype t = T of {once: bool}

    fun layout (T {once, ...})
      = let open Layout
	in record [("once", Bool.layout once)]
	end

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val once = make #once
    end

    fun new once: t = T {once = once}
  end
		       
structure GlobalInfo =
  struct
    datatype t = T of {isGlobalRef: bool,
		       funcs: Func.t list ref}

    fun layout (T {isGlobalRef, funcs, ...})
      = let open Layout
	in record [("isGlobalRef", Bool.layout isGlobalRef),
		   ("funcs", List.layout Func.layout (!funcs))]
	end

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val isGlobalRef = make #isGlobalRef
      val (funcs, funcs') = make' #funcs
    end

    fun new isGlobalRef = T {isGlobalRef = isGlobalRef, funcs = ref []}
  end

structure VarInfo =
  struct
    datatype t = T of {reff: (Label.t * Type.t) option,
		       assigns: Label.t list ref,
		       derefs: Label.t list ref,
		       isLocal: bool ref}

    fun layout (T {reff, assigns, derefs, isLocal, ...})
      = let open Layout
	in record [("reff", Option.layout (tuple2 (Label.layout, Type.layout)) reff),
		   ("assigns", List.layout Label.layout (!assigns)),
		   ("derefs", List.layout Label.layout (!derefs)),
		   ("isLocal", Bool.layout (!isLocal))]
	end

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val reff = make #reff
      val (assigns, assigns') = make' #assigns
      val (derefs, derefs') = make' #derefs
      val (isLocal, isLocal') = make' #isLocal
    end

    fun new reff: t = T {reff = reff, 
			 assigns = ref [],
			 derefs = ref [],
			 isLocal = ref (isSome reff)}
  end

structure LabelInfo =
  struct
    datatype t = T of {reffs: Var.t list ref,
		       assigns: Var.t list ref,
		       derefs: Var.t list ref}

    fun layout (T {reffs, assigns, derefs, ...})
      = let open Layout
	in record [("reffs", List.layout Var.layout (!reffs)),
		   ("assigns", List.layout Var.layout (!assigns)),
		   ("derefs", List.layout Var.layout (!derefs))]
	end

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val (reffs, reffs') = make' #reffs
      val (assigns, assigns') = make' #assigns
      val (derefs, derefs') = make' #derefs
    end

    fun new (): t = T {reffs = ref [],
		       assigns = ref [],
		       derefs = ref []}
  end

fun eliminate (program as Program.T {globals, datatypes, functions, main})
  = let
      exception NoLocalRefs

      (* Initialize funcInfo *)
      val {get = funcInfo: Func.t -> FuncInfo.t,
	   set = setFuncInfo, ...}
	= Property.getSetOnce
	  (Func.plist, Property.initRaise ("LocalRef.funcInfo", Func.layout))

      (* Initialize globalInfo *)
      val {get = globalInfo: Var.t -> GlobalInfo.t,
	   set = setGlobalInfo, ...}
	= Property.getSetOnce
	  (Var.plist, Property.initFun (fn _ => GlobalInfo.new false))

      val _ = Vector.foreach
	      (globals, fn Statement.T {var, exp, ...} =>
	       Option.app (var, fn var => 
			   case exp
			     of PrimApp {prim, ...}
			      => if Prim.name prim = Prim.Name.Ref_ref
				   then setGlobalInfo(var, GlobalInfo.new true)
				   else ()
			      | _ => ()))

      (* Update once and func *)
      fun addFunc f x
	= let
	    val gi = globalInfo x
	  in
	    if GlobalInfo.isGlobalRef gi
	      then if List.contains
		      (GlobalInfo.funcs' gi, f, Func.equals)
		     then ()
		     else List.push (GlobalInfo.funcs gi, f)
	      else ()
	  end
      val dummy = Func.newNoname ()
      val _ = setFuncInfo (dummy, FuncInfo.new true)
      val _ = Vector.foreach
	      (globals, fn Statement.T {exp, ...} =>
	       Exp.foreachVar (exp, addFunc dummy))
      val _ = List.foreach
	      (functions, fn f =>
	       let
		 val {name, blocks, ...} = Function.dest f
		 val _ = setFuncInfo (name, FuncInfo.new (Func.equals(name, main)))
		 val addFunc = addFunc name
	       in
		 Vector.foreach
		 (blocks, fn Block.T {statements, transfer, ...} =>
		  (Vector.foreach
		   (statements, fn Statement.T {exp, ...} =>
		    Exp.foreachVar (exp, addFunc)) ;
		   Transfer.foreachVar (transfer, addFunc)))
	       end)

      (* localize global refs *)
      val (functions,globals)
	= List.fold
	  (functions, ([],Vector.toList globals), fn (f, (functions, globals)) =>
	   if FuncInfo.once (funcInfo (Function.name f))
	     then let
		    val {name, args, start, blocks, returns, raises}
		      = Function.dest f

		    val (globals, locals)
		      = List.fold
		        (globals, ([],[]), fn (s as Statement.T {var, ...},
					       (globals, locals)) =>
			 if case var
			      of NONE => false
			       | SOME x 
			       => let
				    val gi = globalInfo x
				  in 
				    GlobalInfo.isGlobalRef gi
				    andalso
				    List.forall
				    (GlobalInfo.funcs' gi, fn f =>
				     Func.equals (f, name))
				  end
			   then (globals,s::locals)
			   else (s::globals,locals))

		    val locals = Vector.fromListRev locals
		    val f = if Vector.length locals = 0
			      then f
			      else let
				     val localsLabel = Label.newNoname ()
				     val localsBlock
				       = Block.T
				         {label = localsLabel,
					  args = Vector.new0 (),
					  statements = locals,
					  transfer = Goto {dst = start,
							   args = Vector.new0 ()}}
				     val blocks = Vector.concat 
				                  [Vector.new1 localsBlock,
						   blocks]
				   in
				     Function.new {name = name,
						   args = args,
						   start = localsLabel,
						   blocks = blocks,
						   returns = returns,
						   raises = raises}
				   end
		  in
		    (f::functions, List.rev globals)
		  end
	     else (f::functions, globals))
      val globals = Vector.fromList globals

      (* restore and shrink *)
      val restore = restoreFunction globals
      val restore = Control.trace (Control.Detail, "restore") restore
      val shrink = shrinkFunction globals

      (* varInfo *)
      val {get = varInfo: Var.t -> VarInfo.t,
	   set = setVarInfo,
	   rem = remVarInfo, ...} 
	= Property.getSetOnce
	  (Var.plist, Property.initFun (fn _ => VarInfo.new NONE))
      fun nonLocal x = VarInfo.isLocal (varInfo x) := false
      fun isLocal x = VarInfo.isLocal' (varInfo x)

      (* labelInfo *)
      val {get = labelInfo: Label.t -> LabelInfo.t, 
	   set = setLabelInfo, ...}
	= Property.getSetOnce
	  (Label.plist, Property.initRaise ("LocalRef.labelInfo", Label.layout))

      val functions
	= List.revMap
	  (functions, fn f =>
	   let
	     val {name, args, start, blocks, returns, raises} = Function.dest f

	     (* Find all localizable refs. *)
	     val refs = ref []
	     fun visitStatement label
				(s: Statement.t as Statement.T {var, ty, exp})
	       = let
		   val li = labelInfo label
		   fun setReff isRef
		     = Option.app
		       (var, fn var =>
			if isRef
			  then let
				 val reff = SOME (label, Type.deref ty)
			       in
				 setVarInfo (var, VarInfo.new reff) ;
				 List.push (LabelInfo.reffs li, var) ;
				 List.push (refs, var)
			       end
			  else setVarInfo (var, VarInfo.new NONE))
		   fun setAssign var
		     = (List.push (VarInfo.assigns (varInfo var), label) ;
			List.push (LabelInfo.assigns li, var))
		   fun setDeref var
		     = (List.push (VarInfo.derefs (varInfo var), label) ;
			List.push (LabelInfo.derefs li, var))
		   fun default () = Exp.foreachVar (exp, nonLocal)
		   datatype z = datatype Prim.Name.t
		 in
		   case exp
		     of PrimApp {prim, args, ...}
		      => let
			   fun arg n = Vector.sub (args, n)
			 in
			   case Prim.name prim
			     of Ref_ref => (setReff true; default ())
			      | Ref_assign => (setReff false;
					       setAssign (arg 0); 
					       nonLocal (arg 1))
			      | Ref_deref => (setReff false;
					      setDeref (arg 0))
			      | _ => (setReff false; default ())
			 end
		    | _ => (setReff false; default ())
		 end
	     fun visitBlock (Block.T {label, args, statements, transfer, ...})
	       = let
		   val li = LabelInfo.new ()
		   val _ = setLabelInfo (label, li)
		   val _ = Vector.foreach (statements, visitStatement label)
		   val _ = Transfer.foreachVar (transfer, nonLocal)
		 in
		   ignore
		 end
	     val _ = Function.dfs (f, visitBlock)
	     val refs = List.keepAll (!refs, isLocal)

	     (* Escape early when there are no localizable refs *)
	     val _ = if List.length refs = 0
		       then (Function.clear f;
			     Control.diagnostics
			     (fn display =>
			      let
				open Layout
			      in
				display (seq [Func.layout name,
					      str " NoLocalRefs"])
			      end);
			     raise NoLocalRefs)
		       else ()

	     (* Diagnostics *)
	     val _ = Control.diagnostics
	             (fn display =>
		      let
			open Layout
		      in
			display (seq [Func.layout name,
				      str " LocalRefs: ",
				      List.layout Var.layout (refs)])
		      end);

	     (* Rewrite. *)
	     fun rewriteStatement (s: Statement.t as Statement.T {var, ty, exp})
	       = let
		   datatype z = datatype Prim.Name.t
		 in
		   case exp
		     of PrimApp {prim, args, ...}
		      => let
			   fun arg n = Vector.sub (args, n)

			   fun rewriteReffAssign rvar var
			     = let
				 val vi = varInfo rvar
			       in
				 if VarInfo.isLocal' vi
				   then Statement.T
				        {var = SOME rvar,
					 ty = #2 (valOf (VarInfo.reff vi)),
					 exp = Var var}
				   else s
			       end
			   fun rewriteReff ()
			     = case var 
				 of NONE => s
				  | SOME var => rewriteReffAssign var (arg 0)
			   fun rewriteAssign () = rewriteReffAssign (arg 0) (arg 1)
			   fun rewriteDeref rvar
			     = let
				 val vi = varInfo rvar
			       in
				 if VarInfo.isLocal' vi
				   then let
					in
					  Statement.T
					  {var = var,
					   ty = #2 (valOf (VarInfo.reff vi)),
					   exp = Var rvar}
					end
				   else s
			       end
			   val rewriteDeref
			     = fn () => rewriteDeref (arg 0)
			 in
			   case Prim.name prim
			     of Ref_ref => rewriteReff ()
		              | Ref_assign => rewriteAssign ()
			      | Ref_deref => rewriteDeref ()
			      | _ => s
			 end
		      | _ => s
		 end
	     fun rewriteBlock (Block.T {label, args, statements, transfer})
	       = let
		   val li = labelInfo label
		   (* Don't need to rewrite the statements
		    * if this block doesn't mention localizable refs.
		    *)
		   val statements
		     = if List.exists (LabelInfo.reffs' li, isLocal)
		          orelse
			  List.exists (LabelInfo.assigns' li, isLocal)
			  orelse
			  List.exists (LabelInfo.derefs' li, isLocal)
			 then Vector.map (statements, rewriteStatement)
			 else statements
		 in
		   Block.T {label = label,
			    args = args,
			    statements = statements,
			    transfer = transfer}
		 end
	     val blocks = Vector.map (blocks, rewriteBlock)

	     val f = Function.new {name = name,
				   args = args,
				   start = start,
				   blocks = blocks,
				   returns = returns,
				   raises = raises}
	     val _ = Control.diagnostics
	             (fn display =>
		      display (Function.layout (f, fn _ => NONE)))
	     val f = restore f
	     val _ = Control.diagnostics
	             (fn display =>
		      display (Function.layout (f, fn _ => NONE)))
	     val f = shrink f
	     val _ = Control.diagnostics
	             (fn display =>
		      display (Function.layout (f, fn _ => NONE)))
	   in
	     f
	   end
	   handle NoLocalRefs => f)
      val program = Program.T {datatypes = datatypes,
			       globals = globals,
			       functions = functions,
			       main = main}
      val _ = Program.clearTop program
    in
      program
    end

fun usesConts p 
  = Program.hasPrim (p, fn p => Prim.name p = Prim.Name.Thread_switchTo)

structure NewOnce = NewOnce (S)

val eliminate 
  = fn p => (NewOnce.once p;
	     if usesConts p
	       then p
	       else eliminate p)

end
