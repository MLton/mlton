

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

    datatype t = T of {node: Node.t,
		       once: bool ref}

    fun layout (T {once, ...})
      = let open Layout
	in record [("once", Bool.layout (!once))]
	end

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val node = make #node
      val (once, once') = make' #once
    end

    fun new node: t = T {node = node,
			 once = ref false}
  end
		       
structure GlobalInfo =
  struct
    datatype t = T of {isGlobal: bool,
		       funcs: Func.t list ref}

    fun layout (T {isGlobal, funcs, ...})
      = let open Layout
	in record [("isGlobal", Bool.layout isGlobal),
		   ("funcs", List.layout Func.layout (!funcs))]
	end

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val isGlobal = make #isGlobal
      val (funcs, funcs') = make' #funcs
    end

    fun new isGlobal = T {isGlobal = isGlobal, funcs = ref []}
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
      val {get = nodeInfo: Node.t -> Func.t,
	   set = setNodeInfo, ...}
	= Property.getSetOnce
	  (Node.plist, Property.initRaise ("LocalRef.nodeInfo", Node.layout))

      val graph = Graph.new ()
      fun addEdge edge
	= (Graph.addEdge (graph, edge); ())

      val _ = List.foreach
	      (functions, fn f =>
	       let
		 val name = Function.name f
		 val n = Graph.newNode graph
	       in
		 setFuncInfo (name, FuncInfo.new n) ;
		 setNodeInfo (n, name)
	       end)

      (* Initialize globalInfo *)
      val {get = globalInfo: Var.t -> GlobalInfo.t,
	   set = setGlobalInfo, ...}
	= Property.getSetOnce
	  (Var.plist, Property.initFun (fn _ => GlobalInfo.new false))

      val _ = Vector.foreach
	      (globals, fn Statement.T {var, ...} =>
	       Option.app (var, fn var => setGlobalInfo(var, GlobalInfo.new true)))

      (* Update call and use counts *)
      val _ = List.foreach
	      (functions, fn f =>
	       let
		 val {name, blocks, ...} = Function.dest f
		 val f_node = FuncInfo.node (funcInfo name)

		 fun doit var 
		   = let
		       val gi = globalInfo var
		     in
		       if GlobalInfo.isGlobal gi
			 then if List.contains 
			         (GlobalInfo.funcs' gi, name, Func.equals)
				then ()
				else List.push (GlobalInfo.funcs gi, name)
			 else ()
		     end
	       in
		 Vector.foreach
		 (blocks, fn Block.T {statements, transfer, ...} =>
		  (Vector.foreach
		   (statements, fn Statement.T {exp, ...} =>
		    Exp.foreachVar (exp, doit)) ;
		   Transfer.foreachVar (transfer, doit) ;
		   case transfer 
		     of Call {func = g, ...} 
		      => let
			   val g_node = FuncInfo.node (funcInfo g)
			 in
			   addEdge {from = f_node, to = g_node}
			 end
		      | _ => ()))
	       end)

      (* Update once *)
      val _ = List.foreach
	      (Graph.stronglyConnectedComponents graph,
	       fn [] => ()
	        | [n] => if Node.hasEdge {from = n, to = n}
			   then FuncInfo.once (funcInfo (nodeInfo n)) := false
			   else FuncInfo.once (funcInfo (nodeInfo n)) := true
	        | ns => List.foreach
	                (ns, fn n =>
			 FuncInfo.once (funcInfo (nodeInfo n)) := false))

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

      val (functions, globals)
	= List.fold
	  (functions, ([],globals), fn (f,(functions,globals)) =>
	   let
	     fun cleanup (f, globals)
	       = (Vector.foreach (globals, fn Statement.T {var, ...} =>
				  Option.app (var, remVarInfo));
		  Function.clear f)

	     val {name, args, start, blocks, returns, raises} = Function.dest f
	     val fi = funcInfo name

	     (* Dummy block representing the initialization of globals. *)
	     val dummy = Label.newNoname ()
	     val dummyBlock = Block.T {label = dummy,
				       args = Vector.new0 (),
				       statements = globals,
				       transfer = Goto {dst = start,
							args = Vector.new0 ()}}

	     (* Find all localizable refs. *)
	     val refs = ref []

	     fun visitStatement checkStatement
	                        label
				(s: Statement.t as Statement.T {var, ty, exp})
	       = let
		   val li = labelInfo label
		   fun setReff isRef
		     = let 
			 val maybeSet = checkStatement s
			 val isRef = maybeSet andalso isRef
		       in 
			 Option.app
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
		       end
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

	     val checkGlobalStatement
	       = if FuncInfo.once' fi
		   then fn Statement.T {var = SOME var, ...}
		         => let
			      val gi = globalInfo var
			      val funcs = GlobalInfo.funcs' gi
			    in
			      if List.forall
				 (funcs, fn f => Func.equals (f, name))
				then true
				else false
			    end
		         | _ => false
		   else fn _ => false
	     val visitGlobalStatement = visitStatement checkGlobalStatement
	     val checkFuncStatement = fn _ => true
	     val visitFuncStatement = visitStatement checkFuncStatement

	     fun visitBlock visitStatement
	                    (Block.T {label, args, statements, transfer, ...})
	       = let
		   val li = LabelInfo.new ()
		   val _ = setLabelInfo (label, li)
		   val _ = Vector.foreach (statements, visitStatement label)
		   val _ = Transfer.foreachVar (transfer, nonLocal)
		 in
		   ignore
		 end
	     val visitGlobalBlock = visitBlock visitGlobalStatement
	     val visitFuncBlock = visitBlock visitFuncStatement

	     val post = visitGlobalBlock dummyBlock
	     val _ = Function.dfs (f, visitFuncBlock)
	     val _ = post ()

	     val refs = List.keepAll (!refs, isLocal)
	     (* Escape early when there are no localizable refs *)
	     val _ = if List.length refs = 0
		       then (cleanup (f, globals);
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
	     val (globals,dummyStatements)
	       = Vector.foldr
	         (globals, ([],[]), fn (s as Statement.T {var, ...},
					(globals,dummyStatements)) =>
		  if case var
		       of NONE => false
			| SOME x => VarInfo.isLocal' (varInfo x)
		    then (globals,s::dummyStatements)
		    else (s::globals,dummyStatements))
	     val globals = Vector.fromList globals
	     val dummyStatements = Vector.fromList dummyStatements
	     val dummyBlock = Block.T {label = dummy,
				       args = Vector.new0 (),
				       statements = dummyStatements,
				       transfer = Goto {dst = start,
							args = Vector.new0 ()}}

	     val blocks = Vector.concat [Vector.new1 dummyBlock, blocks]
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
				   start = dummy,
				   blocks = blocks,
				   returns = returns,
				   raises = raises}
	     val _ = cleanup (f, globals)
	     val _ = Control.diagnostics
	             (fn display =>
		      display (Function.layout (f, fn _ => NONE)))
	     val restore = Control.trace (Control.Detail, "restore")
                                         (restoreFunction globals)
	     val _ = Control.diagnostics
	             (fn display =>
		      display (Function.layout (f, fn _ => NONE)))
	     val f = restore f
	   in
	     ((f,true)::functions,globals)
	   end
	   handle NoLocalRefs => ((f,false)::functions,globals))
      val shrink = shrinkFunction globals
      val functions = List.revMap 
	              (functions, fn (f,b) => 
		       if b then shrink f else f)
      val program = Program.T {datatypes = datatypes,
			       globals = globals,
			       functions = functions,
			       main = main}
      val _ = Program.clearTop program
    in
      program
    end

fun usesConts p 
  = Program.hasPrim (p, fn p =>
		     case Prim.name p
		       of Prim.Name.Thread_copy => true
			| Prim.Name.Thread_copyShrink => true
			| Prim.Name.Thread_current => true
			| Prim.Name.Thread_finishHandler => true
			| Prim.Name.Thread_switchTo => true
			| Prim.Name.Thread_switchToCont => true
			| _ => false)

val eliminate = fn p => if usesConts p
			  then p
			  else eliminate p

end