
functor Phi (structure S: T) =
  struct

    type int = Int.t
    type word = Word.t

    datatype t = T of {value: S.t ref,
		       id: int ref,
		       preds: t list ref}

    fun layout (T {value, id, ...})
      = let open Layout
	in record [("value", S.layout (!value)),
		   ("id", Int.layout (!id))]
	end

    fun value (T {value, ...}) = !value

    val id = Counter.new 0
    val all : t list ref = ref []

    fun new x = let
		  val x = T {value = ref x,
			     id = ref (Counter.next id),
			     preds = ref []}
		in
		  List.push(all, x) ;
		  x
		end
		  
    fun equals (T {id = ref idA, ...}, T {id = ref idB, ...}) = idA = idB

    fun flow (a, b as T {preds = predsB, ...})
      = if List.contains (!predsB, a, equals)
	  then ()
	  else List.push (predsB, a)

    fun force (x as T {value = valueX, id = idX, preds = predsX, ...},
	       continue)
      = let
	  val preds' = List.removeAll(!predsX, fn z => equals (z, x))
	  val preds' = List.removeDuplicates(preds', equals)
	  val _ = predsX := preds'
	in
	  case preds'
	    of [y as T {value = ref valueY,
			id = ref idY,
			preds = ref predsY}]
	     => let
		in
		  (valueX := valueY;
		   idX := idY;
		   predsX := predsY;
		   continue ())
		end
	     | _ => ()
	end
      
    fun fixedPoint () 
      = (FixedPoint.fix'
	 (fn continue =>
	  List.foreach (!all, fn x => force (x, continue)));
	 all := [])

    fun reset () = all := []

    fun filter p
      = let
	  val p = fn T {value = ref v, ...} => p v
	in
	  all := List.keepAll
	         (!all, fn x as T {preds, ...} =>
		  if p x
		    then (preds := List.keepAll(!preds, p) ;
			  true)
		    else false)
	end
  end

functor LocalRef (S: LOCAL_FLATTEN_STRUCTS): LOCAL_REF = 
struct

open S
open Exp Transfer

type int = Int.t
type word = Word.t

structure Graph = DirectedGraph
structure Node = Graph.Node

structure Phi = Phi (structure S = Var)

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
		       isLocal: bool ref,
		       index: int ref,
		       vars: Var.t list ref}

    fun layout (T {reff, assigns, derefs, isLocal, index, ...})
      = let open Layout
	in record [("reff", Option.layout (tuple2 (Label.layout, Type.layout)) reff),
		   ("assigns", List.layout Label.layout (!assigns)),
		   ("derefs", List.layout Label.layout (!derefs)),
		   ("isLocal", Bool.layout (!isLocal)),
		   ("index", Int.layout (!index))]
	end

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val reff = make #reff
      val (assigns, assigns') = make' #assigns
      val (derefs, derefs') = make' #derefs
      val (isLocal, isLocal') = make' #isLocal
      val (index, index') = make' #index
      val (vars, vars') = make' #vars
    end

    fun new reff: t = T {reff = reff, 
			 assigns = ref [],
			 derefs = ref [],
			 isLocal = ref (isSome reff),
			 index = ref ~1,
			 vars = ref []}

    fun pushVar (T {vars, ...}, var) = List.push (vars, var)
    fun popVar (T {vars, ...}) = ignore (List.pop vars)
    fun peekVar (T {vars, ...}) = hd (!vars)
    fun switchVar (T {vars, ...}, var) = (ignore (List.pop vars);
					  List.push (vars, var))
  end

structure LabelInfo =
  struct
    datatype t = T of {args: (Var.t * Type.t) vector,
		       reffs: Var.t list ref,
		       assigns: Var.t list ref,
		       derefs: Var.t list ref,
		       refArgs: Var.t vector ref,
		       pre: Phi.t vector ref,
		       post: Phi.t vector ref,
		       preds: Label.t list ref,
		       isHandler: bool ref}

    fun layout (T {reffs, assigns, derefs, refArgs, isHandler, ...})
      = let open Layout
	in record [("reffs", List.layout Var.layout (!reffs)),
		   ("assigns", List.layout Var.layout (!assigns)),
		   ("derefs", List.layout Var.layout (!derefs)),
		   ("refArgs", Vector.layout Var.layout
		                             (!refArgs)),
		   ("isHandler", Bool.layout (!isHandler))]
	end

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val args = make #args
      val (reffs, reffs') = make' #reffs
      val (assigns, assigns') = make' #assigns
      val (derefs, derefs') = make' #derefs
      val (refArgs, refArgs') = make' #refArgs
      val (pre, pre') = make' #pre
      val (post, post') = make' #post
      val (preds, preds') = make' #preds
      val (isHandler, isHandler') = make' #isHandler
    end

    fun new args: t = T {args = args,
			 reffs = ref [],
			 assigns = ref [],
			 derefs = ref [],
			 refArgs = ref (Vector.new0 ()),
			 pre = ref (Vector.new0 ()),
			 post = ref (Vector.new0 ()),
			 preds = ref [],
			 isHandler = ref false}
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
		  Function.clear f;
		  Phi.reset ())

	     val {name, args, start, blocks, returns, raises} = Function.dest f
	     val fi = funcInfo name

	     (* Dummy block representing the initialization of globals. *)
	     val dummy = Label.newNoname ()
	     val dummyBlock = Block.T {label = dummy,
				       args = Vector.new0 (),
				       statements = globals,
				       transfer = Goto {dst = start,
							args = Vector.new0 ()}}

	     (* Find all localizable refs and initialize phi flow. *)
	     val refs = ref []
	     val index = ref 0
	     val refArgs = ref (Vector.new0 ())

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
	     val checkFuncStatement
	       = fn _ => true
	     val visitFuncStatement = visitStatement checkFuncStatement

	     fun visitBlock visitStatement
	                    (Block.T {label, args, statements, transfer, ...})
	       = let
		   val li = LabelInfo.new args
		   val _ = setLabelInfo (label, li)
		   val _ = Vector.foreach (statements, visitStatement label)
		   val _ = Transfer.foreachVar (transfer, nonLocal)

		   val oldIndex = !index
		   val oldRefArgs = !refArgs

		   val _ = LabelInfo.refArgs li := oldRefArgs

		   val newRefArgs = Vector.fromList (LabelInfo.reffs' li)
		   val _ = Vector.foreach
		           (newRefArgs, fn var =>
			    (VarInfo.index (varInfo var) := !index;
			     index := !index + 1))
		   val newIndex = !index
		   val newRefArgs = Vector.concat [oldRefArgs, newRefArgs]
		   val _ = refArgs := newRefArgs

		   val pre 
		     = Vector.tabulate
		       (oldIndex, fn i => 
			Phi.new (Vector.sub (oldRefArgs, i)))
		   val _ = LabelInfo.pre li := pre
		   val post 
		     = Vector.tabulate
                       (newIndex, fn i => 
			if i < oldIndex
			  then let
				 val x = Vector.sub (oldRefArgs, i)
			       in
				 if List.contains 
				    (LabelInfo.assigns' li, x, Var.equals)
				   then Phi.new (Vector.sub (newRefArgs, i))
				   else Vector.sub (pre, i)
			       end
			  else Phi.new (Vector.sub (newRefArgs, i)))
		   val _ = LabelInfo.post li := post
		 in
		   fn () => (index := oldIndex;
			     refArgs := oldRefArgs)
		 end
	     val visitGlobalBlock = visitBlock visitGlobalStatement
	     val visitFuncBlock = visitBlock visitFuncStatement

	     val post = visitGlobalBlock dummyBlock
	     val _ = Tree.traverse (Function.dominatorTree f, visitFuncBlock)
	     val _ = post ()

             (* Diagnostics *)
	     val _ = Control.diagnostics
	             (fn display =>
		      let 
			open Layout
		      in
			display (seq [Func.layout name,
				      str " ",
				      FuncInfo.layout fi]);
			List.foreach
			(!refs, fn var =>
			 display (seq [Var.layout var,
				       str " ",
				       VarInfo.layout (varInfo var)]))
		      end)

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

	     (* Complete phi flow. *)
	     fun visitBlock (Block.T {label, transfer, ...})
	       = let
		   val li = labelInfo label
		   val post = LabelInfo.post' li
		 in
		   case transfer
		     of Call {return = Return.NonTail 
			      {handler = Handler.Handle h, 
			       ...}, 
			      ...} 
		      => LabelInfo.isHandler (labelInfo h) := true
                      | _ => () ;
		   Transfer.foreachLabel
		   (transfer, fn l => 
		    let
		      val li = labelInfo l
		      val pre = LabelInfo.pre' li
		    in
		      List.push (LabelInfo.preds li, label) ;
		      Vector.foreachi
		      (pre, fn (i, pre') =>
		       let
			 val post' = Vector.sub (post, i)
		       in
			 Phi.flow (post', pre')
		       end)
		    end)
		 end
	     val _ = visitBlock dummyBlock
	     val _ = Vector.foreach (blocks, visitBlock)

	     (* Simplify phi flow. *)
	     val _ = Phi.filter isLocal
	     (* Solve phi flow. *)
	     val fixedPoint = Control.trace
	                      (Control.Pass, "Phi.fixedPoint")
                              Phi.fixedPoint
	     val _ = fixedPoint ()

	     (* Set ref args. *)
	     fun visitBlock (Block.T {label, ...})
	       = let
		   val li = labelInfo label
		   val refArgs = LabelInfo.refArgs li
		   val pre = LabelInfo.pre' li
		   val preds = LabelInfo.preds' li
		   val posts = List.map (preds, LabelInfo.post' o labelInfo)

		   val refArgs' 
		     = Vector.keepAllMapi
		       (!refArgs, fn (i, var) => 
			if isLocal var
			  then if List.forall
			          (posts, fn post =>
				   Phi.equals (Vector.sub(pre, i), 
					       Vector.sub(post, i)))
				 then NONE
				 else SOME var
			  else NONE)
		   val _ = refArgs := refArgs'

		   (* Compensate for handlers. *)
		   val _ = if LabelInfo.isHandler' li
			     then Vector.foreach (refArgs', nonLocal)
			     else ()
		 in
		   ()
		 end
	     val _ = visitBlock dummyBlock
	     val _ = Vector.foreach (blocks, visitBlock)

             (* Compensate for handlers. *)
	     fun visitBlock (Block.T {label, ...})
	       = let
		   val li = labelInfo label
		   val refArgs = LabelInfo.refArgs li

		   val refArgs' = Vector.keepAll (!refArgs, isLocal)
		   val _ = refArgs := refArgs'
		 in
		   ()
		 end
	     val _ = visitBlock dummyBlock
	     val _ = Vector.foreach (blocks, visitBlock)

             (* Diagnostics *)
	     val _ = Control.diagnostics
	             (fn display =>
		      let 
			open Layout
		      in
			Vector.foreach
			(blocks, fn Block.T {label, ...} =>
			 display (seq [Label.layout label,
				       str " ",
				       LabelInfo.layout (labelInfo label)]))
		      end)

	     (* Rewrite. *)
	     val blocks = ref []
	     fun rewriteStatement addPost 
                                  (s: Statement.t as Statement.T {var, ty, exp})
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
				   then let
					  val x = Var.new rvar
					in 
					  addPost (fn _ => VarInfo.popVar vi) ;
					  VarInfo.pushVar (vi, x);
					  Statement.T 
					  {var = SOME x,
					   ty = #2 (valOf (VarInfo.reff vi)),
					   exp = Var var}
					end
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
					   exp = Var (VarInfo.peekVar vi)}
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
	     fun route dst
	       = let
		   val li = labelInfo dst
		   val refArgs = LabelInfo.refArgs' li
		 in
		   if Vector.length refArgs = 0
		     then dst
		     else let
			    val newArgs = Vector.map
			                  (refArgs, fn var =>
					   VarInfo.peekVar (varInfo var))
			    val dst' = Label.new dst
			    val args = LabelInfo.args li
			    val args' = Vector.map (args, fn (x,ty) =>
						    (Var.new x, ty))
			    val args'' = Vector.concat [Vector.map(args', #1),
							newArgs]
			    val block = Block.T
			                {label = dst',
					 args = args',
					 statements = Vector.new0 (),
					 transfer = Goto {dst = dst,
							  args = args''}}
			  in
			    List.push (blocks, block) ;
			    dst'
			  end
		 end
	     fun rewriteTransfer t = Transfer.replaceLabel (t, route)
	     fun visitBlock' (Block.T {label, args, statements, transfer})
	       = let
		   val posts = ref []
		   fun addPost f = List.push (posts, f)
		   val li = labelInfo label
		   val refArgs = LabelInfo.refArgs' li
		   val args = if Vector.length refArgs = 0
				then args
				else let
				       val newArgs 
					 = Vector.map
					   (refArgs, fn var =>
					    let
					      val vi = varInfo var
					      val x = Var.new var
					      val ty = #2 (valOf (VarInfo.reff vi))
					    in
					      addPost (fn _ => VarInfo.popVar vi) ;
					      VarInfo.pushVar (vi, x) ;
					      (x, ty)
					    end)
				     in
				       Vector.concat [args, newArgs]
				     end
		   (* Don't need to rewrite the statements
		    * if this block doesn't mention localizable refs.
		    *)
		   val statements
		     = if List.exists (LabelInfo.reffs' li, isLocal)
		          orelse
			  List.exists (LabelInfo.assigns' li, isLocal)
			  orelse
			  List.exists (LabelInfo.derefs' li, isLocal)
			 then Vector.map (statements, rewriteStatement addPost)
			 else statements
		   val transfer = rewriteTransfer transfer
		 in
		   (Block.T {label = label,
			     args = args,
			     statements = statements,
			     transfer = transfer},
		    fn () => List.foreach (!posts, fn f => f ()))
		 end
	     fun visitBlock block
	       = let
		   val (block, post) = visitBlock' block
		 in
		   List.push (blocks, block) ;
		   post
		 end

	     local
	       val (Block.T {label, args, statements, transfer}, post) 
		 = visitBlock' dummyBlock
	       val _ = List.push 
		       (blocks, Block.T {label = label,
					 args = args,
					 statements = Vector.new0 (),
					 transfer = transfer})
	     in
	       val globals = statements
	       val post = post
	     end
	     val _ = Tree.traverse (Function.dominatorTree f, visitBlock)
	     val _ = post ()

	     val f = Function.new {name = name,
				   args = args,
				   start = dummy,
				   blocks = Vector.fromList (!blocks),
				   returns = returns,
				   raises = raises}

	     val _ = cleanup (f, globals)
	   in
	     (f::functions,globals)
	   end
	   handle NoLocalRefs => (f::functions,globals))
      val shrink = shrinkFunction globals
      val functions = List.revMap (functions, shrink)
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