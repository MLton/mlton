
(* Restore SSA
 *
 * Based primarily on Section 19.1 of Appel's "Modern Compiler Implementation in ML",
 * (but see the caveats in the comments below).
 * The main deviation is the calculation of liveness of the violating variables,
 * which is used to predicate the insertion of phi arguments.  This is due to 
 * the algorithm's bias towards imperative languages, for which it makes the 
 * assumption that all variables are defined in the start block and all variables
 * are "used" at exit.  
 * This is "optimized" for restoration of functions with small numbers of violating
 * variables -- use bool vectors to represent sets of violating variables.
 * Also, we use a Promize.t to suspend part of the dominance frontier computation.
 *
 * Requirements: no violation in globals
 *)

functor Restore (S: RESTORE_STRUCTS): RESTORE =
struct

open S
open Exp Transfer

structure LabelInfo =
  struct
    datatype t = T of {args: (Var.t * Type.t) vector ref,
		       preds: Label.t list ref,
		       defs: bool vector ref,
		       uses: bool vector ref,
		       live: bool array ref,
		       dtindex: int ref,
		       df: Label.t vector Promise.t ref,
		       phi: Var.t list ref,
		       phiArgs: Var.t vector ref,
		       queued: bool ref}

    fun layout (T {defs, uses, live, dtindex, df, phiArgs, ...})
      = let open Layout
	in record [("defs", Vector.layout Bool.layout (!defs)),
		   ("uses", Vector.layout Bool.layout (!uses)),
		   ("live", Array.layout Bool.layout (!live)),
		   ("dtindex", Int.layout (!dtindex)),
		   ("df", Promise.layout (Vector.layout Label.layout) (!df)),
		   ("phiArgs", Vector.layout Var.layout (!phiArgs))]
	end

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val (args, args') = make' #args
      val (preds, preds') = make' #preds
      val (defs, defs') = make' #defs
      val (uses, uses') = make' #uses
      val (live, live') = make' #live
      val (dtindex, dtindex') = make' #dtindex
      val (df, df') = make' #df
      val (phi, phi') = make' #phi
      val (phiArgs, phiArgs') = make' #phiArgs
      val (queued, queued') = make' #queued
    end

    fun new (): t = T {args = ref (Vector.new0 ()),
		       preds = ref [],
		       defs = ref (Vector.new0 ()),
		       uses = ref (Vector.new0 ()),
		       live = ref (Array.new0 ()),
		       dtindex = ref ~1,
		       df = ref (Promise.delay (fn () => Vector.new0 ())),
		       phi = ref [],
		       phiArgs = ref (Vector.new0 ()),
		       queued = ref false}
  end

structure Cardinality =
  struct
    datatype t = Zero
               | One
               | Many

    fun layout c
      = let open Layout
	in str (case c
		  of Zero => "zero"
		   | One => "one"
		   | Many => "many")
	end

    val equals: t * t -> bool = op =

    val inc: t -> t
      = fn Zero => One
         | _ => Many

    val isZero: t -> bool
      = fn Zero => true
         | _ => false
    val isMany: t -> bool 
      = fn Many => true
         | _ => false
   end

structure VarInfo =
  struct
    datatype t = T of {defs: Cardinality.t ref,
		       ty: Type.t ref,
		       index: int ref,
		       defSites: Label.t list ref,
		       useSites: Label.t list ref,
		       vars: Var.t list ref}

    fun layout (T {defs, index, defSites, useSites, vars, ...})
      = let open Layout
	in record [("defs", Cardinality.layout (!defs)),
		   ("index", Int.layout (!index)),
		   ("defSites", List.layout Label.layout (!defSites)),
		   ("useSites", List.layout Label.layout (!useSites)),
		   ("vars", List.layout Var.layout (!vars))]
	end

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val (defs,defs') = make' #defs
      val (index,index') = make' #index
      val (defSites,defSites') = make' #defSites
      val (useSites,useSites') = make' #useSites
      val (ty,ty') = make' #ty
      val (vars, vars') = make' #vars
    end
    fun addDef (T {defs, ...}) = defs := Cardinality.inc (!defs)
    fun addDefSite (T {defSites, ...}, l) = List.push(defSites, l)
    fun addUseSite (T {useSites, ...}, l) = List.push(useSites, l)
    val violates = Cardinality.isMany o defs'

    fun new (): t = T {defs = ref Cardinality.Zero,
		       index = ref ~1,
		       defSites = ref [],
		       useSites = ref [],
		       ty = ref Type.unit,
		       vars = ref []}

    fun pushVar (T {vars, ...}, var) = List.push (vars, var)
    fun popVar (T {vars, ...}) = ignore (List.pop vars)
    fun peekVar (T {vars, ...}) = case !vars
				    of [] => NONE
				     | h::_ => SOME h
  end

fun restoreFunction (globals: Statement.t vector)
  = let
      exception NoViolations

      val {get = varInfo: Var.t -> VarInfo.t,
	   rem = remVarInfo, ...}
	= Property.get
	  (Var.plist, Property.initFun (fn _ => VarInfo.new ()))

      val {get = labelInfo: Label.t -> LabelInfo.t,
	   rem = remLabelInfo, ...}
	= Property.get
	  (Label.plist, Property.initFun (fn _ => LabelInfo.new ()))

      fun cleanup f
	= (Vector.foreach (globals, fn Statement.T {var, ...} =>
			   Option.app (var, remVarInfo));
	   Function.clear f)

      fun mkQueue ()
	= let
	    val todo = ref []
	  in
	    {enque = fn (l, li) => let 
				     val queued = LabelInfo.queued li
				   in
				     if !queued
				       then ()
				       else (queued := true ; 
					     List.push (todo, (l,li)))
				   end,
	     deque = fn () => case !todo
				of [] => NONE
		                 | (l,li)::todo' 
				 => (todo := todo';
				     LabelInfo.queued li := false;
				     SOME (l,li))}
	  end

      fun mkPost ()
	= let
	    val post = ref []
	  in
	    {addPost = fn th => List.push (post, th),
	     post = fn () => List.foreach(!post, fn th => th ())}
	  end
    in
      fn (f: Function.t) =>
      let
	val {name, args, start, blocks, returns, raises} = Function.dest f

	(* check for violations *)
	val violations = ref []
	fun addDef (x, ty) 
	  = let
	      val vi = varInfo x
	    in
	      if VarInfo.violates vi
		then ()
		else (VarInfo.ty vi := ty ;
		      VarInfo.addDef vi ;
		      if VarInfo.violates vi
			then List.push (violations, x)
			else ())
	    end
	fun checkViolations ()
	  = (Vector.foreach(globals, fn Statement.T {var, ty, ...} =>
			    Option.app (var, fn x => addDef (x, ty)));
	     Function.foreachVar (f, addDef))
	val checkViolations = Control.trace (Control.Detail, "checkViolations")
	                                    checkViolations
	val _ = checkViolations ()

	(* violation in globals *)
	val _ = if Vector.exists
	           (globals, fn Statement.T {var, ...} =>
		    case var 
		      of NONE => false
		       | SOME x => VarInfo.violates (varInfo x))
		  then Error.bug "Restore.restore: violation in globals"
		  else ()

	(* escape early *)
	val _ = if List.length (!violations) = 0
		  then (Control.diagnostics
			(fn display =>
			 let
			   open Layout
			 in
			   display (seq [Func.layout name,
					 str " NoViolations"])
			 end);
			raise NoViolations)
		  else ()

	(* init violations *)
	val index = ref 0
	val violations
	  = Vector.fromListMap
	    (!violations, fn x =>
	     let
	       val vi = varInfo x
	       val _ = VarInfo.index vi := (!index)
	       val _ = Int.inc index
	     in
	       x
	     end)
	val numViolations = !index

	(* Diagnostics *)
	val _ = Control.diagnostics
	        (fn display =>
		 let
		   open Layout
		 in
		   display (seq [Func.layout name,
				 str " Violations: ",
				 Vector.layout Var.layout violations])
		 end)

	(* init entryBlock *)
	val entry = Label.newNoname ()
	val entryBlock = Block.T {label = entry,
				  args = args,
				  statements = Vector.new0 (),
				  transfer = Goto {dst = start,
						   args = Vector.new0 ()}}

	(* compute dominator tree *)
	fun dominatorTree () = Function.dominatorTree f
	val dominatorTree = Control.trace (Control.Detail, "dominatorTree")
                                          dominatorTree
	val dt = dominatorTree ()
	val dt' = Tree.T (entryBlock, Vector.new1 dt)

	(* compute df (dominance frontier) *)
	(*  based on section 19.1 of Appel's "Modern Compiler Implementation in ML" *)
	(* also computes defSites and useSites of violating variables *)
	(* also computes preds, defs, and uses *)
	val dtindex = ref 0
	fun doitTree (Tree.T (Block.T {label, args, statements, transfer},
			      children))
	  = let
	      val li = labelInfo label

	      val _ = LabelInfo.args li := args

	      val _ = Transfer.foreachLabel 
		      (transfer, fn l => 
		       List.push (LabelInfo.preds (labelInfo l), label))


	      val defs = Array.new (numViolations, false)
	      val uses = Array.new (numViolations, false)
	      val live = Array.new (numViolations, false)
	      fun addDef x
		= let 
		    val vi = varInfo x
		  in 
		    if VarInfo.violates vi
		      then let
			     val index = VarInfo.index' vi
			   in
			     VarInfo.addDefSite (varInfo x, label);
			     Array.update (defs, index, true);
			     Array.update (uses, index, false)
			   end
		      else ()
		  end	
	      fun addUse x
		= let 
		    val vi = varInfo x
		  in 
		    if VarInfo.violates vi
		      then let
			     val index = VarInfo.index' vi
			   in 
			     VarInfo.addUseSite (varInfo x, label);
			     Array.update (uses, index, true)
			   end
		      else ()
		  end
	      val _ = Transfer.foreachVar (transfer, addUse)
	      val _ = Vector.foreachr
		      (statements, fn Statement.T {var, exp, ...} =>
		       (Option.app (var, addDef);
			Exp.foreachVar (exp, addUse)))
	      val _ = Vector.foreach (args, addDef o #1)
	      val _ = LabelInfo.defs li := Array.toVector defs
	      val _ = LabelInfo.uses li := Array.toVector uses
	      val _ = LabelInfo.live li := live

	      val _ = Int.inc dtindex
	      val dtindexMin = !dtindex
	      val _ = LabelInfo.dtindex li := dtindexMin
	      val _ = Vector.foreach(children, doitTree)
	      val dtindexMax = !dtindex
	      fun dominates l 
		= let val dtindex = LabelInfo.dtindex' (labelInfo l)
		  in dtindexMin <= dtindex andalso dtindex <= dtindexMax
		  end

	      fun promise ()
		= let
		    val df = ref []
		    fun addDF l
		      = if List.contains(!df, l, Label.equals)
			  then ()
			  else List.push(df,l)
		    val _ = Transfer.foreachLabel
		            (transfer, fn l =>
			     if Vector.exists
			        (children, fn Tree.T (b, _) => 
				 Label.equals (Block.label b, l))
			       then ()
			       else addDF l)
		    val _ = Vector.foreach
		            (children, fn Tree.T (Block.T {label, ...}, _) =>
			     let
			       val li = labelInfo label
			     in
			       Vector.foreach
			       (Promise.force (LabelInfo.df' li), fn l =>
				if dominates l
				  then ()
				  else addDF l)
			     end)
		  in
		    Vector.fromList (!df)
		  end
	      val _ = LabelInfo.df li := Promise.delay promise
	    in
	      ()
	    end
	fun computeDF () = doitTree dt'
	val computeDF = Control.trace (Control.Detail, "computeDF")
                                      computeDF
	val _ = computeDF ()

	(* compute liveness *)
	fun computeLive ()
	  = Vector.foreach
	    (violations, fn x =>
	     let
	       val {enque, deque} = mkQueue ()
	       val enque = fn l => enque (l, labelInfo l)

	       val vi = varInfo x
	       val index = VarInfo.index' vi
	       val useSites = VarInfo.useSites' vi
	       val _ = List.foreach (useSites, enque)

	       fun doit (l,li)
		 = let
		     val defs = LabelInfo.defs' li
		     val live = LabelInfo.live' li
		   in
		     if Vector.sub(defs, index)
		        orelse
			Array.sub (live, index)
		       then ()
		       else (Array.update(live, index, true) ;
			     List.foreach (LabelInfo.preds' li, enque))
		   end
	       fun loop ()
		 = case deque ()
		     of NONE => ()
		      | SOME (l,li) => (doit (l, li); loop ())
	     in
	       loop ()
	     end)
	val computeLive = Control.trace (Control.Detail, "computeLive")
	                                computeLive
	val _ = computeLive ()

	(* insert phi-functions *)
	(*  based on section 19.1 of Appel's "Modern Compiler Implementation in ML"
	 *  (beware: Alg. 19.6 (both in the book and as corrected by the 
	 *   errata) has numerous typos; and this implementation computes sets of 
	 *   variables that must have phi-functions at a node, which is close to 
	 *   the algorithm in the book, but the reverse of the algorithm as 
	 *   corrected by the errata, which computes sets of nodes that must have 
	 *   a phi-functions for a variable.)
	 *)
	fun computePhi ()
	  = Vector.foreach
	    (violations, fn x =>
	     let
	       val {enque, deque} = mkQueue ()
		 
	       val vi = varInfo x
	       val index = VarInfo.index' vi
	       val defSites = VarInfo.defSites' vi
	       val _ = List.foreach
		       (defSites, fn l =>
			enque (l, labelInfo l))
		       
	       fun doit (l,li) 
		 = Vector.foreach
		   (Promise.force (LabelInfo.df' li), fn l =>
		    let
		      val li = labelInfo l
		      val live = LabelInfo.live' li
		      val phi = LabelInfo.phi li
		    in
		      if Array.sub(live, index) 
			 andalso
			 not (List.contains(!phi, x, Var.equals))
			then (List.push(phi, x);
			      enque (l, li))
			else ()
		    end)
	       fun loop ()
		 = case deque ()
		     of NONE => ()
		      | SOME (l,li) => (doit (l, li); loop ())
	     in
	       loop ()
	     end)
	val computePhi = Control.trace (Control.Detail, "computePhi")
                                       computePhi
	val _ = computePhi ()

	(* finalize phi args *)
	fun visitBlock (Block.T {label, ...}) 
	  = let
	      val li = labelInfo label
	      val phi = LabelInfo.phi li
	      val phiArgs = LabelInfo.phiArgs li
	    in
	      phiArgs := Vector.fromList (!phi) ;
	      phi := []
	    end
	val _ = visitBlock entryBlock
	val _ = Vector.foreach (blocks, visitBlock)

	(* Diagnostics *)
	val _ = Control.diagnostics
	        (fn display =>
		 let
		   open Layout
		 in
		   Vector.foreach
		   (violations, fn x =>
		    display (seq [Var.layout x,
				  str " ",
				  VarInfo.layout (varInfo x)]));
		   Vector.foreach
		   (blocks, fn Block.T {label, ...} =>
		    display (seq [Label.layout label,
				  str " ",
				  LabelInfo.layout (labelInfo label)]))
		 end)

	(* rewrite *)
	val blocks = ref []
	fun rewriteVar (x: Var.t)
	  = case VarInfo.peekVar (varInfo x)
	      of NONE => x
	       | SOME x' => x'
	fun rewriteStatement addPost
	                     (s: Statement.t as Statement.T {var, ty, exp})
	  = let
	      val exp = Exp.replaceVar (exp, rewriteVar)
	      val var
		= case var
		    of NONE => NONE
		     | SOME x => let
				   val vi = varInfo x
				 in 
				   if VarInfo.violates vi
				     then let
					    val x' = Var.new x
					  in
					    addPost (fn _ => VarInfo.popVar vi) ;
					    VarInfo.pushVar (vi, x');
					    SOME x'
					  end
				     else SOME x
				 end
	    in
	      Statement.T {var = var,
			   ty = ty,
			   exp = exp}
	    end
	local
	  type t = {dst: Label.t,
		    phiArgs: Var.t vector,
		    route: Label.t,
		    hash: Word.t}
	  val routeTable : t HashSet.t = HashSet.new {hash = #hash}
	in
	  fun route dst
	    = let
		val li = labelInfo dst
		val phiArgs = LabelInfo.phiArgs' li
	      in
		if Vector.length phiArgs = 0
		  then dst
		  else let
			 val phiArgs = Vector.map
		                        (phiArgs, valOf o VarInfo.peekVar o varInfo)
			 val hash = Vector.fold
			            (phiArgs, Label.hash dst, fn (x, h) =>
				     Word.xorb(Var.hash x, h))
			 val {route, ...} 
			   = HashSet.lookupOrInsert
			     (routeTable, hash, 
			      fn {dst = dst', phiArgs = phiArgs', ... } =>
			      Label.equals (dst, dst') 
			      andalso
			      Vector.equals (phiArgs, phiArgs', Var.equals),
			      fn () =>
			      let
				val route = Label.new dst
				val args = Vector.map 
				           (LabelInfo.args' li, fn (x,ty) =>
					    (Var.new x, ty))
				val args' = Vector.concat 
				            [Vector.map(args, #1),
					     phiArgs]
				val block = Block.T
				            {label = route,
					     args = args,
					     statements = Vector.new0 (),
					     transfer = Goto {dst = dst,
							      args = args'}}
				val _ = List.push (blocks, block)
			      in
				{dst = dst,
				 phiArgs = phiArgs,
				 route = route,
				 hash = hash}
			      end)
		       in
			 route
		       end
	      end
	end
	local
	  type t = {handlerWrap: Label.t, handlerRoute: Label.t}
	  val handlerWrapTable: t HashSet.t 
	    = HashSet.new {hash = fn {handlerRoute, ...} =>
			   Label.hash handlerRoute}
	  type t = {contWrap: Label.t, contRoute: Label.t, handlerWrap: Label.t}
	  val contWrapTable: t HashSet.t 
	    = HashSet.new {hash = fn {contRoute, handlerWrap, ...} =>
			   Word.xorb(Label.hash contRoute, Label.hash handlerWrap)}
	in
	  fun rewriteNonTailHandle {func, args, cont, handler}
	    = let
		val handlerRoute = route handler
		val {handlerWrap, ...}
		  = HashSet.lookupOrInsert
		    (handlerWrapTable, Label.hash handlerRoute,
		     fn {handlerRoute = handlerRoute', ...} =>
		     Label.equals (handlerRoute, handlerRoute'),
		     fn () =>
		     let
		       val handlerWrap = Label.new handler
		       val args = Vector.map
			          (LabelInfo.args' (labelInfo handler), 
				   fn (x,ty) => (Var.new x, ty))
		       val handlerWrapBlock
			 = Block.T
			   {label = handlerWrap,
			    args = args,
			    statements = Vector.new1
			                 (Statement.T
					  {var = NONE,
					   ty = Type.unit,
					   exp = HandlerPop handlerWrap}),
			    transfer = Goto {dst = handlerRoute,
					     args = Vector.map(args, #1)}}
		       val _ = List.push (blocks, handlerWrapBlock)
		     in
		       {handlerWrap = handlerWrap,
			handlerRoute = handlerRoute}
		     end)
		val contRoute = route cont
		val {contWrap, ...}
		  = HashSet.lookupOrInsert
		    (contWrapTable, 
		     Word.xorb(Label.hash contRoute, Label.hash handlerWrap),
		     fn {contRoute = contRoute', handlerWrap = handlerWrap', ...} =>
		     Label.equals (contRoute, contRoute') andalso
		     Label.equals (handlerWrap, handlerWrap),
		     fn () =>
		     let
		       val contWrap = Label.new cont
		       val args = Vector.map
			          (LabelInfo.args' (labelInfo cont),
				   fn (x,ty) => (Var.new x, ty))
		       val contWrapBlock
			 = Block.T
			   {label = contWrap,
			    args = args,
			    statements = Vector.new1
			                 (Statement.T
					  {var = NONE,
					   ty = Type.unit,
					   exp = HandlerPop handlerWrap}),
			    transfer = Goto {dst = contRoute,
					     args = Vector.map(args, #1)}}
		       val _ = List.push (blocks, contWrapBlock)
		     in
		       {contWrap = contWrap,
			contRoute = contRoute,
			handlerWrap = handlerWrap}
		     end)
		val callWrap = Label.newNoname ()
		val callWrapBlock 
		  = Block.T
		    {label = callWrap,
		     args = Vector.new0 (),
		     statements = Vector.new1
		                  (Statement.T
				   {var = NONE,
				    ty = Type.unit,
				    exp = HandlerPush handlerWrap}),
		     transfer = Call {func = func,
				      args = args,
				      return = Return.NonTail
				               {cont = contWrap,
						handler = Handler.Handle handlerWrap}}}
		val _ = List.push (blocks, callWrapBlock)
	      in
		Goto {dst = callWrap, args = Vector.new0 ()}
	      end
	end
	fun rewriteTransfer (t: Transfer.t) 
	  = let
	      fun default () = Transfer.replaceLabel (t, route)
	    in
	      case t
		of Call {func, args, 
			 return = Return.NonTail {cont,
						  handler = Handler.Handle handler}}
		 => if Vector.length (LabelInfo.phiArgs' (labelInfo handler)) = 0
		      then default ()
		      else rewriteNonTailHandle {func = func,
						 args = args,
						 cont = cont,
						 handler = handler}
	         | _ => default ()
	    end
	fun visitBlock' (Block.T {label, args, statements, transfer})
	  = let
	      val {addPost, post} = mkPost ()
	      val li = labelInfo label
	      fun doit x = let
			     val vi = varInfo x
			     val ty = VarInfo.ty' vi
			   in
			     if VarInfo.violates vi
			       then let
				      val x' = Var.new x
				    in
				      addPost (fn _ => VarInfo.popVar vi) ;
				      VarInfo.pushVar (vi, x') ;
				      (x', ty)
				    end
			       else (x, ty)
			   end
	      val args = Vector.map
		         (args, fn (x, _) => doit x)
	      val phiArgs = Vector.map
		            (LabelInfo.phiArgs' li, fn x => doit x)
	      val args = Vector.concat [args, phiArgs]
	      val statements 
		= if Vector.exists(LabelInfo.defs' li, fn b => b)
		     orelse
		     Vector.exists(LabelInfo.uses' li, fn b => b)
		    then Vector.map (statements, rewriteStatement addPost)
		    else statements
	      val transfer = rewriteTransfer transfer
	      val block = Block.T {label = label,
				   args = args,
				   statements = statements,
				   transfer = transfer}
	    in
	      (block, post)
	    end
	fun visitBlock block
	  = let 
	      val (block, post) = visitBlock' block
	    in
	      List.push (blocks, block) ;
	      post
	    end
	fun rewrite ()
	  = let
	      local
		val (Block.T {label, args, statements, transfer}, post) 
		  = visitBlock' entryBlock
		val entryBlock = Block.T {label = label,
					  args = Vector.new0 (),
					  statements = statements,
					  transfer = transfer}
		val _ = List.push (blocks, entryBlock)
	      in
		val args = args
		val post = post
	      end
	      val _ = Tree.traverse (Function.dominatorTree f, visitBlock)
	      val _ = post ()
	    in
	      Function.new {name = name,
			    args = args,
			    start = entry,
			    blocks = Vector.fromList (!blocks),
			    returns = returns,
			    raises = raises}
	    end
	val rewrite = Control.trace (Control.Detail, "rewrite")
                                    rewrite
	val f = rewrite ()

	val _ = cleanup f
      in
	f
      end
      handle NoViolations => (cleanup f; f)
    end

val traceRestoreFunction
  = Trace.trace ("Restore.restoreFunction",
		 Func.layout o Function.name,
		 Func.layout o Function.name)

val restoreFunction 
  = fn g =>
    let
      val r = restoreFunction g
    in
      fn f =>
      (traceRestoreFunction r f
       handle e => (Error.bug (concat ["restore raised ",
				       Layout.toString (Exn.layout e)])
		    ; raise e))
   end

fun restore (Program.T {datatypes, globals, functions, main})
  = let
      val r = restoreFunction globals
    in
      Program.T {datatypes = datatypes,
		 globals = globals,
		 functions = List.revMap (functions, r),
		 main = main}
    end
end