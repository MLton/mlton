functor RemoveUnused (S: REMOVE_UNUSED_STRUCTS): REMOVE_UNUSED = 
struct

open S
open Exp Transfer

structure Used =
  struct
    structure L = TwoPointLattice (val bottom = "unused"
				   val top = "used")
    open L
    val use = makeTop
    val isUsed = isTop
    val whenUsed = addHandler
  end

structure Coned =
  struct
    structure L = TwoPointLattice (val bottom = "not coned"
				   val top = "coned")
    open L
    val con = makeTop
    val isConed = isTop
    val whenConed = addHandler
  end

structure Deconed =
  struct
    structure L = TwoPointLattice (val bottom = "not deconed"
				   val top = "deconed")
    open L
    val decon = makeTop
    val isDeconed = isTop
    val whenDeconed = addHandler
  end

structure Catches =
  struct
    structure L = TwoPointLattice (val bottom = "does not catch"
				   val top = "catches")
    open L
    val catch = makeTop
    val doesCatch = isTop
    val whenCatches = addHandler
  end

structure SideEffects =
  struct
    structure L = TwoPointLattice (val bottom = "does not side effect"
				   val top = "side effects")
    open L
    val sideEffect = makeTop
    val doesSideEffect = isTop
    val whenSideEffects = addHandler
  end

structure Terminates =
  struct
    structure L = TwoPointLattice (val bottom = "does not terminate"
				   val top = "terminates")
    open L
    val terminate = makeTop
    val doesTerminate = isTop
    val whenTerminates = addHandler
  end

structure Fails =
  struct
    structure L = TwoPointLattice (val bottom = "does not fail"
				   val top = "fails")
    open L
    val fail = makeTop
    val doesFail = isTop
    val whenFails = addHandler
  end

fun remove (program as Program.T {datatypes, globals, functions, main})
  = let
      val {get = varInfo: Var.t -> {used: Used.t}, ...}
	= Property.get
	  (Var.plist,
	   Property.initFun (fn _ => {used = Used.new ()}))
      local
	fun make s = (s o varInfo, ! o s o varInfo)
      in
	val usedVar = #used o varInfo
	val useVar = Used.use o usedVar
	val isUsedVar = Used.isUsed o usedVar
	val isNotUsedVar = not o isUsedVar
	fun whenUsedVar (x, th) = Used.whenUsed(usedVar x, th)
	fun flowVarVar (x, y) = Used.<=(usedVar x, usedVar y)
	fun flowVarTyVar ((x, _), y) = flowVarVar (x, y)
	fun flowVarVarTy (x, (y, _)) = flowVarVar (x, y)
	fun flowVarTyVarTy ((x, _), (y, _)) = flowVarVar (x, y)
	fun flowVarsVars (xs, ys) = Vector.foreach2(xs, ys, flowVarVar)
	fun flowVarTysVars (xs, ys) = Vector.foreach2(xs, ys, flowVarTyVar)
	fun flowVarsVarTys (xs, ys) = Vector.foreach2(xs, ys, flowVarVarTy)
	fun flowVarTysVarTys (xs, ys) = Vector.foreach2(xs, ys, flowVarTyVarTy)
      end

      val {get = tyInfo: Type.t -> 
	                 {deconed: bool ref}, 
	   ...}
	= Property.get
	  (Type.plist,
	   Property.initFun (fn _ => {deconed = ref false}))
      local
	fun make s = (s o tyInfo, ! o s o tyInfo)
      in
	val (deconedTy, deconedTy') = make #deconed
      end

      val {get = tyconInfo: Tycon.t -> 
	                    {cons: {con: Con.t, args: Type.t vector} vector,
			     numCons: int ref},
	   set = setTyconInfo, ...}
	= Property.getSetOnce
	  (Tycon.plist, 
	   Property.initRaise ("RemovedUnused.tyconInfo", Tycon.layout))
      local
	fun make s = (s o tyconInfo, ! o s o tyconInfo)
      in
	val consTycon = #cons o tyconInfo
	val (numConsTycon, numConsTycon') = make #numCons
      end

      val {get = conInfo: Con.t -> 
	                  {args: (Var.t * Type.t) vector,
			   coned: Coned.t,
			   deconed: Deconed.t,
			   dummy: Exp.t option ref,
			   tycon: Tycon.t},
	   set = setConInfo, ...}
	= Property.getSetOnce
	  (Con.plist, 
	   Property.initRaise ("RemoveUnused.conInfo", Con.layout))
      local
	fun make s = (s o conInfo, ! o s o conInfo)
      in
	val conedCon = #coned o conInfo
	val conCon = Coned.con o conedCon
	val isConedCon = Coned.isConed o conedCon
	fun whenConedCon (c, th) = Coned.whenConed(conedCon c, th)

	val deconedCon = #deconed o conInfo
	val deconCon = Deconed.decon o deconedCon
	val isDeconedCon = Deconed.isDeconed o deconedCon
	fun whenDeconedCon (c, th) = Deconed.whenDeconed(deconedCon c, th)

	val argsCon = #args o conInfo
	val (dummy, dummy') = make #dummy
	val tyconCon = #tycon o conInfo
      end
      val conInfo 
	= Trace.trace ("RemoveUnused.conInfo",
		       Con.layout,
		       fn {coned, deconed, ...} 
		        => Layout.record [("coned", Coned.layout coned),
					  ("deconed", Deconed.layout deconed)])
	              conInfo
      fun newConInfo (con, args, tycon)
	= setConInfo (con, {args = Vector.map(args, fn t => (Var.newNoname (), t)),
			    coned = Coned.new (),
			    deconed = Deconed.new (),
			    dummy = ref NONE,
			    tycon = tycon})


      val {get = funcInfo: Func.t -> 
	                   {used: Used.t,
			    args: (Var.t * Type.t) vector,
			    returns: (Var.t * Type.t) vector,
			    sideEffects: SideEffects.t,
			    terminates: Terminates.t,
			    fails: Fails.t,
			    bug: Label.t option ref,
			    bugConts: (Type.t vector * Label.t) list ref,
			    wrappers: Block.t list ref},
	   set = setFuncInfo, ...}
	= Property.getSetOnce
	  (Func.plist,
	   Property.initRaise ("RemoveUnused.funcInfo", Func.layout))
      local
	fun make s = (s o funcInfo, ! o s o funcInfo)
      in
	val usedFunc = #used o funcInfo
	val useFunc = Used.use o usedFunc
	val isUsedFunc = Used.isUsed o usedFunc
	fun whenUsedFunc (f, th) = Used.whenUsed(usedFunc f, th)

	val sideEffectsFunc = #sideEffects o funcInfo
	val sideEffectFunc = SideEffects.sideEffect o sideEffectsFunc
	val doesSideEffectFunc = SideEffects.doesSideEffect o sideEffectsFunc
	val doesNotSideEffectFunc = not o doesSideEffectFunc
	fun whenSideEffectsFunc (f, th) 
	  = SideEffects.whenSideEffects(sideEffectsFunc f, th)
	fun flowSideEffects (f, g) 
	  = SideEffects.<=(sideEffectsFunc f, sideEffectsFunc g)

	val terminatesFunc = #terminates o funcInfo
	val terminateFunc = Terminates.terminate o terminatesFunc
	val doesTerminateFunc = Terminates.doesTerminate o terminatesFunc
	val doesNotTerminateFunc = not o doesTerminateFunc
	fun whenTerminatesFunc (f, th) 
	  = Terminates.whenTerminates(terminatesFunc f, th)
	fun flowTerminates (f, g) 
	  = Terminates.<=(terminatesFunc f, terminatesFunc g)

	val failsFunc = #fails o funcInfo
	val failFunc = Fails.fail o failsFunc
	val doesFailFunc = Fails.doesFail o failsFunc
	val doesNotFailFunc = not o doesFailFunc
	fun whenFailsFunc (f, th) 
	  = Fails.whenFails(failsFunc f, th)
	fun flowFails (f, g) 
	  = Fails.<=(failsFunc f, failsFunc g)

	val argsFunc = #args o funcInfo
	val returnsFunc = #returns o funcInfo

	val (bugFunc, bugFunc') = make #bug
	val (bugContsFunc, bugContsFunc') = make #bugConts
	val (wrappersFunc, wrappersFunc') = make #wrappers
      end

      val {get = labelInfo: Label.t -> 
	                    {used: Used.t,
			     catches: Catches.t,
			     func: Func.t,
			     args: (Var.t * Type.t) vector,
			     wrappers: (Type.t vector * Label.t) list ref},
	   set = setLabelInfo, ...}
	= Property.getSetOnce
	  (Label.plist,
	   Property.initRaise ("RemoveUnused.labelInfo", Label.layout))
      local
	fun make s = (s o labelInfo, ! o s o labelInfo)
      in
	val usedLabel = #used o labelInfo
	val useLabel = Used.use o usedLabel
	val isUsedLabel = Used.isUsed o usedLabel
	fun whenUsedLabel (l, th) = Used.whenUsed(usedLabel l, th)

	val catchesLabel = #catches o labelInfo
	val catchLabel = Catches.catch o catchesLabel
	val doesCatchLabel = Catches.doesCatch o catchesLabel
	fun whenCatchesLabel (l, th) = Catches.whenCatches(catchesLabel l, th)

	val funcLabel = #func o labelInfo
	val argsLabel = #args o labelInfo
	val (wrappersLabel, wrappersLabel') = make #wrappers
      end



      fun visitLabel l = useLabel l
      fun visitLabelTh l = fn () => visitLabel l
      fun visitFunc f = useFunc f
      fun visitFuncTh f = fn () => visitFunc f

      fun visitVar (x: Var.t) = useVar x
      fun visitVars (xs: Var.t Vector.t) = Vector.foreach (xs, visitVar)
      
      fun visitExp (e: Exp.t)
	= case e
	    of ConApp {con, args} 
	     => (conCon con; 
		 flowVarTysVars(argsCon con, args))
	     | PrimApp {prim, targs, args}
	     => let
		  val _ = visitVars args
		  datatype z = datatype Type.dest
		  fun decon t
		    = let
			val {deconed, ...} = tyInfo t
		      in
			if !deconed
			  then ()
			  else (deconed := true;
				case Type.dest t
				  of Datatype t
				   => Vector.foreach
				      (consTycon t,
				       fn {con, ...} 
				        => (deconCon con;
					    Vector.foreach
					    (argsCon con, 
					     fn (x, t) => (useVar x; decon t))))
				   | Tuple ts => Vector.foreach (ts, decon)
				   | Vector t => decon t
				   | _ => ())
		      end
		in
		  case (Prim.name prim, Vector.length targs)
		    of (Prim.Name.MLton_eq, 1)
		     (* MLton_eq may be used on datatypes used as enums. *)
		     => decon (Vector.sub (targs, 0))
		     | (Prim.Name.MLton_equal, 1)
		     (* MLton_equal will be expanded by poly-equal into uses
		      * of constructors as patterns.
		      *)
		     => decon (Vector.sub (targs, 0))
(*		     | (Prim.Name.MLton_size, 1) => decon (Vector.sub (targs, 0)) *)
		     | _ => ()
		end
	     | Select {tuple, ...} => visitVar tuple
	     | Tuple xs => visitVars xs
	     | Var x => visitVar x
	     | _ => ()
      fun visitExpTh e = fn () => visitExp e

      fun visitStatement (f: Func.t,
			  s: Statement.t
			  as Statement.T {var, ty, exp})
	= if Exp.maySideEffect exp
	    then (sideEffectFunc f;
		  visitExp exp)
	    else Option.app(var, fn var => whenUsedVar(var, visitExpTh exp))
      fun visitStatements (f: Func.t,
			   ss: Statement.t Vector.t)
	= Vector.foreach (ss, fn s => visitStatement(f, s))

      fun visitTransfer (f: Func.t, t: Transfer.t)
	= case t
	    of Bug => ()
	     | Call {func, args, return}
	     => let

		in
		  flowVarTysVars(argsFunc func, args);
		  flowSideEffects(func, f);
		  case return
		    of SOME {cont, handler}
		     => (flowVarTysVarTys(argsLabel cont, returnsFunc func);
			 whenTerminatesFunc(func, visitLabelTh cont);
			 case handler
			   of SOME handler 
			    => (whenFailsFunc(func, fn () => catchLabel handler);
				whenFailsFunc(func, visitLabelTh handler))
			    | NONE => flowFails (func, f))
		     | NONE
		     => (flowVarTysVarTys(returnsFunc f, returnsFunc func);
			 flowTerminates(func, f);
			 flowFails(func, f));
		  visitFunc func
		end
	     | Case {test, cases, default}
	     => let
		  val _ = visitVar test
		  fun doit l = (Vector.foreach (l, fn (_, l) => visitLabel l);
				Option.app (default, visitLabel))
		in
		  case cases 
		    of Cases.Char l => doit l
		     | Cases.Int l => doit l
		     | Cases.Word l => doit l
		     | Cases.Word8 l => doit l
		     | Cases.Con cases
		     => if Vector.length cases = 0
			  then Option.app (default, visitLabel)
			  else let
				 val _ = Vector.foreach
				         (cases,
					  fn (con, l)
					   => (deconCon con;
					       flowVarTysVarTys(argsLabel l, 
								argsCon con);
					       whenConedCon(con, visitLabelTh l)))
				 val cons
				   = consTycon (tyconCon (#1 (Vector.sub(cases, 0))))
			       in
				 case default
				   of NONE => ()
				    | SOME l
				    => Vector.foreach
				       (cons,
					fn {con, ...}
					 => if Vector.exists
					       (cases, fn (c, _) => Con.equals(c, con))
					      then ()
					      else whenConedCon(con, visitLabelTh l))
			       end
		end
	     | Goto {dst, args} 
	     => (flowVarTysVars(argsLabel dst, args);
		 visitLabel dst)
	     | Prim {args, failure, success, ...} 
	     => (sideEffectFunc f;
		 visitVars args;
		 visitLabel failure;
		 visitLabel success)
	     | Raise xs 
	     => (failFunc f;
		 visitVars xs)
	     | Return xs 
	     => (terminateFunc f;
		 flowVarTysVars(returnsFunc f, xs))
      val visitTransfer
	= Trace.trace ("RemoveUnused.visitTransfer",
		       Layout.tuple2 (Func.layout, Transfer.layout),
		       Unit.layout)
	              visitTransfer

      fun visitBlock (b: Block.t as Block.T {label, statements, transfer, ...})
	= let
	    val f = funcLabel label
	  in
	    (visitStatements (f, statements);
	     visitTransfer (f, transfer))
	  end
      fun visitBlockTh b = fn () => visitBlock b

      (* Visit all reachable expressions. *)
      val _ = Vector.foreach
	      (datatypes,
	       fn Datatype.T {tycon, cons}
	        => (setTyconInfo (tycon, {cons = cons, numCons = ref ~1});
		    Vector.foreach 
		    (cons, 
		     fn {con, args} => newConInfo (con, args, tycon))))
      val _ = let
		fun doit c = (conCon c; deconCon c)
	      in
		doit Con.truee ; doit Con.falsee 
	      end
      val _ = Vector.foreach
	      (globals,
	       fn Statement.T {var, ty, exp}
	        => Option.app(var, 
			      fn var => whenUsedVar(var, visitExpTh exp)))
      val _ = List.foreach
	      (functions,
	       fn function 
	        => let
		     val {name, args, returns,
			  start, blocks, ...} = Function.dest function
		   in
		     setFuncInfo(name, {used = Used.new (),
					args = args,
					returns = Vector.map
					          (returns,
						   fn t => (Var.newNoname (), t)),
					sideEffects = SideEffects.new (),
					terminates = Terminates.new (),
					fails = Fails.new (),
					bug = ref NONE,
					bugConts = ref [],
					wrappers = ref []});
		     whenUsedFunc(name, visitLabelTh start);
		     Vector.foreach
		     (blocks,
		      fn block as Block.T {label, args, ...}
		       => (setLabelInfo(label, {used = Used.new (),
						catches = Catches.new (),
						func = name,
						args = args,
						wrappers = ref []});
			   whenUsedLabel(label, visitBlockTh block)))
		   end)
      val _ = visitFunc main

      (* Analysis is done,  Now build the resulting program. *)

      val datatypes
	= Vector.keepAllMap
	  (datatypes,
	   fn Datatype.T {tycon, cons} 
	    => let
	         val _ = Control.diagnostics
		         (fn display
		           => let open Layout
			      in display 
				 (seq [Tycon.layout tycon,
				       str ": ",
				       Vector.layout
				       (fn {con, ...}
					 => seq [Con.layout con,
						 record
						 [("isConedCon",
						   Bool.layout (isConedCon con)),
						  ("isDeconedCon", 
						   Bool.layout (isDeconedCon con)),
						  ("argsCon", 
						   Vector.layout
						   (Bool.layout o isUsedVar o #1)
						   (argsCon con))]])
				       cons])
			      end)

		 val r: Exp.t option ref = ref NONE
		 val cons 
		   = Vector.keepAllMap
		     (cons,
		      fn c as {con, args}
		       => let
			  in
			    case (isConedCon con, isDeconedCon con)
			      of (false, _) => NONE
			       | (true, true) => SOME {con = con,
						       args = Vector.keepAllMap
						              (argsCon con,
							       fn (x, t)
							        => if isUsedVar x
								     then SOME t
								     else NONE)}
			       | (true, false)
			       => let
				    val (e, res)
				      = case !r
					  of NONE 
					   => let
						val c = Con.newString "dummy"
						val targs = Vector.new0 ()
						val args = Vector.new0 ()
						val e = ConApp {con = c,
								args = args}
					      in
						r := SOME e ;
						newConInfo (c, targs, tycon) ;
						(e, SOME {con = c, 
							  args = targs})
					      end
					   | SOME e => (e, NONE)
				    val _ = (dummy con) := SOME e
				  in
				    res
				  end
			  end)
		 val num = Vector.length cons
		 val _ = numConsTycon tycon := num
	       in
		 if num = 0
		   then NONE
		   else SOME (Datatype.T {tycon = tycon, cons = cons})
	       end)

      fun getBugFunc f
	= case bugFunc' f
	    of SOME l => l
	     | NONE
	     => let
		  val l = Label.newNoname ()
		  val block = Block.T {label = l,
				       args = Vector.new0 (),
				       statements = Vector.new0 (),
				       transfer = Bug}
		  val _ = bugFunc f := SOME l
		  val _ = List.push(wrappersFunc f, block)
		in
		  l
		end

      fun getBugContFunc (f, args)
	= let
	    val tys = Vector.keepAllMap
	              (args, fn (x, ty) => if isUsedVar x
					     then SOME ty
					     else NONE)
	  in
	    case List.peek
	         (bugContsFunc' f,
		  fn (tys', l')
		   => Vector.length tys' = Vector.length tys
		      andalso
		      Vector.forall2
		      (tys', tys,
		       fn (ty', ty) => Type.equals(ty', ty)))
	      of SOME (_, l') => l'
	       | NONE
	       => let
		    val l' = Label.newNoname ()
		    val args' = Vector.keepAllMap
		                (args,
				 fn (x, ty) 
				  => if isUsedVar x
				       then SOME (Var.newNoname (), ty)
				       else NONE)
		    val (_, tys') = Vector.unzip args'
		    val args'' = Vector.new0 ()
		    val block = Block.T {label = l',
					 args = args',
					 statements = Vector.new0 (),
					 transfer = Goto {dst = getBugFunc f,
							  args = Vector.new0 ()}}
		    val _ = List.push(bugContsFunc f, (tys', l'))
		    val _ = List.push(wrappersFunc f, block)
		  in
		    l'
		  end
	  end
      fun getWrapperLabel (l, args)
	= if Vector.forall2
	     (args, argsLabel l,
	      fn ((x, _), (y, _)) => isUsedVar x = isUsedVar y)
	    then l
	    else let
		   val tys = Vector.keepAllMap
		             (args, fn (x, ty) => if isUsedVar x
						    then SOME ty
						    else NONE)
		 in 
		   case List.peek
		        (wrappersLabel' l,
		         fn (args', l') 
			  => Vector.length args' = Vector.length tys
			     andalso
			     Vector.forall2
			     (args', tys,
			      fn (ty', ty) => Type.equals(ty', ty)))
		     of SOME (_, l') => l'
		      | NONE
		      => let
			   val l' = Label.newNoname ()
			   val (args', args'')
			     = Vector.unzip
			       (Vector.map2
				(args, argsLabel l,
				 fn ((x, ty), (y, _))
			          => let
				       val z = Var.newNoname ()
				     in
				       (if isUsedVar x
					  then SOME (z, ty)
					  else NONE,
					if isUsedVar y
					  then SOME z
					  else NONE)
				     end))
			   val args' = Vector.keepAllMap(args', fn x => x)
			   val (_, tys') = Vector.unzip args'
			   val args'' = Vector.keepAllMap(args'', fn x => x)
			   val block = Block.T {label = l',
						args =  args',
						statements = Vector.new0 (),
						transfer = Goto {dst = l,
								 args = args''}}
			   val _ = List.push(wrappersLabel l, (tys', l'))
			   val _ = List.push(wrappersFunc (funcLabel l), block)
			 in
			   l'
			 end
		 end
      fun getOriginalWrapperLabel l
	= getWrapperLabel (l, 
			   Vector.map(argsLabel l, 
				      fn (_, t) => let 
						     val x = Var.newNoname ()
						   in
						     useVar x;
						     (x, t)
						   end))
      val getContWrapperLabel = getWrapperLabel
      val getConWrapperLabel = getWrapperLabel
      val getHandlerWrapperLabel = getOriginalWrapperLabel
      val getPrimFailureWrapperLabel = getOriginalWrapperLabel
      val getPrimSuccessWrapperLabel = getOriginalWrapperLabel


      fun simplifyExp (e: Exp.t): Exp.t
	= case e
	    of ConApp {con, args}
	     => if isDeconedCon con
		  then ConApp {con = con,
			       args = Vector.keepAllMap2
			              (args, argsCon con,
				       fn (x, (y, t)) => if isUsedVar y
							   then SOME x
							   else NONE)}
		  else valOf (dummy' con)
	     | _ => e
      val simplifyExp
	= Trace.trace ("RemoveUnused.simplifyExp",
		       Exp.layout,
		       Exp.layout)
                      simplifyExp

      fun simplifyStatement (f: Func.t,
			     s: Statement.t 
			     as Statement.T {var, ty, exp}): Statement.t option
	= case exp
	    of SetHandler l 
	     => if doesCatchLabel l
		  then SOME (Statement.T 
			     {var = var,
			      ty = ty,
			      exp = SetHandler (getHandlerWrapperLabel l)})
		  else NONE
	     | _ => let
		      fun doit' var
			= SOME (Statement.T {var = var,
					     ty = ty,
					     exp = simplifyExp exp})
		      fun doit var'
			= if Exp.maySideEffect exp
			    then doit' var
			    else if isSome var'
				   then doit' var'
				   else NONE
		    in
		      case var
			of SOME var => if isUsedVar var
					 then doit (SOME var)
					 else doit NONE
			 | NONE => doit NONE
		    end
      fun simplifyStatements (f: Func.t, 
			      ss: Statement.t Vector.t): Statement.t Vector.t
	= Vector.keepAllMap (ss, fn s => simplifyStatement(f, s))

      fun simplifyTransfer (f: Func.t, t: Transfer.t): Transfer.t
	= case t
	    of Call {func, args, return}
	     => let
		  fun call return
		    = let
			val args = Vector.keepAllMap2
			           (args, argsFunc func,
				    fn (x, (y, t)) => if isUsedVar y
							then SOME x
							else NONE)
		      in
			Call {func = func,
			      args = args,
			      return = return}
		      end
		in
		  case return
		    of SOME {cont, handler}
		     => if Vector.forall(argsLabel cont,
					 fn (x, t) => isNotUsedVar x)
		           andalso
			   doesNotSideEffectFunc func
			   andalso
			   doesTerminateFunc func
			   andalso
			   doesNotFailFunc func
			  then Goto {dst = cont, args = Vector.new0 ()}
			  else (case (doesTerminateFunc func, 
				      doesFailFunc func, 
				      handler)
				  of (true, true, handler) 
				   => call 
				      (SOME 
				       {cont = getContWrapperLabel
				               (cont, returnsFunc func),
				        handler = Option.map
				                  (handler, getHandlerWrapperLabel)})
		                   | (true, false, handler) 
				   => call
				      (SOME {cont = getContWrapperLabel
					            (cont, returnsFunc func),
					     handler = NONE})
		                   | (false, true, SOME handler)
				   => let
					val handler 
					  = SOME (getHandlerWrapperLabel handler)
				      in
					call
					(SOME {cont = getBugContFunc
					              (f, returnsFunc func),
					       handler = handler})
				      end
				   | (false, _, _)
				   => if Vector.forall
					 (returnsFunc f, 
					  fn (x, t) => not (isUsedVar x))
				        then call NONE
				        else call (SOME {cont = getBugContFunc 
							        (f, returnsFunc func), 
					 	 	 handler = NONE}))
                     | NONE => call NONE
		end
	     | Case {test, cases = Cases.Con cases, default}
	     => let
		  val cases 
		    = Vector.keepAllMap
		      (cases, 
		       fn (con, l) 
		        => if isConedCon con
			     then SOME (con, getConWrapperLabel (l, argsCon con))
			     else NONE)
		  fun keep default = Case {test = test,
					   cases = Cases.Con cases,
					   default = default}
		  fun none () = keep NONE
		in
		  case default
		    of NONE => none ()
		     | SOME l => if Vector.length cases = 0
				   then if isUsedLabel l
					  then Goto {dst = l, args = Vector.new0 ()}
					  else Bug
				   else let
					  val numCons
					    = numConsTycon' 
					      (tyconCon (#1 (Vector.sub(cases, 0))))
					in 
					  if Vector.length cases = numCons
					    then none ()
					    else keep (SOME l)
					end
		end
	     | Goto {dst, args}
	     => Goto {dst = dst, 
		      args = Vector.keepAllMap2
		             (args, argsLabel dst,
			      fn (x, (y, t)) => if isUsedVar y
						  then SOME x
						  else NONE)}
	     | Prim {prim, args, failure, success} 
	     => Prim {prim = prim,
		      args = args,
		      failure = getPrimFailureWrapperLabel failure,
		      success = getPrimSuccessWrapperLabel success}
	     | Return xs
	     => Return (Vector.keepAllMap2
			(xs, returnsFunc f,
			 fn (x, (y, t)) => if isUsedVar y
					     then SOME x
					     else NONE))
	     | _ => t
      val simplifyTransfer
	= Trace.trace ("RemoveUnused.simplifyTransfer",
		       Layout.tuple2 (Func.layout, Transfer.layout),
		       Transfer.layout)
	              simplifyTransfer

      fun simplifyBlock (b: Block.t
			 as Block.T {label, args, 
				     statements, transfer}): Block.t option
	= let
	    val {args, func, ...} = labelInfo label
	    val _ = Control.diagnostics
	            (fn display
		      => let open Layout
		         in display
			    (seq [Label.layout label,
				  str ": ",
				  record
				  [("isUsedLabel", Bool.layout (isUsedLabel label)),
				   ("argsLabel",
				    Vector.layout
				    (Bool.layout o isUsedVar o #1)
				    (argsLabel label))]])
			 end)
	  in 
	    if isUsedLabel label
	      then let
		     val args = Vector.keepAllMap
		                (args,
				 fn (x,ty) => if isUsedVar x
						then SOME (x,ty)
						else NONE)
		     val statements = simplifyStatements (func, statements)
		     val transfer = simplifyTransfer (func, transfer)
		   in
		     SOME (Block.T {label = label,
				    args = args,
				    statements = statements,
				    transfer = transfer})
		   end
	      else NONE
	  end
      fun simplifyBlocks (bs: Block.t Vector.t): Block.t Vector.t
	= Vector.keepAllMap (bs, simplifyBlock)


      val globals = simplifyStatements (main, globals)
(*
      val shrinkBlock = shrinkBlock globals
*)

      fun simplifyFunction (f: Function.t): Function.t option
	= let
	    val {name, blocks, start, ...} = Function.dest f
	    val {args, returns, wrappers, ...} = funcInfo name
	    val _ = Control.diagnostics
	            (fn display
		      => let open Layout
			 in display
			    (seq [Func.layout name,
				  str ": ",
				  record
				  [("isUsedFunc", Bool.layout (isUsedFunc name)),
				   ("doesSideEffectFunc",
				    Bool.layout (doesSideEffectFunc name)),
				   ("doesTerminateFunc",
				    Bool.layout (doesTerminateFunc name)),
				   ("doesFailFunc",
				    Bool.layout (doesFailFunc name)),
				   ("argsFunc",
				    Vector.layout
				    (Bool.layout o isUsedVar o #1)
				    (argsFunc name)),
				   ("returnsFunc",
				    Vector.layout
				    (Bool.layout o isUsedVar o #1)
				    (returnsFunc name))]])
			 end)
	  in
	    if isUsedFunc name
	      then let
		     val args = Vector.keepAllMap
		                (args,
				 fn (x, t) => if isUsedVar x
						then SOME (x, t)
						else NONE)

		     val blocks = simplifyBlocks blocks
		     val wrappers = Vector.fromList (!wrappers)
		     val blocks = Vector.concat [wrappers, blocks]
		     val blocks = (* shrinkBlocks *) blocks
		     val returns = Vector.keepAllMap
		                   (returns,
				    fn (x, t) => if isUsedVar x
						   then SOME t
						   else NONE)
		   in
		     SOME (Function.new {args = args,
					 blocks = blocks,
					 name = name,
					 returns = returns,
					 start = start})
		   end
	      else NONE
	  end
      fun simplifyFunctions (fs: Function.t List.t): Function.t List.t
	= List.keepAllMap (fs, simplifyFunction)

      val functions = simplifyFunctions functions

      val program = Program.T {datatypes = datatypes,
			       globals = globals,
			       functions = functions,
			       main = main}
    in
      program
    end
end
