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

structure MayReturn =
  struct
    structure L = TwoPointLattice (val bottom = "does not return"
				   val top = "may return")
    open L
    val return = makeTop
    val mayReturn = isTop
    val whenReturns = addHandler
  end

structure MayRaise =
  struct
    structure L = TwoPointLattice (val bottom = "does not raise"
				   val top = "may raise")
    open L
    val raisee = makeTop
    val mayRaise = isTop
    val whenRaises = addHandler
  end

structure VarInfo =
   struct
      datatype t = T of {used: Used.t}

      fun layout (T {used, ...}) = Used.layout used
	 
      local
	 fun make f (T r) = f r
      in
	 val used = make #used
      end

      fun new () = T {used = Used.new ()}

      fun whenUsed (vi, th) = Used.whenUsed (used vi, th)
      val isUsed = Used.isUsed o used
      val use = Used.use o used
   end

structure ConInfo =
   struct
      datatype t = T of {args: (VarInfo.t * Type.t) vector,
			 coned: Coned.t,
			 deconed: Deconed.t,
			 dummy: Exp.t option ref,
			 tycon: Tycon.t}

      fun layout (T {args, coned, deconed, ...}) =
	 Layout.record [("args", Vector.layout (VarInfo.layout o #1) args),
			("coned", Coned.layout coned),
			("deconed", Deconed.layout deconed)]

      local
	 fun make f (T r) = f r
      in
	 val args = make #args
	 val coned = make #coned
	 val deconed = make #deconed
	 val dummy = make #dummy
	 val tycon = make #tycon
      end

      val con = Coned.con o coned
      val isConed = Coned.isConed o coned
      fun whenConed (c, th) = Coned.whenConed (coned c, th)
      val decon = Deconed.decon o deconed
      val isDeconed = Deconed.isDeconed o deconed
      fun whenDeconed (c, th) = Deconed.whenDeconed (deconed c, th)

      fun new {args: Type.t vector, tycon: Tycon.t}: t =
	 T {args = Vector.map (args, fn t => (VarInfo.new (), t)),
	    coned = Coned.new (),
	    deconed = Deconed.new (),
	    dummy = ref NONE,
	    tycon = tycon}
   end

structure FuncInfo =
   struct
      datatype t = T of {args: (VarInfo.t * Type.t) vector,
			 bugLabel: Label.t option ref,
			 mayRaise: MayRaise.t,
			 mayReturn: MayReturn.t,
			 retLabel: Label.t option ref,
			 returns: (VarInfo.t * Type.t) vector option,
			 sideEffects: SideEffects.t,
			 used: Used.t,
			 wrappers: Block.t list ref}

      fun layout (T {args, mayRaise, mayReturn, returns, sideEffects, used,
		     ...}) =
	 Layout.record
	 [("args",
	   Vector.layout (Layout.tuple2 (VarInfo.layout, Type.layout)) args),
	  ("mayRaise", MayRaise.layout mayRaise),
	  ("mayReturn", MayReturn.layout mayReturn),
	  ("returns", (Option.layout
		       (Vector.layout (Layout.tuple2 (VarInfo.layout,
						      Type.layout)))
		       returns)),
	  ("sideEffects", SideEffects.layout sideEffects),
	  ("used", Used.layout used)]

      local
	 fun make f (T r) = f r
      in
	 val args = make #args
	 val bugLabel = make #bugLabel
	 val mayRaise' = make #mayRaise
	 val mayReturn' = make #mayReturn
	 val retLabel = make #retLabel
	 val returns = make #returns
	 val sideEffects = make #sideEffects
	 val used = make #used
	 val wrappers = make #wrappers
      end

      val raisee = MayRaise.raisee o mayRaise'
      val mayRaise = MayRaise.mayRaise o mayRaise'
      fun whenRaises (i, th) = MayRaise.whenRaises (mayRaise' i, th)

      val return = MayReturn.return o mayReturn'
      fun whenReturns (i, th) = MayReturn.whenReturns (mayReturn' i, th)
      val mayReturn = MayReturn.mayReturn o mayReturn'

      val isUsed = Used.isUsed o used

      val sideEffect = SideEffects.sideEffect o sideEffects
	
      fun new {args, returns} =
	 T {args = args,
	    bugLabel = ref NONE,
	    mayRaise = MayRaise.new (),
	    mayReturn = MayReturn.new (),
	    retLabel = ref NONE,
	    returns = returns,
	    sideEffects = SideEffects.new (),
	    used = Used.new (),
	    wrappers = ref []}
   end

structure LabelInfo =
   struct
      datatype t = T of {args: (VarInfo.t * Type.t) vector,
			 catches: Catches.t,
			 func: FuncInfo.t,
			 used: Used.t,
			 wrappers: (Type.t vector * Label.t) list ref}

      fun layout (T {args, catches, used, ...}) =
	 Layout.record
	 [("args", Vector.layout (VarInfo.layout o #1) args),
	  ("catches", Catches.layout catches),
	  ("used", Used.layout used)]
				  
      fun new {func, args} =
	 T {args = args,
	    catches = Catches.new (),
	    func = func,
	    used = Used.new (),
	    wrappers = ref []}

      local
	 fun make f (T r) = f r
      in
	 val args = make #args
	 val catches = make #catches
	 val func = make #func
	 val used = make #used
	 val wrappers = make #wrappers
      end

      val doesCatch = Catches.doesCatch o catches
      val isUsed = Used.isUsed o used
      fun whenUsed (li, f) = Used.whenUsed (used li, f)
   end

fun remove (program as Program.T {datatypes, globals, functions, main})
  = let
      val {get = varInfo: Var.t -> VarInfo.t, ...}
	= Property.get (Var.plist, Property.initFun (fn _ => VarInfo.new ()))
      local
	fun make s = (s o varInfo, ! o s o varInfo)
      in
	val usedVar = VarInfo.used o varInfo
	val useVar = Used.use o usedVar
	val isUsedVar = Used.isUsed o usedVar
	fun flowVarInfoTyVarInfoTy ((vi, _), (vi', _)) =
	   Used.<= (VarInfo.used vi, VarInfo.used vi')
	fun flowVarInfoTysVarInfoTys (xs, ys) =
	   Vector.foreach2 (xs, ys, flowVarInfoTyVarInfoTy)
	fun flowVarInfoTyVar ((vi, _), x) = Used.<= (VarInfo.used vi, usedVar x)
	fun flowVarInfoTysVars (xs, ys) =
	   Vector.foreach2 (xs, ys, flowVarInfoTyVar)
	fun flowVarTyVarInfoTy ((x, _), (vi, _)) =
	   Used.<= (usedVar x, VarInfo.used vi)
	fun flowVarTysVarInfoTys (xs, ys) =
	   Vector.foreach2 (xs, ys, flowVarTyVarInfoTy)
	fun flowVarVar (x, y) = Used.<= (usedVar x, usedVar y)
	fun flowVarTyVar ((x, _), y) = flowVarVar (x, y)
	fun flowVarVarTy (x, (y, _)) = flowVarVar (x, y)
	fun flowVarTyVarTy ((x, _), (y, _)) = flowVarVar (x, y)
	fun flowVarsVars (xs, ys) = Vector.foreach2(xs, ys, flowVarVar)
	fun flowVarTysVars (xs, ys) = Vector.foreach2(xs, ys, flowVarTyVar)
	fun flowVarsVarTys (xs, ys) = Vector.foreach2(xs, ys, flowVarVarTy)
	fun flowVarTysVarTys (xs, ys) = Vector.foreach2(xs, ys, flowVarTyVarTy)
      end
      val {get = tyInfo: Type.t -> {deconed: bool ref}, destroy, ...} =
	 Property.destGet (Type.plist,
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
      val {get = conInfo: Con.t -> ConInfo.t, set = setConInfo, ...} =
	 Property.getSetOnce
	 (Con.plist, Property.initRaise ("RemoveUnused.conInfo", Con.layout))
      val conInfo =
	 Trace.trace ("RemoveUnused.conInfo", Con.layout, ConInfo.layout) conInfo
      fun newConInfo (con, args, tycon) =
	 setConInfo (con, ConInfo.new {args = args, tycon = tycon})
      val {get = funcInfo: Func.t -> FuncInfo.t, set = setFuncInfo, ...}
	 = Property.getSetOnce
	  (Func.plist,
	   Property.initRaise ("RemoveUnused.funcInfo", Func.layout))
      val {get = labelInfo: Label.t -> LabelInfo.t, set = setLabelInfo, ...}
	= Property.getSetOnce
	  (Label.plist,
	   Property.initRaise ("RemoveUnused.labelInfo", Label.layout))
      val visitLabelInfo = Used.use o LabelInfo.used
      val visitLabel = visitLabelInfo o labelInfo
      fun visitFuncInfo fi = Used.use (FuncInfo.used fi)
      val visitFunc = visitFuncInfo o funcInfo
      fun visitVar (x: Var.t) = useVar x
      fun visitVars (xs: Var.t Vector.t) = Vector.foreach (xs, visitVar)
      fun visitExp (e: Exp.t) =
	 case e of
	    ConApp {con, args} =>
	       let
		  val c = conInfo con
		  val _ = ConInfo.con c
		  val _ = flowVarInfoTysVars (ConInfo.args c, args)
	       in
		  ()
	       end
	  | PrimApp {prim, targs, args} =>
	       let
		  val _ = visitVars args
		  datatype z = datatype Type.dest
		  fun decon t
		    = let
			val {deconed, ...} = tyInfo t
		      in
			if !deconed
			  then ()
			  else (deconed := true;
				case Type.dest t of
				   Datatype t =>
				      Vector.foreach
				      (consTycon t, fn {con, ...} =>
				       let
					  val c = conInfo con
					  val _ = ConInfo.decon c
					  val _ =
					     Vector.foreach
					     (ConInfo.args c, fn (x, t) =>
					      (VarInfo.use x; decon t))
				       in
					  ()
				       end)
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
      fun maybeVisitVarExp (var, exp) =
	 Option.app (var, fn var =>
		     VarInfo.whenUsed (varInfo var, fn () => visitExp exp))
      fun visitStatement (s: Statement.t as Statement.T {var, ty, exp},
			  f: FuncInfo.t) =
	 if Exp.maySideEffect exp
	    then (FuncInfo.sideEffect f
		  ; visitExp exp)
	 else maybeVisitVarExp (var, exp)
      fun visitTransfer (t: Transfer.t, f: FuncInfo.t) =
	 case t
	    of Bug => ()
	     | Call {func, args, return}
	     => let
		   val f' = funcInfo func
		   fun mayRaise () =
		      MayRaise.<= (FuncInfo.mayRaise' f', FuncInfo.mayRaise' f)
		in
		  flowVarInfoTysVars (FuncInfo.args f', args);
		  SideEffects.<= (FuncInfo.sideEffects f',
				  FuncInfo.sideEffects f);
		  case return of
		     Return.Dead => ()
		   | Return.HandleOnly => mayRaise ()
		   | Return.NonTail {cont, handler} =>
			let
			   val contInfo = labelInfo cont
			   val _ = 
			      Option.app
			      (FuncInfo.returns f', fn xts =>
			       flowVarInfoTysVarInfoTys
			       (LabelInfo.args contInfo, xts))
			   val _ = 
			      FuncInfo.whenReturns
			      (f', fn () => visitLabelInfo contInfo)
			in
			   case handler of
			      Handler.Handle handler =>
				 FuncInfo.whenRaises
				 (f', fn () =>
				  let
				     val handlerInfo = labelInfo handler
				  in
				     Catches.catch (LabelInfo.catches handlerInfo)
				     ; visitLabelInfo handlerInfo
				  end)
			    | _ => mayRaise ()
			end
		   | Return.Tail =>
			((case (FuncInfo.returns f, FuncInfo.returns f') of
			     (SOME xts, SOME xts') =>
				flowVarInfoTysVarInfoTys (xts, xts')
			   | _ => ())
			 ; MayReturn.<= (FuncInfo.mayReturn' f',
					 FuncInfo.mayReturn' f)
			 ; mayRaise ());
		  visitFuncInfo f'
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
				 val _ =
				    Vector.foreach
				    (cases, fn (con, l) =>
				     let
					val c = conInfo con
					val _ = ConInfo.decon c
					val li = labelInfo l
					val _ =
					   flowVarInfoTysVarInfoTys
					   (LabelInfo.args li, ConInfo.args c)
					val _ =
					   ConInfo.whenConed
					   (c, fn () => visitLabelInfo li)
				     in
					()
				     end)
				 val cons =
				    consTycon
				    (ConInfo.tycon
				     (conInfo (#1 (Vector.sub (cases, 0)))))
			       in
				 case default
				   of NONE => ()
				    | SOME l =>
					 let
					    val li = labelInfo l
					 in
					    Vector.foreach
					    (cons, fn {con, ...} =>
					     if Vector.exists
						(cases, fn (c, _) =>
						 Con.equals(c, con))
						then ()
					     else
						ConInfo.whenConed
						(conInfo con, fn () =>
						 visitLabelInfo li))
					 end
			       end
		end
	     | Goto {dst, args} =>
		  let
		     val dst = labelInfo dst
		     val _ = flowVarInfoTysVars (LabelInfo.args dst, args)
		     val _ = visitLabelInfo dst
		  in
		     ()
		  end
	     | Prim {args, failure, success, ...} 
	     => (FuncInfo.sideEffect f;
		 visitVars args;
		 visitLabel failure;
		 visitLabel success)
	     | Raise xs =>
		  (FuncInfo.raisee f
		   ; visitVars xs)
	     | Return xs =>
		  (FuncInfo.return f
		   ; flowVarInfoTysVars (valOf (FuncInfo.returns f), xs))
      val visitTransfer
	= Trace.trace ("RemoveUnused.visitTransfer",
		       Layout.tuple2 (Transfer.layout, FuncInfo.layout),
		       Unit.layout)
	              visitTransfer
      fun visitBlock (b: Block.t as Block.T {label, statements, transfer, ...},
		      f: FuncInfo.t) =
	 (Vector.foreach (statements, fn s => visitStatement (s, f))
	  ; visitTransfer (transfer, f))
      (* Visit all reachable expressions. *)
      val _ = Vector.foreach
	      (datatypes,
	       fn Datatype.T {tycon, cons}
	        => (setTyconInfo (tycon, {cons = cons, numCons = ref ~1});
		    Vector.foreach 
		    (cons, 
		     fn {con, args} => newConInfo (con, args, tycon))))
      val _ = let
		 fun doit c =
		    let
		       val c = conInfo c
		       val _ = ConInfo.con c
		       val _ = ConInfo.decon c
		    in
		       ()
		    end
	      in
		doit Con.truee ; doit Con.falsee 
	      end
      val _ = Vector.foreach (globals, fn Statement.T {var, exp, ...} =>
			      maybeVisitVarExp (var, exp))
      val _ = List.foreach
	      (functions, fn function =>
	       let
		  val {name, args, returns, start, blocks, ...} =
		     Function.dest function
		  val f as FuncInfo.T {used, ...} =
		     FuncInfo.new
		     {args = Vector.map (args, fn (x, t) => (varInfo x, t)),
		      returns = (Option.map
				 (returns, fn ts =>
				  Vector.map
				  (ts, fn t => (VarInfo.new (), t))))}
		  val _ = setFuncInfo (name, f)
		  val _ = Used.whenUsed (used, fn () => visitLabel start)
		  val _ =
		     Vector.foreach
		     (blocks, fn block as Block.T {label, args, ...} =>
		      let
			 val li as LabelInfo.T {used, ...} =
			    LabelInfo.new {args = Vector.map (args, fn (x, t) =>
							      (varInfo x, t)),
					   func = f}
			 val _ = setLabelInfo (label, li)
			 val _ = Used.whenUsed (used, fn () =>
						visitBlock (block, f))
		      in
			 ()
		      end)
	       in
		  ()
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
				       (fn {con, ...} =>
					seq [Con.layout con,
					     ConInfo.layout (conInfo con)])
				       cons])
			      end)
		 val r: Exp.t option ref = ref NONE
		 val cons 
		   = Vector.keepAllMap
		     (cons,
		      fn c as {con, args}
		       => let
			     val c = conInfo con
			  in
			    case (ConInfo.isConed c, ConInfo.isDeconed c)
			      of (false, _) => NONE
			       | (true, true) =>
				    SOME {con = con,
					  args = Vector.keepAllMap
					  (ConInfo.args c, fn (x, t) =>
					   if VarInfo.isUsed x
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
				    val _ = ConInfo.dummy c := SOME e
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
      fun getWrapperLabel (l: Label.t,
			   args: (VarInfo.t * Type.t) vector) =
	 let
	    val li = labelInfo l
	 in
	    if Vector.forall2 (args, LabelInfo.args li, fn ((x, _), (y, _)) =>
			       VarInfo.isUsed x = VarInfo.isUsed y)
	       then l
	    else
	       let
		  val tys =
		     Vector.keepAllMap (args, fn (x, ty) =>
					if VarInfo.isUsed x
					   then SOME ty
					else NONE)
	       in 
		  case List.peek (!(LabelInfo.wrappers li), fn (args', l') =>
				  Vector.length args' = Vector.length tys
				  andalso
				  Vector.forall2 (args', tys, fn (ty', ty) =>
						  Type.equals (ty', ty))) of
		     SOME (_, l') => l'
		   | NONE =>
			let
			   val l' = Label.newNoname ()
			   val (args', args'') =
			      Vector.unzip
			      (Vector.map2
			       (args, LabelInfo.args li, fn ((x, ty), (y, _)) =>
				let
				   val z = Var.newNoname ()
				in
				   (if VarInfo.isUsed x then SOME (z, ty) else NONE,
				    if VarInfo.isUsed y then SOME z else NONE)
				end))
			   val args' = Vector.keepAllMap (args', fn x => x)
			   val (_, tys') = Vector.unzip args'
			   val args'' = Vector.keepAllMap (args'', fn x => x)
			   val block = Block.T {label = l',
						args =  args',
						statements = Vector.new0 (),
						transfer = Goto {dst = l,
								 args = args''}}
			   val _ = List.push (LabelInfo.wrappers li, (tys', l'))
			   val _ =
			      List.push (FuncInfo.wrappers (LabelInfo.func li),
					 block)
			in
			   l'
			end
	       end
	 end
      val getContWrapperLabel = getWrapperLabel
      val getConWrapperLabel = getWrapperLabel
      fun getOriginalWrapperLabel l =
	 getWrapperLabel 
	 (l, Vector.map (LabelInfo.args (labelInfo l), fn (_, t) =>
			 let 
			    val x = VarInfo.new ()
			    val _ = VarInfo.use x
			 in
			    (x, t)
			 end))
      val getHandlerWrapperLabel = getOriginalWrapperLabel
      val getPrimFailureWrapperLabel = getOriginalWrapperLabel
      val getPrimSuccessWrapperLabel = getOriginalWrapperLabel
      fun getBugFunc (f: FuncInfo.t): Label.t =
	 let
	    val r = FuncInfo.bugLabel f
	 in
	    case !r of
	       SOME l => l
	     | NONE =>
		  let
		     val l = Label.newNoname ()
		     val block = Block.T {label = l,
					  args = Vector.new0 (),
					  statements = Vector.new0 (),
					  transfer = Bug}
		     val _ = r := SOME l
		     val _ = List.push (FuncInfo.wrappers f, block)
		  in
		     l
		  end
	 end
      fun getRetFunc (f: FuncInfo.t): Label.t =
	 let
	    val r = FuncInfo.retLabel f
	 in
	    case !r of
	       SOME l => l
	     | NONE =>
		  let
		     val l = Label.newNoname ()
		     val returns = valOf (FuncInfo.returns f)
		     val args =
			Vector.keepAllMap
			(returns, fn (vi, ty) =>
			 if VarInfo.isUsed vi
			    then SOME (Var.newNoname (), ty)
			 else NONE)
		     val xs = Vector.map (args, #1)
		     val block = Block.T {label = l,
					  args = args,
					  statements = Vector.new0 (),
					  transfer = Return xs}
		     val _ = r := SOME l
		     val _ = List.push (FuncInfo.wrappers f, block)
		     val _ = setLabelInfo (l, LabelInfo.new {func = f,
							     args = returns})
		  in
		     l
		  end
	 end
      fun getRetContFunc (f, args) = getWrapperLabel (getRetFunc f, args)
      fun simplifyExp (e: Exp.t): Exp.t =
	 case e of
	    ConApp {con, args} =>
	       let
		  val c = conInfo con
	       in
		  if ConInfo.isDeconed c
		     then ConApp {con = con,
				  args = (Vector.keepAllMap2
					  (args, ConInfo.args c,
					   fn (x, (y, t)) =>
					   if VarInfo.isUsed y
					      then SOME x
					   else NONE))}
		  else valOf (! (ConInfo.dummy c))
	       end
	  | _ => e
      val simplifyExp =
	 Trace.trace ("RemoveUnused.simplifyExp", Exp.layout, Exp.layout)
	 simplifyExp
      fun simplifyStatement (s as Statement.T {var, ty, exp},
			     f: FuncInfo.t): Statement.t option =
	 let
	    fun maybe (l, th) =
	       if LabelInfo.doesCatch (labelInfo l)
		  then SOME (Statement.T {var = var, ty = ty, exp = th ()})
	       else
		  NONE
	 in     
	    case exp 
	      of HandlerPop l 
	       => maybe (l, fn () => HandlerPop (getHandlerWrapperLabel l))
	       | HandlerPush l 
	       => maybe (l, fn () => HandlerPush (getHandlerWrapperLabel l))
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
	 end
      fun simplifyStatements (ss: Statement.t Vector.t,
			      f: FuncInfo.t) : Statement.t Vector.t =
	 Vector.keepAllMap (ss, fn s => simplifyStatement (s, f))
      fun simplifyTransfer (t: Transfer.t, f: FuncInfo.t): Transfer.t =
	 case t of
	    Call {func, args, return} =>
	       let
		  val f' = funcInfo func
		  val return =
		     case return of
			Return.Dead => Return.Dead
		      | Return.HandleOnly =>
			   if FuncInfo.mayRaise f'
			      then Return.HandleOnly
			   else Return.Dead
		      | Return.NonTail {cont, handler} =>
			   if FuncInfo.mayReturn f'
			      then
				 Return.NonTail
				 {cont = (getContWrapperLabel
					  (cont, valOf (FuncInfo.returns f'))),
				  handler = if FuncInfo.mayRaise f'
					       then (Handler.map
						     (handler,
						      getHandlerWrapperLabel))
					    else Handler.None}
			   else
			      if FuncInfo.mayRaise f'
				 then
				    (case handler of
					Handler.CallerHandler =>
					   Return.HandleOnly
				      | Handler.Handle l =>
					   Return.NonTail
					   {cont = getBugFunc f,
					    handler =
					    Handler.Handle
					    (getHandlerWrapperLabel l)}
				      | Handler.None => Error.bug "fail to None")
			      else Return.Dead
                      | Return.Tail =>
			   if (case (FuncInfo.returns f, FuncInfo.returns f') of
				  (SOME xts, SOME yts) =>
				     Vector.forall2
				     (xts, yts, fn ((x, _), (y, _)) =>
				      VarInfo.isUsed x = VarInfo.isUsed y)
				| (SOME _, NONE) => true
				| (NONE, SOME _) => Error.bug "return mismatch"
				| (NONE, NONE) => true)
			      then Return.Tail
			   else
			      if FuncInfo.mayReturn f'
				 then
				    Return.NonTail
				    {cont = (getRetContFunc
					     (f, valOf (FuncInfo.returns f'))),
				     handler = if FuncInfo.mayRaise f'
						  then Handler.CallerHandler
					       else Handler.None}
			      else
				 if FuncInfo.mayRaise f'
				    then Return.HandleOnly
				 else Return.Dead
		  val args =
		     Vector.keepAllMap2
		     (args, FuncInfo.args f', fn (x, (y, t)) =>
		      if VarInfo.isUsed y
			 then SOME x
		      else NONE)
	       in
		  Call {func = func,
			args = args,
			return = return}
	       end
	| Case {test, cases = Cases.Con cases, default} =>
	     let
		val cases =
		   Vector.keepAllMap
		   (cases, fn (con, l) =>
		    let
		       val c = conInfo con
		    in
		       if ConInfo.isConed c
			  then SOME (con, getConWrapperLabel (l, ConInfo.args c))
		       else NONE
		    end)
		fun keep default = Case {test = test,
					 cases = Cases.Con cases,
					 default = default}
		fun none () = keep NONE
	     in
		case default
		   of NONE => none ()
		 | SOME l => if Vector.length cases = 0
				then if LabelInfo.isUsed (labelInfo l)
					then Goto {dst = l, args = Vector.new0 ()}
				     else Bug
			     else let
				     val numCons =
					numConsTycon' 
					(ConInfo.tycon
					 (conInfo
					  (#1 (Vector.sub (cases, 0)))))
				  in 
				     if Vector.length cases = numCons
					then none ()
				     else keep (SOME l)
				  end
	     end
	| Goto {dst, args} =>
	     Goto {dst = dst, 
		   args = (Vector.keepAllMap2
			   (args, LabelInfo.args (labelInfo dst),
			    fn (x, (y, t)) => if VarInfo.isUsed y
						 then SOME x
					      else NONE))}
	| Prim {prim, args, failure, success} =>
	     Prim {prim = prim,
		   args = args,
		   failure = getPrimFailureWrapperLabel failure,
		   success = getPrimSuccessWrapperLabel success}
	| Return xs =>
	     Return (Vector.keepAllMap2
		     (xs, valOf (FuncInfo.returns f),
		      fn (x, (y, t)) => if VarInfo.isUsed y
					   then SOME x
					else NONE))
	| _ => t
      val simplifyTransfer
	= Trace.trace ("RemoveUnused.simplifyTransfer",
		       Layout.tuple2 (Transfer.layout, FuncInfo.layout),
		       Transfer.layout)
	simplifyTransfer
      fun simplifyBlock (b: Block.t
			 as Block.T {label, args, 
				     statements, transfer}): Block.t option
	= let
	    val li = labelInfo label
	    val _ = Control.diagnostics
	            (fn display
		      => let open Layout
		         in display (seq [Label.layout label,
					  str ": ",
					  LabelInfo.layout li])
			 end)
	  in 
	    if LabelInfo.isUsed li
	      then let
		     val args =
			Vector.keepAllMap2
			(LabelInfo.args li, args, fn ((vi, _), (x, ty)) =>
			 if VarInfo.isUsed vi
			    then SOME (x, ty)
			 else NONE)
		     val statements =
			simplifyStatements (statements, LabelInfo.func li)
		     val transfer =
			simplifyTransfer (transfer, LabelInfo.func li)
		   in
		     SOME (Block.T {label = label,
				    args = args,
				    statements = statements,
				    transfer = transfer})
		   end
	      else NONE
	  end
      fun simplifyBlocks (bs: Block.t Vector.t): Block.t Vector.t =
	 Vector.keepAllMap (bs, simplifyBlock)
      val globals = simplifyStatements (globals, funcInfo main)
      val shrink = shrinkFunction globals
      fun simplifyFunction (f: Function.t): Function.t option =
	 let
	    val {args, blocks, name, start, ...} = Function.dest f
	    val fi = funcInfo name
	    val _ =
	       Control.diagnostics
	       (fn display =>
		let open Layout
		in display
		   (seq [Func.layout name, str ": ", FuncInfo.layout fi])
		end)
	  in
	    if not (FuncInfo.isUsed fi)
	       then NONE
	    else
	       let
		  val args =
		     Vector.keepAllMap2
		     (FuncInfo.args fi, args, fn ((vi, _), (x, t)) =>
		      if VarInfo.isUsed vi
			 then SOME (x, t)
		      else NONE)
		  val blocks = simplifyBlocks blocks
		  val wrappers = Vector.fromList (! (FuncInfo.wrappers fi))
		  val blocks = Vector.concat [wrappers, blocks]
		  val returns =
		     case FuncInfo.returns fi of
			NONE => NONE
		      | SOME xts => 
			   if FuncInfo.mayReturn fi
			      then 
				 SOME (Vector.keepAllMap
				       (xts, fn (x, t) => if VarInfo.isUsed x
							     then SOME t
							  else NONE))
			   else NONE
	       in
		  SOME (shrink (Function.new {args = args,
					      blocks = blocks,
					      mayRaise = FuncInfo.mayRaise fi,
					      name = name,
					      returns = returns,
					      start = start}))
	       end
	  end
      fun simplifyFunctions (fs: Function.t List.t): Function.t List.t
	= List.keepAllMap (fs, simplifyFunction)
      val functions = simplifyFunctions functions
      val program = Program.T {datatypes = datatypes,
			       globals = globals,
			       functions = functions,
			       main = main}
      val _ = destroy ()
      val _ = Program.clearTop program
    in
      program
    end
 
end
