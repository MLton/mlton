(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Shrink (S: SHRINK_STRUCTS): SHRINK = 
struct

open S
   
datatype z = datatype Exp.t
datatype z = datatype Transfer.t

structure VarInfo =
   struct
      datatype t = T of {isUsed: bool ref,
			 numOccurrences: int ref,
			 value: value option ref,
			 var: Var.t}
      and value =
	 Con of {con: Con.t,
		 args: t vector}
	| Const of Const.t
	| Tuple of t vector

      fun equals (T {var = x, ...}, T {var = y, ...}) = Var.equals (x, y)
	 
      fun layout (T {isUsed, numOccurrences, value, var}) =
	 let open Layout
	 in record [("isUsed", Bool.layout (!isUsed)),
		    ("numOccurrences", Int.layout (!numOccurrences)),
		    ("value", Option.layout layoutValue (!value)),
		    ("var", Var.layout var)]
	 end
      and layoutValue v =
	 let open Layout
	 in case v of
	    Con {con, args} => seq [Con.layout con,
				    Vector.layout layout args]
	  | Const c => Const.layout c
	  | Tuple vis => Vector.layout layout vis
	 end

      fun new (x: Var.t) = T {isUsed = ref false,
			      numOccurrences = ref 0,
			      value = ref NONE,
			      var = x}

      fun setValue (T {value, ...}, v) =
	 (Assert.assert ("VarInfo.setValue", fn () => Option.isNone (!value))
	  ; value := SOME v)

      fun var (T {var, ...}): Var.t = var
      fun numOccurrences (T {numOccurrences = r, ...}) = r
      fun value (T {value, ...}): value option = !value
   end

structure Value =
   struct
      datatype t = datatype VarInfo.value

      val layout = VarInfo.layoutValue

      fun fromBool (b: bool): t =
	 Con {con = if b then Con.truee else Con.falsee,
	      args = Vector.new0 ()}

      fun toExp (v: t): Exp.t =
	 case v of
	    Con {con, args} =>
	       Exp.ConApp {con = con,
			   args = Vector.map (args, VarInfo.var)}
	  | Const c => Exp.Const c
	  | Tuple xs => Exp.Tuple (Vector.map (xs, VarInfo.var))
   end

structure Position =
   struct
      datatype t =
	 Formal of int
       | Free of Var.t

      fun layout (p: t) =
	 case p of
	    Formal i => Int.layout i
	  | Free x => Var.layout x

      val equals =
	 fn (Formal i, Formal i') => i = i'
	  | (Free x, Free x') => Var.equals (x, x')
	  | _ => false

      val usesFormal = fn Formal _ => true | _ => false
   end

structure Positions =
   struct
      local
	 structure A = MonoVector (Position)
      in
	 open A
      end

      fun usesFormal (ps: t): bool = exists (ps, Position.usesFormal)
   end

structure LabelMeaning =
   struct
      datatype t =
	 Block of Block.t
       | Bogus
       | Bug
       | Case of {cases: Label.t Cases.t,
		  default: Label.t option}
       | Goto of {dst: Label.t,
		  args: Positions.t}
       | Raise of Positions.t
       | Return of Positions.t

      fun layout (m: t) =
	 let
	    open Layout
	 in
	    case m of
	       Block b => seq [str "Block ", Label.layout (Block.label b)]
	     | Bogus => str "Bogus"
	     | Bug => str "Bug"
	     | Case {cases, default} => str "Case"
	     | Goto {dst, args} =>
		  seq [str "Goto ",
		       tuple [Label.layout dst, Positions.layout args]]
	     | Raise ps => seq [str "Raise ", Positions.layout ps]
	     | Return ps => seq [str "Return ", Positions.layout ps]
	 end

      val bogus = Bogus
   end

val traceApply =
   Trace.trace ("Prim.apply",
		fn (p, args, _: VarInfo.t * VarInfo.t -> bool) =>
		let open Layout
		in seq [Prim.layout p,
			List.layout (Prim.ApplyArg.layout
				     (Var.layout o VarInfo.var)) args]
		end,
		Prim.ApplyResult.layout (Var.layout o VarInfo.var))

val bug = ([], Bug)

fun shrinkFunction (globals: Statement.t vector) =
   let
      fun use (VarInfo.T {isUsed, var, ...}): Var.t =
	 (isUsed := true
	  ; var)
      fun uses (vis: VarInfo.t vector): Var.t vector = Vector.map (vis, use)
      (* varInfo can't be getSetOnce because of setReplacement. *)
      val {get = varInfo: Var.t -> VarInfo.t, set = setVarInfo, ...} =
	 Property.getSet (Var.plist, Property.initFun VarInfo.new)
      fun varInfos xs = Vector.map (xs, varInfo)
      fun simplifyVar (x: Var.t) = use (varInfo x)
      val simplifyVar =
	 Trace.trace ("simplifyVar", Var.layout, Var.layout) simplifyVar
      fun simplifyVars xs = Vector.map (xs, simplifyVar)
      fun incVarInfo (x: VarInfo.t): unit =
	 Int.inc (VarInfo.numOccurrences x)
      fun incVar (x: Var.t): unit = incVarInfo (varInfo x)
      fun numVarOccurrences (x: Var.t): int =
	 ! (VarInfo.numOccurrences (varInfo x))
      val _ =
	 Vector.foreach
	 (globals, fn Statement.T {var, exp, ty} =>
	  let
	     fun construct v =
		Option.app (var, fn x => VarInfo.setValue (varInfo x, v))
	  in case exp of
	     ConApp {con, args} =>
		construct (Value.Con {con = con,
				      args = Vector.map (args, varInfo)})
	   | Const c => construct (Value.Const c)
	   | Tuple xs => construct (Value.Tuple (Vector.map (xs, varInfo)))
	   | Var y => Option.app (var, fn x => setVarInfo (x, varInfo y))
	   | _ => ()
	  end)
   in
      fn (f: Function.t, mayDelete: bool) =>
      let
	 val _ = Function.clear f
	 val {args, blocks, name, raises, returns, start, ...} =
	    Function.dest f
	 (* Index the labels by their defining block in blocks. *)
	 val {get = labelIndex, set = setLabelIndex, ...} =
	    Property.getSetOnce (Label.plist,
				 Property.initRaise ("index", Label.layout))
	 val _ = Vector.foreachi (blocks, fn (i, Block.T {label, ...}) =>
				  setLabelIndex (label, i))
	 val numBlocks = Vector.length blocks
	 (* Do a DFS to compute occurrence counts and set label meanings *)
	 datatype state = Unvisited | Visited | Visiting
	 val states = Array.array (numBlocks, Unvisited)
	 val inDegree = Array.array (numBlocks, 0)
	 val isHeader = Array.array (numBlocks, false)
	 val isHandler = Array.array (numBlocks, false)

	 fun layoutLabel (l: Label.t): Layout.t =
	    let
	       val i = labelIndex l
	    in
	       Layout.record [("label", Label.layout l),
			      ("inDegree", Int.layout (Array.sub (inDegree, i)))]
	    end
	 val traceGoto =
	    Trace.trace2
	    ("Shrink.goto", layoutLabel, Vector.layout VarInfo.layout,
	     Layout.tuple2 (List.layout Statement.layout,
			    Transfer.layout))
	 val traceForceBlock =
	    Trace.trace ("Shrink.forceBlock", layoutLabel, Unit.layout)
	 fun visit (l: Label.t): unit =
	    let
	       val i = labelIndex l
	       val _ = Array.update (inDegree, i, 1 + Array.sub (inDegree, i))
	    in
	       case Array.sub (states, i) of
		  Visited => ()
		| Visiting => Array.update (isHeader, i, true)
		| Unvisited => 
		     let
			val _ = Array.update (states, i, Visiting)
			val block as Block.T {args, statements, transfer, ...} =
			   Vector.sub (blocks, i)
			val _ =
			   Vector.foreach
			   (statements, fn s =>
			    Exp.foreachVar (Statement.exp s, incVar))
			val _ =
			   Transfer.foreachLabelVar (transfer, visit, incVar)
			val _ =
			   case transfer of
			      Call {return, ...} =>
				 (case return of
				     Return.NonTail {handler, ...} =>
					Handler.foreachLabel
					(handler, fn l =>
					 Array.update
					 (isHandler, labelIndex l, true))
				   | _ => ())
			    | _ => ()
			val _ = Array.update (states, i, Visited)
		     in
			()
		     end
	    end
	 val _ = visit start
	 val meanings =
	    Array.tabulate
	    (numBlocks, fn i =>
	     let
		val block as Block.T {args, statements, transfer, ...} =
		   Vector.sub (blocks, i)
		fun normal () = LabelMeaning.Block block
		fun extract (actuals: Var.t vector): Positions.t =
		   let
		      val {get: Var.t -> Position.t, set, destroy} =
			 Property.destGetSetOnce
			 (Var.plist, Property.initFun Position.Free)
		      val _ = Vector.foreachi (args, fn (i, (x, _)) =>
					       set (x, Position.Formal i))
		      val ps = Vector.map (actuals, get)
		      val _ = destroy ()
		   in ps
		   end
		fun sameAsArgs args' =
		   Vector.equals (args, args', fn ((x, _), x') =>
				  Var.equals (x, x'))
	     in
		if 0 = Vector.length statements
		   then
		      case transfer of
			 Bug =>
			    if (case returns of
				   NONE => true
				 | SOME ts =>
				      Vector.equals (ts, args, fn (t, (_, t')) =>
						     Type.equals (t, t')))
			       then LabelMeaning.Bug
			    else normal ()
		       | Case {test, cases, default} =>
			    if 1 = Vector.length args
			       then
				  let
				     val (x, _) = Vector.sub (args, 0)
				  in if (Var.equals (x, test)
					 andalso 1 = numVarOccurrences x)
					then
					   LabelMeaning.Case
					   {cases = cases,
					    default = default}
				     else normal ()
				  end
			    else normal ()
		       | Goto {dst, args = actuals} =>
			    let
			       val ps = extract actuals
			       val n =
				  Vector.fold (args, 0, fn ((x, _), n) =>
					       n + numVarOccurrences x)
			       val n' =
				  Vector.fold (ps, 0, fn (p, n) =>
					       case p of
						  Position.Formal _ => n + 1
						| _ => n)
			    in
			       if n = n'
				  then LabelMeaning.Goto {dst = dst,
							  args = ps}
			       else normal ()
			    end
		       | Raise xs => LabelMeaning.Raise (extract xs)
		       | Return xs => LabelMeaning.Return (extract xs)
		       | _ => normal ()
		else normal ()
	     end)
	 val isBlock = Array.array (numBlocks, false)
	 (* Functions for maintaining inDegree. *)
	 fun addLabel l =
	    let
	       val i = labelIndex l
	    in
	       Array.update (inDegree, i, 1 + Array.sub (inDegree, i))
	    end
	 val traceDeleteLabel =
	    Trace.trace ("Shrink.deleteLabel", layoutLabel, Unit.layout)
	 fun indexToLabel i = Block.label (Vector.sub (blocks, i))
	 fun deleteLabel arg =
	    traceDeleteLabel
	    (fn l => deleteLabelIndex (labelIndex l)) arg
	 and deleteLabelIndex (i: int): unit =
	    let
	       val _ =
		  Control.diagnostic
		  (fn () =>
		   let
		      open Layout
		   in
		      seq [str "deleteLabelIndex ",
			   Label.layout (indexToLabel i)]
		   end)
	       val n = Array.sub (inDegree, i) - 1
	       val _ =
		  if n < 0
		     then Error.bug "deleteLabelIndex"
		  else ()
	       val _ = Array.update (inDegree, i, n)
	    in
	       if n = 0 andalso not (Array.sub (isBlock, i))
		  then (Transfer.foreachLabel
			(Block.transfer (Vector.sub (blocks, i)),
			 deleteLabel))
	       else ()
	    end
	 fun primApp (prim: Prim.t, args: VarInfo.t vector)
	    : VarInfo.t Prim.ApplyResult.t =
	    case Prim.name prim of
	       Prim.Name.FFI _ => Prim.ApplyResult.Unknown
	     | _ =>
		  let
		     val args' =
			Vector.map
			(args, fn vi =>
			 case vi of
			    VarInfo.T {value = ref (SOME v), ...} =>
			       (case v of
				   Value.Con {con, args} =>
				      if Vector.isEmpty args
					 then
					    Prim.ApplyArg.Con
					    {con = con,
					     hasArg = not (Vector.isEmpty args)}
				      else Prim.ApplyArg.Var vi
				 | Value.Const c =>
				      Prim.ApplyArg.Const (Const.node c)
				 | _ => Prim.ApplyArg.Var vi)
			  | _ => Prim.ApplyArg.Var vi)
		  in
		     Prim.apply (prim, Vector.toList args', VarInfo.equals)
		     handle e =>
			Error.bug (concat ["Prim.apply raised ",
					   Layout.toString (Exn.layout e)])
		  end
	 (* Another DFS, this time accumulating the new blocks. *)
	 val newBlocks = ref []
	 fun forceBlock arg: unit =
	    traceForceBlock
	    (fn (l: Label.t) =>
	    let
	       val i = labelIndex l
	    in
	       if Array.sub (isBlock, i)
		  then ()
	       else
		  let
		     val _ = Array.update (isBlock, i, true)
		     val block as Block.T {args, ...} = Vector.sub (blocks, i)
		     val (statements, transfer) = simplifyBlock block
		     val _ =
			List.push
			(newBlocks,
			 Block.T {label = l,
				  args = args,
				  statements = Vector.fromList statements,
				  transfer = transfer})
		  in
		     ()
		  end
	    end) arg
	 and simplifyBlock (Block.T {label, statements, transfer, ...})
	    : Statement.t list * Transfer.t =
	    let
	       val _ =
		  Control.diagnostic
		  (fn () =>
		   let
		      open Layout
		   in
		      seq [str "simplifyBlock ", Label.layout label]
		   end)
	       val fs = Vector.map (statements, evalStatement)
	       val (ss, transfer) = simplifyTransfer transfer
	       val statements = Vector.foldr (fs, ss, fn (f, ss) => f ss)
	    in
	       (statements, transfer)
	    end
	 and simplifyTransfer (t: Transfer.t): Statement.t list * Transfer.t =
	    (
		  Control.diagnostic
		  (fn () =>
		   let
		      open Layout
		   in
		      seq [str "simplifyTransfer ", Transfer.layout t]
		   end)
		  ;
	    case t of
	       Bug => ([], Bug)
	     | Call {func, args, return} =>
		  let
		     val return =
			case return of
			   Return.NonTail {cont, handler} =>
			      let
				 val i = labelIndex cont
				 val m = Array.sub (meanings, i)
				 val isTail =
				    (case handler of
					Handler.CallerHandler => true
				      | Handler.Handle _ => false
				      | Handler.None => true)
                                    andalso 
				    (case m of
					LabelMeaning.Bug => true
				      | LabelMeaning.Return ps =>
					   Vector.length ps =
					   (Vector.length
					    (Block.args (Vector.sub (blocks, i))))
					   andalso
					   Vector.foralli
					   (ps,
					    fn (i, Position.Formal i') => i = i'
					     | _ => false)
				      | _ => false)
			      in
				 if isTail
				    then (deleteLabelIndex i; Return.Tail)
				 else
				    let
				       val _ = forceBlock cont
				       val _ = Handler.foreachLabel (handler,
								     forceBlock)
				    in
				       Return.NonTail {cont = cont,
						       handler = handler}
				    end
			      end
			 | _ => return
		  in 
		     ([], Call {func = func,
				args = simplifyVars args,
				return = return})
		  end
	      | Case {test, cases, default} =>
		   let
		      val test = varInfo test
		      fun cantSimplify () =
			 let
			    val _ = Cases.foreach (cases, forceBlock)
			    val _ = Option.app (default, forceBlock)
			 in
			    ([], Case {test = use test,
				       cases = cases,
				       default = default})
			 end
		   in
		      simplifyCase {test = test,
				    cases = cases,
				    default = default,
				    cantSimplify = cantSimplify}
		   end
	      | Goto {dst, args} => goto (dst, varInfos args)
	      | Prim {prim, args, failure, success} =>
		   let
		      val args = varInfos args
		   in
		      case primApp (prim, args) of
			 Prim.ApplyResult.Const c =>
			    let
			       val _ = deleteLabel failure
			       val x = Var.newNoname ()
			       val isUsed = ref false
			       val vi =
				  VarInfo.T {isUsed = isUsed,
					     numOccurrences = ref 0,
					     value = ref (SOME (Value.Const c)),
					     var = x}
			       val (ss, t) = goto (success, Vector.new1 vi)
			       val ss =
				  if !isUsed
				     then Statement.T {var = SOME x,
						       ty = Type.ofConst c,
						       exp = Exp.Const c}
					:: ss
				  else ss
			    in
			       (ss, t)
			    end
		       | Prim.ApplyResult.Var x =>
			    let
			       val _ = deleteLabel failure
			       val (ss, t) = goto (success, Vector.new1 x)
			    in
			       (ss, t)
			    end
		       | _ =>
			    let
			       val _ = forceBlock failure
			       val _ = forceBlock success
			    in
			       ([],
				Prim {prim = prim,
				      args = uses args,
				      failure = failure,
				      success = success})
			    end
		   end
	      | Raise xs => ([], Raise (simplifyVars xs))
	      | Return xs => ([], Return (simplifyVars xs))
		   )
	 and simplifyCase {test: VarInfo.t, cases, default, cantSimplify}
	    : Statement.t list * Transfer.t =
	    if Cases.isEmpty cases
	       then (case default of
			NONE => ([], Bug)
		      | SOME l => goto (l, Vector.new0 ()))
	    else
	       let
		  fun findCase (cases, is, args) =
		     let
			val n = Vector.length cases
			fun loop k =
			   if k = n
			      then
				 (case default of
				     NONE => ([], Bug)
				   | SOME j => goto (j, Vector.new0 ()))
			   else
			      let
				 val (i, j) = Vector.sub (cases, k)
			      in
				 if is i
				    then (Int.for (k + 1, n, fn k =>
						   deleteLabel
						   (#2 (Vector.sub (cases, k))))
					  ; Option.app (default, deleteLabel)
					  ; goto (j, args))
				 else (deleteLabel j; loop (k + 1))
			      end
		     in loop 0
		     end
	       in
		  case (VarInfo.value test, cases) of
		     (SOME (Value.Const c), _) =>
			let
			   fun doit (l, z) =
			      findCase (l, fn z' => z = z', Vector.new0 ())
			in
			   case (cases, Const.node c) of
			      (Cases.Char l, Const.Node.Char c) => doit (l, c)
			    | (Cases.Int l, Const.Node.Int i) => doit (l, i)
			    | (Cases.Word l, Const.Node.Word w) => doit (l, w)
			    | (Cases.Word8 l, Const.Node.Word w) =>
				 doit (l, Word8.fromWord w)
			    | _ => Error.bug "strange constant for cases"
			end
		   | (SOME (Value.Con {con, args}), Cases.Con cases) =>
			findCase (cases, fn c => Con.equals (con, c), args)
		   | (SOME v, _) =>
			Error.bug (concat ["strange bind for case test: ",
					   Layout.toString (Value.layout v)])
		   | (NONE, _) =>
			(* If all cases are the same, eliminate the case. *)
			let
			   val l = Cases.hd cases
			   val i = labelIndex l
			   val m = Array.sub (meanings, i)
			   local
			      open LabelMeaning
			   in
			      fun usesFormal (m: t): bool =
				 case m of
				    Block _ => true
				  | Bogus => false (* doesn't matter *)
				  | Bug => false
				  | Case _ => true
				  | Goto {args, ...} => Positions.usesFormal args
				  | Raise ps => Positions.usesFormal ps
				  | Return ps => Positions.usesFormal ps
			      fun equals (m: t, m': t): bool =
				 case (m, m') of
				    (Goto {dst, args},
				     Goto {dst = dst', args = args'}) =>
				       Label.equals (dst, dst')
				       andalso Positions.equals (args, args')
				  | (Raise ps, Raise ps') =>
				       Positions.equals (ps, ps')
				  | (Return ps, Return ps') =>
				       Positions.equals (ps, ps')
				  | _ => false
			   end
			   fun isOk (l: Label.t): bool =
			      let
				 val m' = Array.sub (meanings, labelIndex l)
			      in
				 not (usesFormal m') andalso equals (m, m')
			      end
			in
			   if (not (usesFormal m)
			       andalso Cases.forall (cases, isOk)
			       andalso (case default of
					   NONE => true
					 | SOME l => isOk l))
			      then
				 let
				    fun free (p: Position.t): VarInfo.t =
				       case p of
					  Position.Free x => varInfo x
					| _ => Error.bug "free"
				    val _ = addLabel l
				    val _ = Cases.foreach (cases, deleteLabel)
				    val _ = Option.app (default, deleteLabel)
				 in
				    case m of
				       LabelMeaning.Goto {dst, args} =>
					  (addLabel dst
					   ; deleteLabel l
					   ; goto (dst, Vector.map (args, free)))
				     | LabelMeaning.Raise ps =>
					  (deleteLabel l
					   ; ([], Raise (Vector.map
							 (ps, use o free))))
				     | LabelMeaning.Return ps =>
					  (deleteLabel l
					   ; ([], Return (Vector.map
							  (ps, use o free))))
				     | _ => Error.bug "strange useless case"
				 end
			   else
			      cantSimplify ()
			end
	       end
	 and goto arg : Statement.t list * Transfer.t =
	    traceGoto
	    (fn (dst: Label.t, args: VarInfo.t vector) =>
	    let
	       val _ =
		  Control.diagnostic
		  (fn () =>
		   let
		      open Layout
		   in
		      seq [str "goto ",
			   Label.layout dst,
			   Vector.layout VarInfo.layout args]
		   end)
	       val i = labelIndex dst
	       val n = Array.sub (inDegree, i)
	       val _ = Assert.assert ("goto", fn () => n >= 1)
	       fun simple () = ([], Goto {dst = dst, args = uses args})
	    in
	       if n = 1
		  then let
			  val _ = Array.update (inDegree, i, 0)
			  val b = Vector.sub (blocks, i)
			  val _ =
			     Vector.foreach2
			     (Block.args b, args, fn ((x, _), vi) =>
			      setVarInfo (x, vi))
		       in
			  simplifyBlock b
		       end
	       else if Array.sub (isBlock, i)
		       then simple ()
	       else
		  let
		     fun normal () = (forceBlock dst; simple ())
		     fun extract p =
			case p of
			   Position.Formal n => Vector.sub (args, n)
			 | Position.Free x => varInfo x
		     datatype z = datatype LabelMeaning.t
		  in
		     case Array.sub (meanings, i) of
			Block _ => normal ()
		      | Bogus => Error.bug "bogus label meaning"
		      | Bug => normal ()
		      | Case {cases, default} =>
			   let
			      val _ = Array.update (inDegree, i, n - 1)
			      val _ = Cases.foreach (cases, addLabel)
			      val _ = Option.app (default, addLabel)
			      fun cantSimplify () =
				 (Array.update (inDegree, i, n)
				  ; Cases.foreach (cases, deleteLabel)
				  ; Option.app (default, deleteLabel)
				  ; normal ())
			   in
			      simplifyCase {cantSimplify = cantSimplify,
					    cases = cases,
					    default = default,
					    test = Vector.sub (args, 0)}
			   end
		      | Goto {dst, args} =>
			   if Array.sub (isHeader, i)
			      then normal ()
			   else
			      (Array.update (inDegree, i, n - 1)
			       ; addLabel dst
			       ; goto (dst, Vector.map (args, extract)))
		      | Raise ps =>
			   ([], Transfer.Raise (Vector.map (ps, use o extract)))
		      | Return ps =>
			   ([], Transfer.Return (Vector.map (ps, use o extract)))
		  end
	    end) arg
	 and evalStatement (s as Statement.T {var, ty, exp})
	    : Statement.t list -> Statement.t list =
	    let
	       fun delete ss = ss
	       fun doit {makeExp: unit -> Exp.t,
			 sideEffect: bool,
			 value: Value.t option} =
		  let
		     fun make var =
			Statement.T {var = var,
				     ty = ty,
				     exp = makeExp ()}
		  in
		     case var of
			NONE =>
			   if sideEffect
			      then (fn ss => make NONE :: ss)
			   else delete
		      | SOME x =>
			   let
			      val VarInfo.T {isUsed, value = r, ...} = varInfo x
			      val _ = r := value
			   in
			      fn ss =>
			      if !isUsed
				 then make (SOME x) :: ss
			      else if sideEffect
				      then make NONE :: ss
				   else ss
			   end
		  end
	       fun setVar vi =
		  (Option.app (var, fn x => setVarInfo (x, vi))
		   ; delete)
	       fun construct (v: Value.t, makeExp) =
		  doit {makeExp = makeExp,
			sideEffect = false,
			value = SOME v}
	    in
	       case exp of
		  ConApp {con, args} =>
		     let
			val args = varInfos args
		     in
			construct (Value.Con {con = con, args = args},
				   fn () => ConApp {con = con,
						    args = uses args})
		     end
		| Const c => construct (Value.Const c, fn () => exp)
		| HandlerPush l =>
		     if Array.sub (isHandler, labelIndex l)
			then (forceBlock l
			      ; fn ss => s :: ss)
		     else delete
		| HandlerPop l => 
		     if Array.sub (isHandler, labelIndex l)
			then (fn ss => s :: ss)
		     else delete
		| PrimApp {prim, targs, args} =>
		     let
			val args = varInfos args
			fun apply {prim, targs, args} =
			   doit {sideEffect = Prim.maySideEffect prim,
				 makeExp = fn () => PrimApp {prim = prim,
							     targs = targs,
							     args = uses args},
				 value = NONE}
			datatype z = datatype Prim.ApplyResult.t
		     in
			case primApp (prim, args) of
			   Apply (p, args) => apply {prim = p,
						     targs = Vector.new0 (),
						     args = Vector.fromList args}
			 | Bool b =>
			      let
				 val con = Con.fromBool b
			      in
				 construct (Value.Con {con = con,
						       args = Vector.new0 ()},
					    fn () =>
					    ConApp {con = con,
						    args = Vector.new0 ()})
			      end
			 | Const c => construct (Value.Const c,
						 fn () => Exp.Const c)
			 | Unknown => apply {prim = prim,
					     targs = targs,
					     args = args}
			 | Var vi => setVar vi
		     end
		| Select {tuple, offset} =>
		     let
			val vi as VarInfo.T {value, ...} = varInfo tuple
		     in
			case !value of
			   NONE =>
			      doit {makeExp = fn () => Select {tuple = use vi,
							       offset = offset},
				    sideEffect = false,
				    value = NONE}
			 | SOME (Value.Tuple vs) =>
			      setVar (Vector.sub (vs, offset))
			 | _ => Error.bug "select of non-tuple"
		     end
		| Tuple xs =>
		     let
			val xs = varInfos xs
		     in
			construct (Value.Tuple xs,
				   fn () => Tuple (uses xs))
		     end
		| Var x => setVar (varInfo x)
		| _ => doit {makeExp = fn () => exp,
			     sideEffect = true,
			     value = NONE}
	    end
	 val _ = forceBlock start
	 val f = 
	    Function.new {args = args,
			  blocks = Vector.fromList (!newBlocks),
			  name = name,
			  raises = raises,
			  returns = returns,
			  start = start}
	 val _ = Function.clear f
      in
	 f
      end
   end

val traceShrinkFunction =
   Trace.trace2 ("Shrink.shrinkFunction",
		 Func.layout o Function.name,
		 Bool.layout,
		 Func.layout o Function.name)

val shrinkFunction =
   fn g =>
   let
      val s = shrinkFunction g
   in
      fn f =>
      (traceShrinkFunction s (f, true)
       handle e => (Error.bug (concat ["shrinker raised ",
				       Layout.toString (Exn.layout e)])
		    ; raise e))
   end

fun shrink (Program.T {datatypes, globals, functions, main}) =
   let
      val s = shrinkFunction globals
   in
      Program.T {datatypes = datatypes,
		 globals = globals,
		 functions = List.revMap (functions, s),
		 main = main}
   end

end
