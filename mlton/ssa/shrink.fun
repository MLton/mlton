(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Shrink (S: SHRINK_STRUCTS): SHRINK = 
struct

open S

structure Array =
   struct
      open Array
	 
      fun inc (a: int t, i: int): unit = update (a, i, 1 + sub (a, i))
      fun dec (a: int t, i: int): unit = update (a, i, sub (a, i) - 1)
   end
   
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
      datatype t = T of {aux: aux,
			 blockIndex: int, (* The index of the block *)
			 label: Label.t} (* redundant, the label of the block *)
			 
      and aux =
	 Block
       | Bug
       | Case of {cases: Label.t Cases.t,
		  default: Label.t option}
       | Goto of {dst: t,
		  args: Positions.t}
       | Raise of Positions.t
       | Return of Positions.t

      local
	 fun make f (T r) = f r
      in
	 val aux = make #aux
	 val blockIndex = make #blockIndex
      end

      fun layout (T {aux, blockIndex, label, ...}) =
	 let
	    open Layout
	 in
	    seq [Label.layout label,
		 str " ",
		 case aux of
		    Block => str "Block "
		  | Bug => str "Bug"
		  | Case {cases, default} => str "Case"
		  | Goto {dst, args} =>
		       seq [str "Goto ",
			    tuple [layout dst, Positions.layout args]]
		  | Raise ps => seq [str "Raise ", Positions.layout ps]
		  | Return ps => seq [str "Return ", Positions.layout ps]]
	 end

      fun usesFormal (T {aux, ...}): bool =
	 case aux of
	    Block => true
	  | Bug => false
	  | Case _ => true
	  | Goto {args, ...} => Positions.usesFormal args
	  | Raise ps => Positions.usesFormal ps
	  | Return ps => Positions.usesFormal ps

      fun equals (m: t, m': t): bool =
	 case (aux m, aux m') of
	    (Block, Block) => blockIndex m = blockIndex m'
	  | (Goto {dst, args}, Goto {dst = dst', args = args'}) =>
	       equals (dst, dst')
	       andalso Positions.equals (args, args')
	  | (Raise ps, Raise ps') => Positions.equals (ps, ps')
	  | (Return ps, Return ps') => Positions.equals (ps, ps')
	  | _ => false
   end

structure State =
   struct
      datatype state =
	 Unvisited
       | Visited of LabelMeaning.t
       | Visiting

      val layout =
	 let
	    open Layout
	 in
	    fn Unvisited => str "Unvisited"
	     | Visited m => LabelMeaning.layout m
	     | Visiting => str "Visiting"
	 end
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
      val setVarInfo =
	 Trace.trace2 ("Shrink.setVarInfo",
		       Var.layout, VarInfo.layout, Unit.layout)
	 setVarInfo
      fun varInfos xs = Vector.map (xs, varInfo)
      fun simplifyVar (x: Var.t) = use (varInfo x)
      val simplifyVar =
	 Trace.trace ("Shrink.simplifyVar", Var.layout, Var.layout) simplifyVar
      fun simplifyVars xs = Vector.map (xs, simplifyVar)
      fun incVarInfo (x: VarInfo.t): unit =
	 Int.inc (VarInfo.numOccurrences x)
      fun incVar (x: Var.t): unit = incVarInfo (varInfo x)
      fun incVars xs = Vector.foreach (xs, incVar)
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
	 val states = Array.array (numBlocks, State.Unvisited)
	 val inDegree = Array.array (numBlocks, 0)
	 fun addLabelIndex i =
	    Array.update (inDegree, i, 1 + Array.sub (inDegree, i))
	 val isHeader = Array.array (numBlocks, false)
	 val numHandlerUses = Array.array (numBlocks, 0)
	 fun layoutLabel (l: Label.t): Layout.t =
	    let
	       val i = labelIndex l
	    in
	       Layout.record [("label", Label.layout l),
			      ("inDegree", Int.layout (Array.sub (inDegree, i)))]
	    end
	 fun incLabel (l: Label.t): unit =
	    incLabelMeaning (labelMeaning l)
	 and incLabelMeaning (LabelMeaning.T {aux, blockIndex, ...}): unit =
	    let
	       val i = blockIndex
	       val n = Array.sub (inDegree, i)
	       val _ = Array.update (inDegree, i, 1 + n)
	       datatype z = datatype LabelMeaning.aux
	    in
	       if n = 0
		  then
		     (case aux of
			 Goto {dst, ...} =>
			    addLabelIndex (LabelMeaning.blockIndex dst)
		       | _ => ())
	       else ()
	    end
	 and labelMeaning (l: Label.t): LabelMeaning.t =
	    let
	       val i = labelIndex l
	    in
	       case Array.sub (states, i) of
		  State.Visited m => m
		| State.Visiting =>
		     (Array.update (isHeader, i, true)
		      ; (LabelMeaning.T
			 {aux = LabelMeaning.Block,
			  blockIndex = i,
			  label = Block.label (Vector.sub (blocks, i))}))
		| State.Unvisited => 
		     let
			val _ = Array.update (states, i, State.Visiting)
			val m = computeMeaning i
			val _ = Array.update (states, i, State.Visited m)
		     in
			m
		     end
	    end
	 and computeMeaning (i: int): LabelMeaning.t =
	    let
	       val block as Block.T {label, args, statements, transfer, ...} =
		  Vector.sub (blocks, i)
	       val _ =
		  Vector.foreach
		  (statements, fn s => Exp.foreachVar (Statement.exp s, incVar))
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
	       fun doit aux =
		  LabelMeaning.T {aux = aux,
				  blockIndex = i,
				  label = Block.label (Vector.sub (blocks, i))}
	       fun normal () = doit LabelMeaning.Block
	    in
	       case transfer of
		  Bug =>
		     if 0 = Vector.length statements
			andalso (case returns of
				    NONE => true
				  | SOME ts =>
				       Vector.equals
				       (ts, args, fn (t, (_, t')) =>
					Type.equals (t, t')))
			then doit LabelMeaning.Bug
		     else normal ()
                | Call {func, args, return} =>
		     let
			val _ = incVars args
			val _ =
			   Return.foreachHandler
			   (return, fn l =>
			    Array.inc (numHandlerUses, labelIndex l))
			val _ = Return.foreachLabel (return, incLabel)
		     in
			normal ()
		     end
		| Case {test, cases, default} =>
		     let
			val _ = incVar test
			val _ = Cases.foreach (cases, incLabel)
			val _ = Option.app (default, incLabel)
		     in
			if 0 = Vector.length statements
			   andalso not (Array.sub (isHeader, i))
			   andalso 1 = Vector.length args
			   andalso
			      let
				 val (x, _) = Vector.sub (args, 0)
			      in
				 Var.equals (x, test)
				 andalso 1 = numVarOccurrences x
			      end
			   then
			      doit (LabelMeaning.Case {cases = cases,
						       default = default})
			else normal ()
		     end
		| Goto {dst, args = actuals} =>
		     let
			val _ = incVars actuals
			val m = labelMeaning dst
		     in
			if Array.sub (isHeader, i)
			   orelse 0 <> Vector.length statements
			   then (incLabelMeaning m
				 ; normal ())
			else
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
			      datatype z = datatype LabelMeaning.aux
			   in
			      if n <> n'
				 then (incLabelMeaning m
				       ; normal ())
			      else
				 let
				    fun extract (ps': Positions.t)
				       : Positions.t =
				       Vector.map
				       (ps', fn p =>
					let
					   datatype z = datatype Position.t
					in
					   case p of
					      Free x => Free x
					    | Formal i => Vector.sub (ps, i)
					end)
				    val a =
				       case LabelMeaning.aux m of
					  Block => Goto {dst = m,
							 args = ps}
					| Bug => Bug
					| Case _ => Goto {dst = m,
							  args = ps}
					| Goto {dst, args} =>
					     Goto {dst = dst,
						   args = extract args}
					| Raise ps => Raise (extract ps)
					| Return ps => Return (extract ps)
				 in
				    doit a
				 end
			   end
		     end
		| Prim {args, failure, success, ...} =>
		     (incVars args
		      ; incLabel failure
		      ; incLabel success
		      ; normal ())
		| Raise xs =>
		     let
			val _ = incVars xs
		     in
			if 0 = Vector.length statements
			   andalso (0 = Vector.length xs
				    orelse 0 < Vector.length args)
			   then doit (LabelMeaning.Raise (extract xs))
			else normal ()
		     end
		 | Return xs =>
		      let
			 val _ = incVars xs
		      in
			 if 0 = Vector.length statements
			    andalso (0 = Vector.length xs
				     orelse 0 < Vector.length args)
			    then doit (LabelMeaning.Return (extract xs))
			 else normal ()
		      end
	    end
	 val _ = incLabel start
	 fun indexMeaning i =
	    case Array.sub (states, i) of
	       State.Visited m => m
	     | _ => Error.bug "indexMeaning not computed"
	 val labelMeaning = indexMeaning o labelIndex
	 fun save (f, s) =
	    File.withOut
	    (concat ["/tmp/", Func.toString (Function.name f),
		     ".", s, ".dot"],
	     fn out =>
	     Layout.outputl
	     (#graph (Function.layoutDot (f, fn _ => NONE)),
	      out))
(*	 val _ = save (f, "pre") *)
	 (* *)
	 val _ =
	    if true
	       then ()
	    else
	       Layout.outputl
	       (Vector.layout
		(fn i =>
		 (Layout.record
		  [("label",
		    Label.layout (Block.label (Vector.sub (blocks, i)))),
		   ("inDegree", Int.layout (Array.sub (inDegree, i))),
		   ("state", State.layout (Array.sub (states, i)))]))
		(Vector.tabulate (numBlocks, fn i => i)),
		Out.error)
	 val _ =
	    Assert.assert
	    ("Shrink.labelMeanings", fn () =>
	     let
		val inDegree' = Array.array (numBlocks, 0)
		fun bumpIndex i =
		   Array.update (inDegree', i, 1 + Array.sub (inDegree', i))
		fun bumpMeaning m = bumpIndex (LabelMeaning.blockIndex m)
		fun bumpLabel l =
		   case Array.sub (states, labelIndex l) of
		      State.Visited m => bumpMeaning m
		    | _ => Error.bug "unvisited"
		fun doit (LabelMeaning.T {aux, blockIndex, ...}) =
		   let
		      datatype z = datatype LabelMeaning.aux
		   in
		      case aux of
			 Block =>
			    Transfer.foreachLabel
			    (Block.transfer (Vector.sub (blocks, blockIndex)),
			     bumpLabel)
		       | Bug => ()
		       | Case {cases, default, ...} =>
			    (Cases.foreach (cases, bumpLabel)
			     ; Option.app (default, bumpLabel))
		       | Goto {dst, ...} => bumpMeaning dst
		       | Raise _ => ()
		       | Return _ => ()
		   end
		val _ =
		   Array.foreachi
		   (states, fn (i, s) =>
		    if Array.sub (inDegree, i) > 0
		       then
			  (case s of
			      State.Visited m => doit m
			    | _ => ())
		    else ())
		val _ = bumpMeaning (labelMeaning start)
	     in
		Array.equals (inDegree, inDegree', Int.equals)
		orelse
		let
		   val _ =
		      Layout.outputl
		      (Vector.layout
		       (fn i =>
			(Layout.record
			 [("label",
			   Label.layout (Block.label (Vector.sub (blocks, i)))),
			  ("inDegree", Int.layout (Array.sub (inDegree, i))),
			  ("inDegree'", Int.layout (Array.sub (inDegree', i))),
			  ("state", State.layout (Array.sub (states, i)))]))
		       (Vector.tabulate (numBlocks, fn i => i)),
		       Out.error)
		in
		   false
		end
	     end)
	 val isBlock = Array.array (numBlocks, false)
	 (* Functions for maintaining inDegree. *)
	 val addLabelIndex =
	    fn i =>
	    (Assert.assert ("addLabelIndex", fn () =>
			    Array.sub (inDegree, i) > 0)
	     ; addLabelIndex i)
	 val addLabel = addLabelIndex o labelIndex
	 val addLabelMeaning = addLabelIndex o LabelMeaning.blockIndex
	 fun layoutLabelMeaning m =
	    Layout.record
	    [("inDegree", Int.layout (Array.sub
				      (inDegree, LabelMeaning.blockIndex m))),
	     ("meaning", LabelMeaning.layout m)]
	 val traceDeleteLabelMeaning =
	    Trace.trace ("Shrink.deleteLabelMeaning",
			 layoutLabelMeaning, Unit.layout)
	 fun indexToLabel i = Block.label (Vector.sub (blocks, i))
	 fun deleteLabel l = deleteLabelMeaning (labelMeaning l)
	 and deleteLabelMeaning arg: unit =
	    traceDeleteLabelMeaning
	    (fn (m: LabelMeaning.t) =>
	    let
	       val i = LabelMeaning.blockIndex m
	       val n = Array.sub (inDegree, i) - 1
	       val _ = Array.update (inDegree, i, n)
	       val _ = Assert.assert ("deleteLabelMeaning", fn () => n >= 0)
	    in
	       if n = 0 (* andalso not (Array.sub (isBlock, i)) *)
		  then
		     let
			datatype z = datatype LabelMeaning.aux
		     in
			case LabelMeaning.aux m of
			   Block =>
			      let
				 val t = Block.transfer (Vector.sub (blocks, i))
				 val _ = Transfer.foreachLabel (t, deleteLabel)
				 val _ =
				    case t of
				       Transfer.Call {return, ...} =>
					  Return.foreachHandler
					  (return, fn l =>
					   Array.dec (numHandlerUses,
						      labelIndex l))
				     | _ => ()
			      in
				 ()
			      end
			 | Bug => ()
			 | Case {cases, default} =>
			      (Cases.foreach (cases, deleteLabel)
			       ; Option.app (default, deleteLabel))
			 | Goto {dst, ...} => deleteLabelMeaning dst
			 | Raise _ => ()
			 | Return _ => ()
		     end
	       else ()
	    end) arg
	 val deleteIndex = deleteLabelMeaning o indexMeaning
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
	 fun layoutIndex i =
	    layoutLabel (Block.label (Vector.sub (blocks, i)))
      	 val traceForceMeaningBlock =
	    Trace.trace ("Shrink.forceMeaningBlock",
			layoutLabelMeaning, Unit.layout)
	 val traceSimplifyBlock =
	    Trace.trace ("Shrink.simplifyBlock",
			 layoutLabel o Block.label,
			 Layout.tuple2 (List.layout Statement.layout,
					Transfer.layout))
	 val traceGotoMeaning =
	    Trace.trace2
	    ("Shrink.gotoMeaning",
	     layoutLabelMeaning,
	     Vector.layout VarInfo.layout,
	     Layout.tuple2 (List.layout Statement.layout, Transfer.layout))
	 val newBlocks = ref []
	 fun forceBlock (l: Label.t): unit = forceMeaningBlock (labelMeaning l)
	 and forceMeaningBlock arg =
	    traceForceMeaningBlock
	    (fn (LabelMeaning.T {aux, blockIndex = i, ...}) =>
	     if Array.sub (isBlock, i)
		then ()
	     else
		let
		   val _ = Array.update (isBlock, i, true)
		   val block as Block.T {label, args, ...} =
		      Vector.sub (blocks, i)
		   fun extract (p: Position.t): VarInfo.t =
		      varInfo (case p of
				  Position.Formal n => #1 (Vector.sub (args, n))
				| Position.Free x => x)
		   val (statements, transfer) =
		      let
			 datatype z = datatype LabelMeaning.aux
		      in
			 case aux of
			    Block => simplifyBlock block
			  | Bug => ([], Transfer.Bug)
			  | Case _ => simplifyBlock block
			  | Goto {dst, args} =>
			       gotoMeaning (dst, Vector.map (args, extract))
			  | Raise ps =>
			       ([],
				Transfer.Raise (Vector.map (ps, use o extract)))
			  | Return ps =>
			       ([],
				Transfer.Return (Vector.map (ps, use o extract)))
		      end
		   val _ =
		      List.push
		      (newBlocks,
		       Block.T {label = label,
				args = args,
				statements = Vector.fromList statements,
				transfer = transfer})
		in
		   ()
		end) arg
	 and simplifyBlock arg : Statement.t list * Transfer.t =
	    traceSimplifyBlock
	    (fn (Block.T {label, statements, transfer, ...}) =>
	    let
	       val fs = Vector.map (statements, evalStatement)
	       val (ss, transfer) = simplifyTransfer transfer
	       val statements = Vector.foldr (fs, ss, fn (f, ss) => f ss)
	    in
	       (statements, transfer)
	    end) arg
	 and simplifyTransfer (t: Transfer.t): Statement.t list * Transfer.t =
	    case t of
	       Bug => ([], Bug)
	     | Call {func, args, return} =>
		  let
		     val return =
			case return of
			   Return.NonTail {cont, handler} =>
			      let
				 val i = labelIndex cont
				 val m = indexMeaning i
				 val isTail =
				    (case handler of
					Handler.CallerHandler => true
				      | Handler.Handle _ => false
				      | Handler.None => true)
                                    andalso 
				    (case LabelMeaning.aux m of
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
				    then (deleteIndex i; Return.Tail)
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
			   val m = labelMeaning (Cases.hd cases)
			   fun isOk (l: Label.t): bool =
			      let
				 val m' = labelMeaning l
			      in
				 not (LabelMeaning.usesFormal m')
				 andalso LabelMeaning.equals (m, m')
			      end
			in
			   if (not (LabelMeaning.usesFormal m)
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
				    val _ = addLabelMeaning m
				    val _ = Cases.foreach (cases, deleteLabel)
				    val _ = Option.app (default, deleteLabel)
				 in
				    case LabelMeaning.aux m of
				       LabelMeaning.Goto {dst, args} =>
					  (addLabelMeaning dst
					   ; deleteLabelMeaning m
					   ; (gotoMeaning
					      (dst, Vector.map (args, free))))
				     | LabelMeaning.Raise ps =>
					  (deleteLabelMeaning m
					   ; ([], Raise (Vector.map
							 (ps, use o free))))
				     | LabelMeaning.Return ps =>
					  (deleteLabelMeaning m
					   ; ([], Return (Vector.map
							  (ps, use o free))))
				     | _ => Error.bug "strange useless case"
				 end
			   else
			      cantSimplify ()
			end
	       end
	 and goto (dst: Label.t, args: VarInfo.t vector)
	    : Statement.t list * Transfer.t =
	    gotoMeaning (labelMeaning dst, args)
	 and gotoMeaning arg : Statement.t list * Transfer.t =
	    traceGotoMeaning
	    (fn (m as LabelMeaning.T {aux, blockIndex = i, ...},
		 args: VarInfo.t vector) =>
	     let
		val n = Array.sub (inDegree, i)
		val _ = Assert.assert ("goto", fn () => n >= 1)
		fun normal () =
		   if n = 1
		      then
			 let
			    val _ = Array.update (inDegree, i, 0)
			    val b = Vector.sub (blocks, i)
			    val _ =
			       Vector.foreach2
			       (Block.args b, args, fn ((x, _), vi) =>
				setVarInfo (x, vi))
			 in
			    simplifyBlock b
			 end
		   else
		      let
			 val _ = forceMeaningBlock m
		      in
			 ([],
			  Goto {dst = Block.label (Vector.sub (blocks, i)),
				args = uses args})
		      end
		fun extract p =
		   case p of
		      Position.Formal n => Vector.sub (args, n)
		    | Position.Free x => varInfo x
		datatype z = datatype LabelMeaning.aux
	     in
		case aux of
		   Block => normal ()
		 | Bug => ([], Transfer.Bug)
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
			 orelse Array.sub (isBlock, i)
			 then normal ()
		      else
			 let
			    val n' = n - 1
			    val _ = Array.update (inDegree, i, n')
			    val _ = 
			       if n' > 0
				  then addLabelMeaning dst
			       else ()
			 in
			    gotoMeaning (dst, Vector.map (args, extract))
			 end
		 | Raise ps =>
		      ([], Transfer.Raise (Vector.map (ps, use o extract)))
		 | Return ps =>
		      ([], Transfer.Return (Vector.map (ps, use o extract)))
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
		     (* You must do the forceBlock l in HandlerPop, not in
		      * HandlerPush, because it must occur after where the
		      * nontail call containing the handler was, because the
		      * SSA dominance condition requires all variables used
		      * in the handler to dominate the call, and hence the
		      * HandlerPop.
		      *)
		     (fn ss =>
		      if 0 = Array.sub (numHandlerUses, labelIndex l)
			then ss
		      else s :: ss)
		| HandlerPop l =>
		     (fn ss =>
		      let
			 val i = labelIndex l
		      in
			 if 0 = Array.sub (numHandlerUses, i)
			    then ss
			 else
			    ((* Ensure that this label isn't deleted*)
			     Array.inc (inDegree, i)
			     ; forceMeaningBlock (indexMeaning i)
			     ; s :: ss)
		      end)
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
	 val _ = forceMeaningBlock (labelMeaning start)
	 val f = 
	    Function.new {args = args,
			  blocks = Vector.fromList (!newBlocks),
			  name = name,
			  raises = raises,
			  returns = returns,
			  start = start}
(*	 val _ = save (f, "post") *)
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
