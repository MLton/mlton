(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
			 ty: Type.t option,
			 value: value option ref,
			 var: Var.t}
      and value =
	 Con of {con: Con.t,
		 args: t vector}
	| Const of Const.t
	| Select of {tuple: t, offset: int}
	| Tuple of t vector

      fun equals (T {var = x, ...}, T {var = y, ...}) = Var.equals (x, y)
	 
      fun layout (T {isUsed, numOccurrences, ty, value, var}) =
	 let open Layout
	 in record [("isUsed", Bool.layout (!isUsed)),
		    ("numOccurrences", Int.layout (!numOccurrences)),
		    ("ty", Option.layout Type.layout ty),
		    ("value", Option.layout layoutValue (!value)),
		    ("var", Var.layout var)]
	 end
      and layoutValue v =
	 let open Layout
	 in case v of
	    Con {con, args} => seq [Con.layout con,
				    Vector.layout layout args]
	  | Const c => Const.layout c
	  | Select {tuple, offset} => seq [str "#", Int.layout (offset + 1), 
					   str " ", layout tuple]
	  | Tuple vis => Vector.layout layout vis
	 end

      fun new (x: Var.t, ty: Type.t option) = T {isUsed = ref false,
						 numOccurrences = ref 0,
						 ty = ty,
						 value = ref NONE,
						 var = x}

      fun setValue (T {value, ...}, v) =
	 (Assert.assert ("VarInfo.setValue", fn () => Option.isNone (!value))
	  ; value := SOME v)


      fun numOccurrences (T {numOccurrences = r, ...}) = r
      fun ty (T {ty, ...}): Type.t option = ty
      fun value (T {value, ...}): value option = !value
      fun var (T {var, ...}): Var.t = var
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
	  | Select {tuple, offset} => 
	       Exp.Select {tuple = VarInfo.var tuple, offset = offset}
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

fun shrinkFunction (globals: Statement.t vector) =
   let
      fun use (VarInfo.T {isUsed, var, ...}): Var.t =
	 (isUsed := true
	  ; var)
      fun uses (vis: VarInfo.t vector): Var.t vector = Vector.map (vis, use)
      (* varInfo can't be getSetOnce because of setReplacement. *)
      val {get = varInfo: Var.t -> VarInfo.t, set = setVarInfo, ...} =
	 Property.getSet (Var.plist, 
			  Property.initFun (fn x => VarInfo.new (x, NONE)))
(*	 Property.getSet (Var.plist, Property.initFun VarInfo.new) *)
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
	     val _ = Option.app
	             (var, fn x =>
		      setVarInfo (x, VarInfo.new (x, SOME ty)))
	     fun construct v =
		Option.app (var, fn x => VarInfo.setValue (varInfo x, v))
	  in case exp of
	     ConApp {con, args} =>
		construct (Value.Con {con = con,
				      args = Vector.map (args, varInfo)})
	   | Const c => construct (Value.Const c)
	   | Select {tuple, offset} =>
		construct (Value.Select {tuple = varInfo tuple,
					 offset = offset})
	   | Tuple xs => construct (Value.Tuple (Vector.map (xs, varInfo)))
	   | Var y => Option.app (var, fn x => setVarInfo (x, varInfo y))
	   | _ => ()
	  end)
   in
      fn (f: Function.t, mayDelete: bool) =>
      let
	 val _ = Function.clear f
	 val {args, blocks, name, raises, returns, sourceInfo, start, ...} =
	    Function.dest f
	 val _ = Vector.foreach
	         (args, fn (x, ty) => 
		  setVarInfo (x, VarInfo.new (x, SOME ty)))
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
	 fun addLabelIndex i = Array.inc (inDegree, i)
	 val isHeader = Array.array (numBlocks, false)
	 val numHandlerUses = Array.array (numBlocks, 0)
	 fun layoutLabel (l: Label.t): Layout.t =
	    let
	       val i = labelIndex l
	    in
	       Layout.record [("label", Label.layout l),
			      ("inDegree", Int.layout (Array.sub (inDegree, i)))]
	    end
	 fun incAux aux =
	    case aux of
	       LabelMeaning.Goto {dst, ...} =>
		  addLabelIndex (LabelMeaning.blockIndex dst)
	     | _ => ()
	 fun incLabel (l: Label.t): unit =
	    incLabelMeaning (labelMeaning l)
	 and incLabelMeaning (LabelMeaning.T {aux, blockIndex, ...}): unit =
	    let
	       val i = blockIndex
	       val n = Array.sub (inDegree, i)
	       val _ = Array.update (inDegree, i, 1 + n)
	    in
	       if n = 0
		  then incAux aux
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
	       val _ = Vector.foreach
		       (args, fn (x, ty) =>
			setVarInfo (x, VarInfo.new (x, SOME ty)))
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
		  Arith {args, overflow, success, ...} =>
		     (incVars args
		      ; incLabel overflow
		      ; incLabel success
		      ; normal ())
		| Bug =>
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
			   andalso 1 = numVarOccurrences test
			   andalso Var.equals (test, #1 (Vector.sub (args, 0)))
			   then
			      doit (LabelMeaning.Case {cases = cases,
						       default = default})
			else
			   normal ()
		     end
		| Goto {dst, args = actuals} =>
		     let
			val _ = incVars actuals
			val m = labelMeaning dst
		     in
			if 0 <> Vector.length statements
			   orelse Array.sub (isHeader, i)
			   then (incLabelMeaning m
				 ; normal ())
			else
			   if Vector.equals (args, actuals, fn ((x, _), x') =>
					     Var.equals (x, x')
					     andalso 1 = numVarOccurrences x)
			      then m (* It's an eta. *)
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
		 | Runtime {args, return, ...} =>
		     (incVars args
		      ; incLabel return
		      ; normal ())
	    end
	 val _ = incLabel start
	 fun indexMeaning i =
	    case Array.sub (states, i) of
	       State.Visited m => m
	     | _ => Error.bug "indexMeaning not computed"
	 val indexMeaning =
	    Trace.trace ("Shrink.indexMeaning", Int.layout, LabelMeaning.layout)
	    indexMeaning
	 val labelMeaning = indexMeaning o labelIndex
	 val labelIndex' = labelIndex
	 val labelIndex = LabelMeaning.blockIndex o labelMeaning
	 fun meaningLabel m =
	    Block.label (Vector.sub (blocks, LabelMeaning.blockIndex m))
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
		fun bumpIndex i = Array.inc (inDegree', i)
		fun bumpMeaning m = bumpIndex (LabelMeaning.blockIndex m)
		val bumpLabel = bumpMeaning o labelMeaning
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
	 val addLabel =
	    Trace.trace ("Shrink.addLabel", layoutLabel, Unit.layout) addLabel
	 val addLabelMeaning = addLabelIndex o LabelMeaning.blockIndex
	 fun layoutLabelMeaning m =
	    Layout.record
	    [("inDegree", Int.layout (Array.sub
				      (inDegree, LabelMeaning.blockIndex m))),
	     ("meaning", LabelMeaning.layout m)]
	 val traceDeleteLabelMeaning =
	    Trace.trace ("Shrink.deleteLabelMeaning",
			 layoutLabelMeaning, Unit.layout)
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
						      (LabelMeaning.blockIndex
						       (labelMeaning l))))
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
		     traceApply Prim.apply
		     (prim, Vector.toList args', VarInfo.equals)
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
	 val traceEvalStatement =
	    Trace.trace
	    ("Shrink.evalStatement",
	     Statement.layout,
	     Layout.ignore: (Statement.t list -> Statement.t list) -> Layout.t)
	 val traceSimplifyTransfer =
	    Trace.trace ("Shrink.simplifyTransfer",
			 Transfer.layout,
			 Layout.tuple2 (List.layout Statement.layout,
					Transfer.layout))
	 val newBlocks = ref []
	 fun simplifyLabel l =
	    let
	       val m = labelMeaning l
	       val _ = forceMeaningBlock m
	    in
	       meaningLabel m
	    end
	 and forceBlock (l: Label.t): unit = forceMeaningBlock (labelMeaning l)
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
	 and simplifyTransfer arg : Statement.t list * Transfer.t =
	    traceSimplifyTransfer
	    (fn (t: Transfer.t) =>
	    case t of
	        Arith {prim, args, overflow, success, ty} =>
		   let
		      val args = varInfos args
		   in
		      case primApp (prim, args) of
			 Prim.ApplyResult.Const c =>
			    let
			       val _ = deleteLabel overflow
			       val x = Var.newNoname ()
			       val isUsed = ref false
			       val vi =
				  VarInfo.T {isUsed = isUsed,
					     numOccurrences = ref 0,
					     ty = SOME ty,
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
			       val _ = deleteLabel overflow
			    in
			       goto (success, Vector.new1 x)
			    end
		       | Prim.ApplyResult.Overflow =>
			    let
			       val _ = deleteLabel success
			    in
			       goto (overflow, Vector.new0 ())
			    end
		       | _ =>
			    ([], Arith {prim = prim,
					args = uses args,
					overflow = simplifyLabel overflow,
					success = simplifyLabel success,
					ty = ty})
		   end
	     | Bug => ([], Bug)
	     | Call {func, args, return} =>
		  let
		     val return =
			case return of
			   Return.NonTail {cont, handler} =>
			      let
				 val m = labelMeaning cont
				 val i = LabelMeaning.blockIndex m
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
				    then (deleteLabelMeaning m; Return.Tail)
				 else
				    let
				       val _ = forceMeaningBlock m
				       val handler =
					  Handler.map
					  (handler, fn l =>
					   let
					      val m = labelMeaning l
					      val _ = forceMeaningBlock m
					   in
					      meaningLabel m
					   end)
				    in
				       Return.NonTail {cont = meaningLabel m,
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
			 ([],
			  Case {test = use test,
				cases = Cases.map (cases, simplifyLabel),
				default = Option.map (default, simplifyLabel)})
		   in
		      simplifyCase
		      {cantSimplify = cantSimplify,
		       cases = cases,
		       default = default,
		       gone = fn () => (Cases.foreach (cases, deleteLabel)
					; Option.app (default, deleteLabel)),
		       test = test}
		   end
	      | Goto {dst, args} => goto (dst, varInfos args)
	      | Raise xs => ([], Raise (simplifyVars xs))
	      | Return xs => ([], Return (simplifyVars xs))
	      | Runtime {prim, args, return} =>
		   ([], Runtime {prim = prim, 
				 args = simplifyVars args, 
				 return = simplifyLabel return})
		   ) arg
	 and simplifyCase {cantSimplify, cases, default, gone, test: VarInfo.t}
	    : Statement.t list * Transfer.t =
	    let
	       (* tryToEliminate makes sure that the destination meaning
		* hasn't already been simplified.  If it has, then we can't
		* simplify the case.
		*)
	       fun tryToEliminate m =
		  let
		     val i = LabelMeaning.blockIndex m
		  in
		     if Array.sub (inDegree, i) = 0
			then cantSimplify ()
		     else
			let
			   val _ = addLabelIndex i
			   val _ = gone ()
			in
			   gotoMeaning (m, Vector.new0 ())
			end
		  end
	    in
	       if Cases.isEmpty cases
		  then (case default of
			   NONE => ([], Bug)
			 | SOME l => tryToEliminate (labelMeaning l))
	       else
		  let
		     val m = labelMeaning (Cases.hd cases)
		     local
			open LabelMeaning
		     in
			fun usesFormal (T {aux, blockIndex = i, ...}): bool =
			   case aux of
			      Block =>
				 0 < Vector.length (Block.args
						    (Vector.sub (blocks, i)))
			    | Bug => false
			    | Goto {args, ...} => Positions.usesFormal args
			    | Raise ps => Positions.usesFormal ps
			    | Return ps => Positions.usesFormal ps
			    | _ => true
			fun equals (m: t, m': t): bool =
			   case (aux m, aux m') of
			      (Block, Block) => blockIndex m = blockIndex m'
			    | (Bug, Bug) => true
			    | (Goto {dst, args},
			       Goto {dst = dst', args = args'}) =>
				 equals (dst, dst')
				 andalso Positions.equals (args, args')
			    | (Raise ps, Raise ps') => Positions.equals (ps, ps')
			    | (Return ps, Return ps') => Positions.equals (ps, ps')
			    | _ => false
		     end
		     fun isOk (l: Label.t): bool =
			let
			   val m' = labelMeaning l
			in
			   not (usesFormal m') andalso equals (m, m')
			end
		  in
		     if Cases.forall (cases, isOk)
			andalso (case default of
				    NONE => true
				  | SOME l => isOk l)
			then
			   (* All cases the same -- eliminate the case. *)
			   tryToEliminate m
		     else
			let
			   fun findCase (cases, is, args) =
			      let
				 val n = Vector.length cases
				 fun doit (j, args) =
				    let
				       val m = labelMeaning j
				       val _ = addLabelMeaning m
				       val _ = gone ()
				    in
				       gotoMeaning (m, args)
				    end
				 fun loop k =
				    if k = n
				       then
					  (case default of
					      NONE => (gone (); ([], Bug))
					    | SOME j => doit (j, Vector.new0 ()))
				    else
				       let
					  val (i, j) = Vector.sub (cases, k)
				       in
					  if is i
					     then doit (j, args)
					  else loop (k + 1)
				       end
			      in loop 0
			      end
			in
			   case (VarInfo.value test, cases) of
			      (SOME (Value.Const c), _) =>
				 let
				    fun doit (l, z) =
				       findCase (l, fn z' => z = z',
						 Vector.new0 ())
				 in
				    case (cases, Const.node c) of
				       (Cases.Char l, Const.Node.Char c) =>
					  doit (l, c)
				     | (Cases.Int l, Const.Node.Int i) =>
					  doit (l, i)
				     | (Cases.Word l, Const.Node.Word w) =>
					  doit (l, w)
				     | (Cases.Word8 l, Const.Node.Word w) =>
					  doit (l, Word8.fromWord w)
				     | _ =>
					  Error.bug "strange constant for cases"
				 end
			    | (SOME (Value.Con {con, args}), Cases.Con cases) =>
				 findCase (cases, fn c =>
					   Con.equals (con, c), args)
			    | _ => cantSimplify ()
(*
			    | (NONE, _) => cantSimplify ()
			    | (_, _) =>
				 Error.bug
				 (concat
				  ["strange bind for case test: ",
				   Layout.toString (VarInfo.layout test)])
*)
			end
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
		      simplifyCase {cantSimplify = normal,
				    cases = cases,
				    default = default,
				    gone = fn () => deleteLabelMeaning m,
				    test = Vector.sub (args, 0)}
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
	 and evalStatement arg : Statement.t list -> Statement.t list =
	    traceEvalStatement
	    (fn (s as Statement.T {var, ty, exp}) =>
	    let
	       val _ = Option.app 
		       (var, fn x => 
			setVarInfo (x, VarInfo.new (x, SOME ty)))
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
		      case Array.sub (states, labelIndex' l) of
			 State.Visited m =>
			    if 0 = Array.sub (numHandlerUses,
					      LabelMeaning.blockIndex m)
			       then ss
			    else Statement.handlerPush (meaningLabel m) :: ss
		       | _ => ss)
		| HandlerPop l =>
		     (fn ss =>
		      case Array.sub (states, labelIndex' l) of
			 State.Visited m =>
			    let
			       val i = LabelMeaning.blockIndex m
			    in
			       if 0 = Array.sub (numHandlerUses, i)
				  then ss
			       else
				  ((* Ensure that this label isn't deleted*)
				   Array.inc (inDegree, i)
				   ; Array.inc (numHandlerUses, i)
				   ; forceMeaningBlock (indexMeaning i)
				   ; Statement.handlerPop (meaningLabel m) :: ss)
			    end
		       | _ => ss)
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
			 | Var vi => setVar vi
			 | _ => apply {prim = prim,
				       targs = targs,
				       args = args}
		     end
		| Select {tuple, offset} =>
		     let
			val tuple as VarInfo.T {value, ...} = varInfo tuple
		     in
			case !value of
			   SOME (Value.Tuple vs) =>
			      setVar (Vector.sub (vs, offset))
			 | _ =>
			      construct (Value.Select {tuple = tuple, 
						       offset = offset},
					 fn () => Select {tuple = use tuple,
							  offset = offset})
(*
			 | _ => Error.bug
				(concat
				  ["select of non-tuple: ",
				   Layout.toString (VarInfo.layout tuple)])
*)
		     end
		| Tuple xs =>
		     let
			val xs = varInfos xs
		     in
                        case DynamicWind.withEscape
			     (fn escape =>
			      Vector.foldri
			      (xs, NONE, fn (i, VarInfo.T {value, ...}, tuple') => 
			       case !value of
				  SOME (Value.Select {offset, tuple}) =>
				     if offset = i
				        then case tuple' of
					        NONE => 
						   (case VarInfo.ty tuple of
						       SOME ty =>
							  (case Type.detupleOpt ty of
							      SOME ts =>
								 if Vector.length xs =
								    Vector.length ts
								    then SOME tuple
								 else escape NONE
							    | NONE => escape NONE)
						     | NONE => escape NONE)
					      | SOME tuple'' => 
						   if VarInfo.equals (tuple'', tuple)
						      then tuple'
						   else escape NONE
				     else escape NONE
				| _ => escape NONE)) of
			  SOME tuple => setVar tuple
			| NONE => construct (Value.Tuple xs,
					     fn () => Tuple (uses xs))
		     end
		| Var x => setVar (varInfo x)
		| _ => doit {makeExp = fn () => exp,
			     sideEffect = true,
			     value = NONE}
	    end) arg
	 val start = labelMeaning start
	 val _ = forceMeaningBlock start
	 val f = 
	    Function.new {args = args,
			  blocks = Vector.fromList (!newBlocks),
			  name = name,
			  raises = raises,
			  returns = returns,
			  sourceInfo = sourceInfo,
			  start = meaningLabel start}
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

val shrinkFunctionNoDelete =
   fn f => traceShrinkFunction (shrinkFunction (Vector.new0 ())) (f, false)

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

fun eliminateDeadBlocksFunction f =
   let
      val {args, blocks, name, raises, returns, sourceInfo, start} =
	 Function.dest f
      val {get = isLive, set = setLive, rem} =
	 Property.getSetOnce (Label.plist, Property.initConst false)
      val _ = Function.dfs (f, fn Block.T {label, ...} =>
			    (setLive (label, true)
			     ; fn () => ()))
      fun statementIsLive (Statement.T {exp, ...}) =
	 case exp of
	    HandlerPop l => isLive l
	  | HandlerPush l => isLive l
	  | _ => true
      val f =
	 if Vector.forall (blocks, isLive o Block.label)
	    then f
	 else
	    let 
	       val blocks =
		  Vector.keepAllMap
		  (blocks,
		   fn block as Block.T {args, label, statements,
					transfer} =>
		   if isLive label
		      then
			 SOME
			 (if Vector.forall (statements, statementIsLive)
			     then block
			  else
			     let
			        val statements =
				   Vector.keepAll
				   (statements, statementIsLive)
			     in
			        Block.T {args = args,
					 label = label,
					 statements = statements,
					 transfer = transfer}
			     end)
		   else NONE)
	    in
	       Function.new {args = args,
			     blocks = blocks,
			     name = name,
			     raises = raises,
			     returns = returns,
			     sourceInfo = sourceInfo,
			     start = start}
	    end
       val _ = Vector.foreach (blocks, rem o Block.label)
   in
     f
   end

fun eliminateDeadBlocks (Program.T {datatypes, globals, functions, main}) =
   let
      val functions =
	 List.revMap
	 (functions, eliminateDeadBlocksFunction)
   in
      Program.T {datatypes = datatypes,
		 globals = globals,
		 functions = functions,
		 main = main}
   end

end
