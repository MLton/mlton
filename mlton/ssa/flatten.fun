(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
(*
 * Flatten arguments to jumps, constructors, and functions.
 * If a tuple is explicitly available at all uses of a jump (resp. function)
 * then
 *   - The formals and call sites are changed so that the components of the
 *     tuple are passed.
 *   - The tuple is reconstructed at the beginning of the body of the jump.
 *
 * Similarly, if a tuple is explicitly available at all uses of a constructor,
 *   - The constructor argument type is changed to flatten the tuple type.
 *   - The tuple is passed flat at each ConApp.
 *   - The tuple is reconstructed at each Case target.
 *)

functor Flatten (S: FLATTEN_STRUCTS): FLATTEN = 
struct

open S
open Exp Transfer

structure Rep =
   struct
      structure Set = DisjointSet
	 
      datatype t =
	 Leaf
       | Tuple of bool Set.t (* true means keep it as a tuple *)

      local open Layout
      in
	 val layout =
	    fn Leaf => str "leaf"
	     | Tuple s => Bool.layout (Set.value s)
      end

      val isFlat =
	 fn Tuple s => not (Set.value s)
	  | _ => false
	       
      fun fromType t =
	 case Type.detupleOpt t of
	    NONE => Leaf
	  | SOME l => Tuple (Set.singleton false)

      fun fromTypes (ts: Type.t vector): t vector =
	 Vector.map (ts, fromType)

      fun fromFormals (xts: (Var.t * Type.t) vector): t vector =
	 Vector.map (xts, fromType o #2)

      val tuplize: t -> unit =
	 fn Leaf => ()
	  | Tuple s => Set.setValue (s, true)

      fun tuplizes rs = Vector.foreach (rs, tuplize)

      val unify =
	 fn (Leaf, Leaf) => ()
	  | (Tuple s, Tuple s') =>
	       let val isTuple = Set.value s orelse Set.value s'
	       in Set.union (s, s')
		  ; Set.setValue (s, isTuple)
	       end
	  | _ => Error.bug "unify"

      fun unifys (rs, rs') = Vector.foreach2 (rs, rs', unify)

      val layouts = Vector.layout layout
	 
      val unifys = Trace.trace2 ("unifys", layouts, layouts, Unit.layout) unifys
   end

fun flatten (program as Program.T {datatypes, globals, functions, main}) =
   let
      val {get = conInfo: Con.t -> {argsTypes: Type.t vector,
				    args: Rep.t vector},
	   set = setConInfo, ...} =
	 Property.getSetOnce 
	 (Con.plist, Property.initRaise ("conInfo", Con.layout))
      val conArgs = #args o conInfo
      val _ =
	 Vector.foreach
	 (datatypes, fn Datatype.T {cons, ...} =>
	  Vector.foreach
	  (cons, fn {con, args} =>
	   setConInfo (con, {argsTypes = args,
			     args = Vector.map (args, Rep.fromType)})))
      val {get = funcInfo: Func.t -> {args: Rep.t vector,
				      returns: Rep.t vector option},
	   set = setFuncInfo, ...} =
	 Property.getSetOnce 
	 (Func.plist, Property.initRaise ("funcInfo", Func.layout))
      val funcArgs = #args o funcInfo
      val funcReturns = #returns o funcInfo
      val _ =
	 List.foreach
	 (functions, fn f =>
	  let val {name, args, returns, ...} = Function.dest f
	  in setFuncInfo (name, {args = Rep.fromFormals args,
				 returns = Option.map (returns, Rep.fromTypes)})
	  end)
      val {get = labelInfo: Label.t -> {args: Rep.t vector},
	   set = setLabelInfo, ...} =
	 Property.getSetOnce
	 (Label.plist, Property.initRaise ("labelInfo", Label.layout))
      val labelArgs = #args o labelInfo
      val {get = varInfo: Var.t -> {tuple: Var.t vector option},
	   set = setVarInfo, ...} =
	 Property.getSetOnce
	 (Var.plist, Property.initConst {tuple = NONE})
      val varTuple = #tuple o varInfo
      fun coerce (x: Var.t, r: Rep.t) =
	 case varTuple x of
	    NONE => Rep.tuplize r
	  | _ => ()
      fun coerces (xs: Var.t vector, rs: Rep.t vector) =
	 Vector.foreach2 (xs, rs, coerce)

      fun doitStatement (Statement.T {var, ty, exp}) =
	 case exp of
	    Tuple xs => setVarInfo (valOf var, {tuple = SOME xs})
	  | ConApp {con, args} => coerces (args, conArgs con)
	  | _ => ()
      val _ = Vector.foreach (globals, doitStatement)
      val _ =
	 List.foreach
	 (functions, fn f =>
	  let
	     val {name, blocks, ...} = Function.dest f
	     val returns = funcReturns name
	  in
	     Vector.foreach
	     (blocks, fn Block.T {label, args, statements, ...} =>
	      (setLabelInfo (label, {args = Rep.fromFormals args})
	       ; Vector.foreach (statements, doitStatement)))
	     ; Vector.foreach
	       (blocks, fn Block.T {label, transfer, ...} =>
		case transfer of
		   Return xs =>
		      (case returns of
			  NONE => Error.bug "return mismatch"
			| SOME rs => coerces (xs, rs))
		 | Call {func, args, return} =>
		      let
			 val {args = funcArgs, returns = funcReturns} =
			    funcInfo func
			 val _ = coerces (args, funcArgs)
		      in
			 case return of
			    Return.Dead => ()
			  | Return.HandleOnly => ()
			  | Return.NonTail {cont, handler} =>
			       (Option.app (funcReturns, fn rs =>
					    Rep.unifys (rs, labelArgs cont))
				; (Handler.foreachLabel
				   (handler, fn handler => 
				    Rep.tuplizes (labelArgs handler))))
			  | Return.Tail =>
			       (case (funcReturns, returns) of
				   (SOME rs, SOME rs') => Rep.unifys (rs, rs')
				 | _ => ())
		      end
		 | Goto {dst, args} => coerces (args, labelArgs dst)
		 | _ => ())
	  end)
      val _ =
	 Control.diagnostics
	 (fn display =>
	  List.foreach
	  (functions, fn f => 
	   let 
	      val name = Function.name f
	      val {args, returns} = funcInfo name
	      open Layout
	   in 
	      display (seq [Func.layout name,
			    str " ",
			    align [Vector.layout Rep.layout args,
				   Option.layout
				   (Vector.layout Rep.layout) returns]])
	   end))
      fun flattenTypes (ts: Type.t vector, rs: Rep.t vector): Type.t vector =
	 Vector.fromList
	 (Vector.fold2 (ts, rs, [], fn (t, r, ts) =>
			if Rep.isFlat r
			   then Vector.fold (Type.detuple t, ts, op ::)
			else t :: ts))
      val datatypes =
	 Vector.map
	 (datatypes, fn Datatype.T {tycon, cons} =>
	  Datatype.T {tycon = tycon,
		      cons = (Vector.map
			      (cons, fn {con, args} =>
			       {con = con,
				args = flattenTypes (args, conArgs con)}))})
      fun flattens (xs: Var.t vector, rs: Rep.t vector) =
	 Vector.fromList
	 (Vector.fold2 (xs, rs, [],
		       fn (x, r, xs) =>
		       if Rep.isFlat r
			  then (case varTuple x of
				   SOME ys => Vector.fold (ys, xs, op ::)
				 | _ => Error.bug "tuple unavailable")
		       else x :: xs))
      fun doitStatement (stmt as Statement.T {var, ty, exp}) =
	 case exp of
	    ConApp {con, args} =>
	       Statement.T {var = var,
			    ty = ty,
			    exp = ConApp {con = con,
					  args = flattens (args, conArgs con)}}
	  | _ => stmt
      val globals = Vector.map (globals, doitStatement)

      fun doitFunction f =
	 let
	    val {name, args, start, blocks, returns, mayRaise} = Function.dest f
	    val {args = argsReps, returns = returnsReps} = funcInfo name

	    val newBlocks = ref []

	    fun doitArgs (args, reps) =
	       let
		  val (args, stmts) =
		     Vector.fold2
		     (args, reps, ([], []), fn ((x, ty), r, (args, stmts)) =>
		      if Rep.isFlat r
			 then let
			         val tys = Type.detuple ty
				 val xs = Vector.map (tys, fn _ => Var.newNoname ())
				 val args =
				    Vector.fold2
				    (xs, tys, args, fn (x, ty, args) =>
				     (x, ty) :: args)
			      in 
				 (args,
				  Statement.T {var = SOME x,
					       ty = ty,
					       exp = Tuple xs}
				  :: stmts)
			      end
		      else ((x, ty) :: args, stmts))
	       in
		 (Vector.fromList args, Vector.fromList stmts)
	       end

	    fun doitCaseCon {test, cases, default} =
	       let
		  val cases =
		     Vector.map
		     (cases, fn (c, l) =>
		      let
			 val {args, argsTypes} = conInfo c
			 val actualReps = labelArgs l
		      in if Vector.forall2
			    (args, actualReps, fn (r, r') =>
			     Rep.isFlat r = Rep.isFlat r')
			    then (c, l)
			 else 
			 (* Coerce from the constructor representation to the
			  * formals the jump expects.
			  *)
			 let
			    val l' = Label.newNoname ()
			    (* The formals need to match the type of the con.
			     * The actuals need to match the type of l.
			     *)
			    val (stmts, formals, actuals) =
			       Vector.fold3
			       (args, actualReps, argsTypes,
				([], [], []),
				fn (r, r', ty, (stmts, formals, actuals)) =>
				if Rep.isFlat r
				   then
				   (* The con is flat *)
				   let
				      val xts =
					 Vector.map
					 (Type.detuple ty, fn ty =>
					  (Var.newNoname (), ty))
				      val xs = Vector.map (xts, #1)
				      val formals =
					 Vector.fold (xts, formals, op ::)
				      val (stmts, actuals) =
					 if Rep.isFlat r'
					    then (stmts,
						  Vector.fold 
						  (xs, actuals, op ::))
					 else
					 let
					    val x = Var.newNoname ()
					 in
					   (Statement.T {var = SOME x,
							 ty = ty,
							 exp = Tuple xs}
					    :: stmts,
					    x :: actuals)
					 end
				   in (stmts, formals, actuals)
				   end
				else
			        (* The con is tupled *)
				let
				   val tuple = Var.newNoname ()
				   val formals = (tuple, ty) :: formals
				   val (stmts, actuals) =
				      if Rep.isFlat r'
					 then
					 let
					    val xts =
					       Vector.map
					       (Type.detuple ty, fn ty =>
						(Var.newNoname (), ty))
					    val xs = Vector.map (xts, #1)
					    val actuals =
					       Vector.fold
					       (xs, actuals, op ::)
					    val stmts =
					       Vector.foldi
					       (xts, stmts,
						fn (i, (x, ty), stmts) =>
						Statement.T 
						{var = SOME x,
						 ty = ty,
						 exp = Select {tuple = tuple,
							       offset = i}}
						:: stmts)
					 in (stmts, actuals)
					 end
				      else (stmts, tuple :: actuals)
				in (stmts, formals, actuals)
				end)
			    val _ =
			       List.push
			       (newBlocks,
				Block.T
				{label = l',
				 args = Vector.fromList formals,
				 statements = Vector.fromList stmts,
				 transfer = Goto {dst = l,
						  args = Vector.fromList actuals}})
			 in
			   (c, l')
			 end
		      end)
	       in Case {test = test,
			cases = Cases.Con cases,
			default = default}
	       end 

	    fun doitTransfer transfer =
	       case transfer of
		  Return xs => Return (flattens (xs, valOf returnsReps))
		| Call {func, args, return} =>
		     Call {func = func, 
			   args = flattens (args, funcArgs func),
			   return = return}
		| Case {test, cases = Cases.Con cases, default} =>
		     doitCaseCon {test = test, 
				  cases = cases, 
				  default = default}
		| Goto {dst, args} =>
		     Goto {dst = dst,
			   args = flattens (args, labelArgs dst)}
		| _ => transfer

	    fun doitBlock (Block.T {label, args, statements, transfer}) =
	       let
		 val (args, stmts) = doitArgs (args, labelArgs label)
		 val statements = Vector.map (statements, doitStatement)
		 val statements = Vector.concat [stmts, statements]
		 val transfer = doitTransfer transfer
	       in
		  Block.T {label = label,
			   args = args,
			   statements = statements,
			   transfer = transfer}
	       end

	    val (args, stmts) = doitArgs (args, argsReps)
	    val start' = Label.newNoname ()
	    val _ = List.push
	            (newBlocks,
		     Block.T {label = start',
			      args = Vector.new0 (),
			      statements = stmts,
			      transfer = Goto {dst = start, 
					       args = Vector.new0 ()}})
	    val start = start'
	    val blocks = Vector.map (blocks, doitBlock)
	    val blocks = Vector.concat [Vector.fromList (!newBlocks), blocks]
	    val returns =
	       Option.map
	       (returns, fn ts =>
		flattenTypes (ts, valOf returnsReps))
	 in
	    Function.new {name = name,
			  args = args,
			  start = start,
			  blocks = blocks,
			  returns = returns,
			  mayRaise = mayRaise}
	 end

      val shrink = shrinkFunction globals
      val functions = List.revMap (functions, shrink o doitFunction)
      val program =
	 Program.T {datatypes = datatypes,
		    globals = globals,
		    functions = functions,
		    main = main}
      val _ = Program.clear program
   in
      program
   end
end
