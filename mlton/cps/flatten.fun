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
 *   - The tuple is reconstructed at each ConSelect.
 *
 * The trickiest bit is for constructors, because ConSelect offsets must be
 * changed and the tuple must be reconstructed.  For example,
 *
 * datatype t = T of [(u * v), (w * z)]
 * val x = ConSelect {con = T, offset = 2, variant = v}
 *
 * Suppose that both args to T are flattened.  Then this becomes:
 *
 * datatype t = T of [u, v, w, z]
 * val a = ConSelect {con = T, offset = 3, variant = v}
 * val b = ConSelect {con = T, offset = 4, variant = v}
 * val x = (a, b)
 *
 *)

functor Flatten (S: FLATTEN_STRUCTS): FLATTEN = 
struct

open S
open Dec PrimExp Transfer

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

fun flatten (Program.T {datatypes, globals, functions, main}) =
   let
      val {get = conInfo: Con.t -> {argTypes: Type.t vector,
				    args: Rep.t vector},
	   set = setConInfo} =
	 Property.getSetOnce (Con.plist, Property.initRaise ("args", Con.layout))
      val conArgs = #args o conInfo
      val _ =
	 Vector.foreach
	 (datatypes, fn {cons, ...} =>
	  Vector.foreach
	  (cons, fn {con, args} =>
	   setConInfo (con, {argTypes = args,
			     args = Vector.map (args, Rep.fromType)})))
      val {get = funcInfo: Func.t -> {args: Rep.t vector,
				      returns: Rep.t vector},
	   set = setFuncInfo} =
	 Property.getSetOnce (Func.plist,
			      Property.initRaise ("Flatten.info", Func.layout))
      val funcArgs = #args o funcInfo
      val funcInfo =
	 Trace.trace ("funcInfo", Func.layout,
		      fn {args, returns} =>
		      Layout.record [("args", Rep.layouts args),
				     ("returns", Rep.layouts returns)])
	 funcInfo
      val _ =
	 Vector.foreach
	 (functions, fn {name, args, returns, ...} =>
	  setFuncInfo (name, {args = Rep.fromFormals args,
			      returns = Rep.fromTypes returns}))
      val {get = varTuple, set = setVarTuple} =
	 Property.getSetOnce (Var.plist, Property.initConst NONE)
      val {get = jumpArgs: Jump.t -> Rep.t vector, set = setJumpArgs} =
	 Property.getSetOnce (Jump.plist,
			      Property.initRaise ("args", Jump.layout))
      fun coerce (x: Var.t, r: Rep.t) =
	 case varTuple x of
	    NONE => Rep.tuplize r
	  | _ => ()
      fun coerces (xs: Var.t vector, rs: Rep.t vector) =
	 Vector.foreach2 (xs, rs, coerce)
      val coerces =
	 Trace.trace2 ("coerces",
		       Vector.layout Var.layout,
		       Vector.layout Rep.layout,
		       Unit.layout)
	 coerces
      fun loopBind {var, ty, exp} =
	 case exp of
	    Tuple xs => setVarTuple (var, SOME xs)
	  | ConApp {con, args} => coerces (args, conArgs con)
	  | _ => ()
      val _ = Vector.foreach (globals, loopBind)
      fun loopExp (e, returns) =
	 let val {decs, transfer} = Exp.dest e
	 in List.foreach
	    (decs,
	     fn Bind b => loopBind b
	      | Fun {name, args, body} =>
		   (setJumpArgs (name, Rep.fromFormals args)
		    ; loopExp (body, returns))
	      | HandlerPush j => Rep.tuplizes (jumpArgs j)
	      | _ => ())
	    ; (case transfer of
		  Return xs => coerces (xs, returns)
		| Call {func, args, cont} =>
		     let val {args = formals, returns = returns'} = funcInfo func
		     in coerces (args, funcArgs func)
			; Rep.unifys (returns',
				      case cont of
					 NONE => returns
				       | SOME c => jumpArgs c)
		     end
		| Jump {dst, args} => coerces (args, jumpArgs dst)
		| _ => ())
	 end
      val _ = Vector.foreach (functions, fn {name, body, ...} =>
			      loopExp (body, #returns (funcInfo name)))
      val _ =
	 Control.displays
	 ("flat", fn display =>
	  Vector.foreach
	  (functions, fn {name, ...} =>
	   let val {args, returns} = funcInfo name
	      open Layout
	   in display (seq [Func.layout name,
			    str " ",
			    align [Vector.layout Rep.layout args,
				   Vector.layout Rep.layout returns]])
	   end))
      fun flattenTypes (ts: Type.t vector, rs: Rep.t vector): Type.t vector =
	 Vector.fromList
	 (Vector.fold2 (ts, rs, [], fn (t, r, ts) =>
			if Rep.isFlat r
			   then Vector.fold (Type.detuple t, ts, op ::)
			else t :: ts))
      val flattenTypes =
	 Trace.trace2 ("flattenTypes",
		       Vector.layout Type.layout,
		       Vector.layout Rep.layout,
		       Vector.layout Type.layout)
	 flattenTypes
      val datatypes =
	 Vector.map
	 (datatypes, fn {tycon, cons} =>
	  {tycon = tycon,
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
      fun loopBind {var: Var.t, ty: Type.t, exp} =
	 let
	    fun single e = [{var = var, ty = ty, exp = e}]
	 in
	    case exp of
	       ConApp {con, args} =>
		  single (ConApp {con = con,
				  args = flattens (args, conArgs con)})
	     | _ => single exp
	 end
      val globals =
	 Vector.fromList
	 (Vector.foldr (globals, [], fn (b, bs) => loopBind b @ bs))
      fun loopCase {test, cases, default, cause} =
	 let
	    val (cases, decs) =
	       Vector.mapAndFold
	       (cases, [], fn ((c, j), decs0) =>
		let
		   val {args, argTypes} = conInfo c
		   val actualReps = jumpArgs j
		in if Vector.forall2 (args, actualReps, fn (r, r') =>
				     Rep.isFlat r = Rep.isFlat r')
		      then ((c, j), decs0)
		   else
		      (* Coerce from the constructor representation to the
		       * formals the jump expects.
		       *)
		      let
			 val j' = Jump.newNoname ()
			 (* The formals need to match the type of the con.
			  * The actuals need to match the type of j.
			  *)
			 val (decs, formals, actuals) =
			    Vector.fold3
			    (args, actualReps, argTypes,
			     ([], [], []),
			     fn (r, r', t, (decs, formals, actuals)) =>
			     if Rep.isFlat r
				then
				   (* The con is flat *)
				   let
				      val xts =
					 Vector.map
					 (Type.detuple t, fn t =>
					  (Var.newNoname (), t))
				      val xs = Vector.map (xts, #1)
				      val formals =
					 Vector.fold (xts, formals, op ::)
				      val (decs, actuals) =
					 if Rep.isFlat r'
					    then (decs,
						  Vector.fold (xs, actuals,
							       op ::))
					 else
					    let
					       val x = Var.newNoname ()
					       val decs =
						  Bind {var = x,
							ty = t,
							exp = Tuple xs}
						  :: decs
					    in (decs, x :: actuals)
					    end
				   in (decs, formals, actuals)
				   end
			     else
				(* The con is tupled *)
				let
				   val tuple = Var.newNoname ()
				   val formals = (tuple, t) :: formals
				   val (decs, actuals) =
				      if Rep.isFlat r'
					 then
					    let
					       val xts =
						  Vector.map
						  (Type.detuple t, fn t =>
						   (Var.newNoname (), t))
					       val xs = Vector.map (xts, #1)
					       val actuals =
						  Vector.fold
						  (xs, actuals, op ::)
					       val decs =
						  Vector.foldi
						  (xts, decs,
						   fn (i, (x, t), decs) =>
						   Bind {var = x,
							 ty = t,
							 exp = Select {tuple = tuple,
								       offset = i}}
						   :: decs)
					    in (decs, actuals)
					    end
				      else (decs, tuple :: actuals)
				in (decs, formals, actuals)
				end)
			 val body =
			    Exp.make
			    {decs = decs,
			     transfer = Jump {dst = j,
					      args = Vector.fromList actuals}}
			 val d = Fun {name = j',
				      args = Vector.fromList formals,
				      body = body}
		      in ((c, j'), d :: decs0)
		      end
		end)
	 in (decs,
	     Case {test = test,
		   cases = Cases.Con cases,
		   default = default,
		   cause = cause})
	 end
      fun loopTransfer (transfer: Transfer.t, returns): Dec.t list * Transfer.t =
	 case transfer of
	    Return xs => ([], Return (flattens (xs, returns)))
	  | Call {func, args, cont} =>
	       ([], Call {func = func, cont = cont,
			  args = flattens (args, funcArgs func)})
	  | Case {cause, test, cases = Cases.Con cases, default} =>
	       loopCase {cause = cause, test = test, cases = cases,
			 default = default}
	  | Jump {dst, args} =>
	       ([], Jump {dst = dst, args = flattens (args, jumpArgs dst)})
	  | _ => ([], transfer)
      fun loopExp (e: Exp.t, returns: Rep.t vector): Exp.t =
	 let
	    val {decs, transfer} = Exp.dest e
	    val (decs', transfer) = loopTransfer (transfer, returns)
	    val decs =
	       List.foldr
	       (decs, decs', fn (d, ds) =>
		case d of
		   Fun {name, args, body} =>
		      let val (args, body) = loopArgsBody (args, jumpArgs name,
							   body, returns)
		      in Fun {name = name, args = args, body = body}
		      end :: ds
		 | Bind b => List.foldr (loopBind b, ds, fn (b, ds) =>
					 Bind b :: ds)
		 | _ => d :: ds)
	 in Exp.make {decs = decs,
		      transfer = transfer}
	 end
      and loopArgsBody (args: (Var.t * Type.t) vector,
			rs: Rep.t vector,
			body: Exp.t,
			returns: Rep.t vector) =
	 let
	    val (args, prefix) =
	       Vector.fold2
	       (args, rs, ([], []), fn ((x, t), r, (args, prefix)) =>
		if Rep.isFlat r
		   then let val ts = Type.detuple t
			    val xs = Vector.map (ts, fn _ => Var.newNoname ())
			    val args =
			       Vector.fold2 (xs, ts, args, fn (x, t, args) =>
					     (x, t) :: args)
			in (args,
			    Bind {var = x, ty = t, exp = Tuple xs} :: prefix)
			end
		else ((x, t) :: args, prefix))
	 in (Vector.fromList args,
	     Exp.prefixs (loopExp (body, returns), prefix))
	 end
      val loopArgsBody =
	 Trace.trace ("loopArgsBody",
		      fn (_, _, body, _) => Exp.layout body,
		      fn (_, e) => Exp.layout e)
	 loopArgsBody
      val shrinkExp = shrinkExp globals
      val functions =
	 Vector.map
	 (functions, fn {name, args, body, returns} =>
	  let val {args = argReps, returns = returnReps} = funcInfo name
	     val (args, body) = loopArgsBody (args, argReps, body, returnReps)
	  in {name = name,
	      args = args,
	      body = shrinkExp body,
	      returns = flattenTypes (returns, returnReps)}
	  end)
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
