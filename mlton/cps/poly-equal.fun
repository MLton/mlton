(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor PolyEqual (S: POLY_EQUAL_STRUCTS): POLY_EQUAL = 
struct

open S

(*
 * This pass implements polymorphic equality.
 *
 * For each datatype tycon and vector type, it builds an equality function and
 * translates calls to = into calls to that function.
 *
 * Also generates calls to primitives stringEqual and intInfEqual.
 *
 * For tuples, it does the equality test inline.  I.E. it does not create
 * a separate equality function for each tuple type.
 *
 * All equality functions are only created if necessary, i.e. if equality
 * is actually used at a type.
 *
 * Optimizaions:
 *  - For datatype tycons that are enumerations, do not build a case dispatch,
 *    just use eq, since you know the backend will represent these as ints.
 *  - Deep equality always does an eq test first.
 *  - If one argument to = is a constant int and the type will get translated
 *    to an IntOrPointer, then just use eq instead of the full equality.  This
 *    is important for implementing code like the following efficiently:
 *       if x = 0  ...    (where x is an IntInf.int)
 *)

open Dec PrimExp Transfer

structure Dexp =
   struct
      open DirectExp

      val casee =
	 fn {test, cases, default, ty} =>
	 casee {cause = Cause.PolyEqual,
		test = test, cases = Con cases, default = default, ty = ty}
	 
      fun andd (e1, e2) =
	 casee {test = e1,
		cases = Vector.new2 ({con = Con.truee,
				      args = Vector.new0 (),
				      body = e2},
				     {con = Con.falsee,
				      args = Vector.new0 (),
				      body = falsee}),
		default = NONE,
		ty = Type.bool}

      val andd = Trace.trace2 ("Dexp.andd", layout, layout, layout) andd

      fun ands es =
	 case es of
	    [] => truee
	  | [e] => e
	  | e :: es => andd (e, ands es)

      val ands = Trace.trace ("Dexp.ands", List.layout layout, layout) ands

      fun or (e1, e2) =
	 casee {test = e1,
		cases = Vector.new2 ({con = Con.truee,
				      args = Vector.new0 (),
				      body = truee},
				     {con = Con.falsee,
				      args = Vector.new0 (),
				      body = e2}),
		default = NONE,
		ty = Type.bool}

      val or = Trace.trace2 ("Dexp.or", layout, layout, layout) or
   end

val toExp = (shrinkExp (Vector.new0 ())) o Dexp.toExp

fun polyEqual (Program.T {datatypes, globals, functions, main}) =
   let
      val {get = info: Tycon.t -> {isEnum: bool,
				   cons: {con: Con.t,
					  args: Type.t vector} vector},
	   set = setInfo} =
	 Property.getSetOnce
	 (Tycon.plist, Property.initRaise ("PolyEqual.info", Tycon.layout))
      val {get = isConst, set = setConst} =
	 Property.getSetOnce (Var.plist, Property.initConst false)
      val isEnum = #isEnum o info
      val tyconCons = #cons o info
      val _ =
	 Vector.foreach
	 (datatypes, fn {tycon, cons} =>
	  setInfo (tycon,
		   {isEnum = Vector.forall (cons, fn {args, ...} =>
					    Vector.isEmpty args),
		    cons = cons}))
      val newFunctions: Function.t list ref = ref []
      val {get = getEqualFunc, set = setEqualFunc} =
	 Property.getSet (Tycon.plist, Property.initConst NONE)
      val {get = getVectorEqualFunc, set = setVectorEqualFunc,
	   destroy = destroyType} =
	 Property.destGetSet (Type.plist, Property.initConst NONE)
      val returns = Vector.new1 Type.bool
      fun equalFunc (tycon: Tycon.t): Func.t =
	 case getEqualFunc tycon of
	    SOME f => f
	  | NONE =>
	       let
		  val name = Func.newString "equal"
		  val _ = setEqualFunc (tycon, SOME name)
		  val arg1 = Var.newNoname ()
		  val arg2 = Var.newNoname ()
		  val ty = Type.con (tycon, Vector.new0 ())
		  val body =
		     Dexp.casee
		     {test = Dexp.var (arg1, ty),
		      default = NONE, ty = Type.bool,
		      cases =
		      Vector.map
		      (tyconCons tycon, fn {con, args} =>
		       let
			  fun makeArgs () =
			     Vector.map (args, fn t => (Var.newNoname (), t))
			  val xs = makeArgs ()
			  val ys = makeArgs ()
			  val cases =
			     Vector.new1
			     {con = con,
			      args = ys,
			      body =
			      if Vector.isEmpty xs
				 then Dexp.truee
			      else
				 let
				    val (x, t) = Vector.sub (xs, 0)
				    val (y, _) = Vector.sub (ys, 0)
				 in
				    Vector.fold2From
				    (xs, ys, 1, equal (x, y, t),
				     fn ((x, t), (y, _), e) =>
				     Dexp.andd (e, equal (x, y, t)))
				 end}
		       in {con = con, args = xs,
			   body = Dexp.casee {test = Dexp.var (arg2, ty),
					      cases = cases,
					      default = SOME Dexp.falsee,
					      ty = Type.bool}}
		       end)}
	       in List.push (newFunctions,
			     Function.T
			     {name = name,
			      args = Vector.new2 ((arg1, ty), (arg2, ty)),
			      body = toExp body,
			      returns = returns})
		  ; name
	       end
      and vectorEqualFunc (t: Type.t): Func.t =
	 case getVectorEqualFunc t of
	    SOME f => f
	  | NONE =>
	       let
		  (* Build two functions, one that checks the lengths and the
		   * other that loops.
		   *)
		  val name = Func.newString "vectorEqual"
		  val _ = setVectorEqualFunc (t, SOME name)
		  val loop = Func.newString "vectorEqualLoop"
		  val vt = Type.vector t
		  val v1 = (Var.newNoname (), vt)
		  val v2 = (Var.newNoname (), vt)
		  val _ =
		     List.push
		     (newFunctions,
		      Function.T
		      {name = name, 
		       args = Vector.new2 (v1, v2),
		       returns = returns,
		       body =
		       toExp
		       (let val v1 = Dexp.var v1
			    val v2 = Dexp.var v2
			    fun length e =
			       Dexp.primApp {prim = Prim.vectorLength,
					     targs = Vector.new1 t,
					     args = Vector.new1 e,
					     ty = Type.int}
			in Dexp.andd (Dexp.eq (length v1, length v2, Type.int),
				      Dexp.call
				      {func = loop,
				       args = Vector.new4 (Dexp.int 0,
							   length v1,
							   v1,
							   v2),
				       ty = Type.bool})
			end)})
		  val v1 = (Var.newNoname (), vt)
		  val v2 = (Var.newNoname (), vt)
		  val i = (Var.newNoname (), Type.int)
		  val len = (Var.newNoname (), Type.int)
		  val _ = 
		     List.push
		     (newFunctions,
		      Function.T
		      {name = loop,
		       args = Vector.new4 (i, len, v1, v2),
		       returns = returns,
		       body =
		       toExp
		       (let val i = Dexp.var i
			    val len = Dexp.var len
			    val v1 = Dexp.var v1
			    val v2 = Dexp.var v2
			    fun sub (v, i) =
			       Dexp.primApp {prim = Prim.vectorSub,
					     targs = Vector.new1 t,
					     args = Vector.new2 (v, i),
					     ty = t}
			in Dexp.or (Dexp.eq (i, len, Type.int),
				    Dexp.name2
				    (sub (v1, i), sub (v2, i),
				     fn (x1, x2) =>
				     Dexp.andd (equal (x1, x2, t),
						Dexp.call
						{func = loop,
						 args = (Vector.new4
							 (Dexp.+ (i, Dexp.int 1),
							  len, v1, v2)),
						 ty = Type.bool})))
			end)})
	       in name
	       end
      and equal (x1: Var.t, x2: Var.t, ty: Type.t): Dexp.t =
	 let
	    val e1 = Dexp.var (x1, ty)
	    val e2 = Dexp.var (x2, ty)
	    fun prim (p, targs) =
	       Dexp.primApp {prim = p,
			     targs = targs, args = Vector.new2 (e1, e2),
			     ty = Type.bool}
	    fun eq () = prim (Prim.eq, Vector.new1 ty)
	    fun hasConstArg () = isConst x1 orelse isConst x2
	 in case Type.dest ty of
	    Type.Char => eq ()
	  | Type.Int => eq ()
	  | Type.IntInf => if hasConstArg ()
			      then eq ()
			   else prim (Prim.intInfEqual, Vector.new0 ())
	  | Type.Word => eq ()
	  | Type.Word8 => eq ()
	  | Type.String => prim (Prim.stringEqual, Vector.new0 ())
	  | Type.Array _ => eq ()
	  | Type.Vector t =>
	       Dexp.call {func = vectorEqualFunc t,
			  args = Vector.new2 (e1, e2),
			  ty = Type.bool}
	  | Type.Ref _ => eq ()
	  | Type.Datatype tycon =>
	       if isEnum tycon orelse hasConstArg ()
		  then eq ()
	       else Dexp.call {func = equalFunc tycon,
			       args = Vector.new2 (e1, e2),
			       ty = Type.bool}
	  | Type.Tuple tys =>
	       let
		  val max = Vector.length tys - 1
		  (* test components i, i+1, ... *)
		  fun loop (i: int): Dexp.t =
		     let
			val ty = Vector.sub (tys, i)
		     in
			Dexp.name2
			(Dexp.select {tuple = e1, offset = i, ty = ty},
			 Dexp.select {tuple = e2, offset = i, ty = ty},
			 fn (y1, y2) =>
			 let val e = equal (y1, y2, ty)
			 in
			    if i = max
			       then e
			    else Dexp.andd (e, loop (i + 1))
			 end)
		     end
	       in
		  if ~1 = max
		     then Dexp.truee
		  else loop 0
	       end
	  | _ => Error.bug "equal of strange type"
	 end
      fun loopBind {var, ty, exp} =
	 let
	    fun const () = setConst (var, true)
	 in
	    case exp of
	       Const c =>
		  (case Const.node c of
		      Const.Node.IntInf i =>
			 if Const.SmallIntInf.isSmall i
			    then const ()
			 else ()
		    | _ => ())
	     | ConApp {args, ...} => if Vector.isEmpty args then const () else ()
	     | _ => ()
	 end
      val _ = Vector.foreach (globals, loopBind)
      fun loopExp e =
      	 let val {decs, transfer} = Exp.dest e
	 in List.foreach (decs,
			  fn Bind b => loopBind b
			   | _ => ())
	    ; (List.foldr
	       (decs, Exp.make {decs = [], transfer = transfer}, fn (d, e) =>
		let fun simple d = Exp.prefix (e, d)
		   fun normal () = simple d
		in case d of
		   Fun {name, args, body} =>
		      simple (Fun {name = name,
				   args = args,
				   body = loopExp body})
		 | Bind {var, exp, ...} =>
		      (case exp of
			  PrimApp {prim, targs, args, ...} =>
			     (case (Prim.name prim, Vector.length targs) of
				 (Prim.Name.MLton_equal, 1) =>
				    let
				       val ty = Vector.sub (targs, 0)
				       fun arg i = Vector.sub (args, i)
				    in Dexp.send
				       (equal (arg 0, arg 1, ty), fn (e, _) =>
					simple (Bind {var = var,
						      ty = Type.bool,
						      exp = e}))
				    end
			       | _ => normal ())
			| _ => normal ())
		 | _ => normal ()
		end))
	 end
      val functions =
	 Vector.map (functions, fn Function.T {name, args, body, returns} =>
		     Function.T {name = name,
				 args = args,
				 returns = returns,
				 body = loopExp body})
      val program =
	 Program.T {datatypes = datatypes,
		    globals = globals,
		    functions = Vector.concat [Vector.fromList (!newFunctions),
					       functions],
		    main = main}
      val _ = destroyType ()
      val _ = Program.clear program
   in
      program
   end

end
