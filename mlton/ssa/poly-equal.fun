(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor PolyEqual (S: POLY_EQUAL_STRUCTS): POLY_EQUAL = 
struct

open S

type int = Int.t
type word = Word.t

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
 * Optimizations:
 *  - For datatype tycons that are enumerations, do not build a case dispatch,
 *    just use eq, since you know the backend will represent these as ints.
 *  - Deep equality always does an eq test first.
 *  - If one argument to = is a constant int and the type will get translated
 *    to an IntOrPointer, then just use eq instead of the full equality.  This
 *    is important for implementing code like the following efficiently:
 *       if x = 0  ...    (where x is an IntInf.int)
 *)

open Exp Transfer

structure DirectExp = DirectExp (S)

structure DirectExp =
   struct
      open DirectExp

      fun add (e1: t, e2: t): t =
	 primApp {prim = Prim.intAdd,
		  targs = Vector.new0 (),
		  args = Vector.new2 (e1, e2),
		  ty = Type.int}

      fun conjoin (e1: t, e2: t): t =
	 casee {test = e1,
		cases = Con (Vector.new2 ({con = Con.truee,
					   args = Vector.new0 (),
					   body = e2},
					  {con = Con.falsee,
					   args = Vector.new0 (),
					   body = falsee})),
		default = NONE,
		ty = Type.bool}

      fun disjoin (e1: t, e2:t ): t =
	 casee {test = e1,
		cases = Con (Vector.new2 ({con = Con.truee,
					   args = Vector.new0 (),
					   body = truee},
					  {con = Con.falsee,
					   args = Vector.new0 (),
					   body = e2})),
		default = NONE,
		ty = Type.bool}
   end

fun polyEqual (Program.T {datatypes, globals, functions, main}) =
   let
      val shrink = shrinkFunction globals

      val {get = tyconInfo: Tycon.t -> {isEnum: bool,
					cons: {con: Con.t,
					       args: Type.t vector} vector},
	   set = setTyconInfo, ...} =
	 Property.getSetOnce
	 (Tycon.plist, Property.initRaise ("PolyEqual.info", Tycon.layout))
      val isEnum = #isEnum o tyconInfo
      val tyconCons = #cons o tyconInfo
      val {get = varInfo: Var.t -> {isConst: bool},
	   set = setVarInfo, ...} =
	 Property.getSetOnce (Var.plist, Property.initConst {isConst = false})
      val _ =
	 Vector.foreach
	 (datatypes, fn Datatype.T {tycon, cons} =>
	  setTyconInfo (tycon,
			{isEnum = Vector.forall (cons, fn {args, ...} =>
						 Vector.isEmpty args),
			 cons = cons}))
      val newFunctions: Function.t list ref = ref []
      val {get = getEqualFunc: Tycon.t -> Func.t option, 
	   set = setEqualFunc, ...} =
	 Property.getSet (Tycon.plist, Property.initConst NONE)
      val {get = getVectorEqualFunc: Type.t -> Func.t option, 
	   set = setVectorEqualFunc,
	   destroy = destroyType} =
	 Property.destGetSet (Type.plist, Property.initConst NONE)
      val returns = Vector.new1 Type.bool
      fun equalFunc (tycon: Tycon.t): Func.t =
	 case getEqualFunc tycon of
	    SOME f => f
	  | NONE =>
	       let
		  val name = Func.newString ("equal_" ^ (Tycon.originalName tycon))
		  val _ = setEqualFunc (tycon, SOME name)

		  local
		     val ty = Type.con (tycon, Vector.new0 ())
		     val arg1 = (Var.newNoname (), ty)
		     val arg2 = (Var.newNoname (), ty)
		     val args = Vector.new2 (arg1, arg2)
		       
		     val darg1 = DirectExp.var arg1
		     val darg2 = DirectExp.var arg2

		     val body = 
		        DirectExp.casee
			{test = darg1,
			 ty = Type.bool,
			 default = NONE,
			 cases =
			 DirectExp.Con
			 (Vector.map
			  (tyconCons tycon, fn {con, args} =>
			   let
			      fun makeArgs () =
			         Vector.map (args, fn ty => (Var.newNoname (), ty))
			      val xs = makeArgs ()
			      val ys = makeArgs ()
			   in
			      {con = con,
			       args = xs,
			       body = 
			       DirectExp.casee
			       {test = darg2,
				ty = Type.bool,
				default = SOME DirectExp.falsee,
				cases =
				DirectExp.Con
				(Vector.new1
				 {con = con,
				  args = ys,
				  body =
				  Vector.fold2
				  (xs, ys, DirectExp.truee,
				   fn ((x, ty), (y, _), de) =>
				   DirectExp.conjoin (de, equal (x, y, ty)))})}}
			   end))}
		     val (start, blocks) = DirectExp.sendReturn body
		     val blocks = Vector.fromList blocks
		  in
		     val _ = List.push
		             (newFunctions,
			      shrink (Function.new {name = name,
						    args = args,
						    start = start,
						    blocks = blocks,
						    returns = returns}))
		  end
	       in
		  name
	       end
      and vectorEqualFunc (ty: Type.t): Func.t =
	 case getVectorEqualFunc ty of
	    SOME f => f
	  | NONE =>
	       let
		  (* Build two functions, one that checks the lengths and the
		   * other that loops.
		   *)
		  val name = Func.newString "vectorEqual"
		  val _ = setVectorEqualFunc (ty, SOME name)
		  val loop = Func.newString "vectorEqualLoop"

		  val vty = Type.vector ty
		  local
		     val v1 = (Var.newNoname (), vty)
		     val v2 = (Var.newNoname (), vty)
		     val args = Vector.new2 (v1, v2)

		     val dv1 = DirectExp.var v1
		     val dv2 = DirectExp.var v2
		      
		     val body =
		        let
			  fun length x =
			     DirectExp.primApp
			     {prim = Prim.vectorLength,
			      targs = Vector.new1 ty,
			      args = Vector.new1 x,
			      ty = Type.int}
			in
			  DirectExp.name
			  (length dv1, fn lv1 =>
			   DirectExp.name
			   (length dv2, fn lv2 =>
			    let
			       val dlv1 = DirectExp.var (lv1, Type.int)
			       val dlv2 = DirectExp.var (lv2, Type.int)
			    in
			       DirectExp.conjoin
			       (DirectExp.eq (dlv1, dlv2, Type.int),
				DirectExp.call
				{func = loop,
				 args = Vector.new4 
				 (DirectExp.int 0, dlv1, dv1, dv2),
				 ty = Type.bool})
			    end))
			end
		     val (start, blocks) = DirectExp.sendReturn body
		     val blocks = Vector.fromList blocks
		  in
		     val _ = List.push
		             (newFunctions,
			      shrink (Function.new {name = name,
						    args = args,
						    start = start,
						    blocks = blocks,
						    returns = returns}))
		  end

		  local
		     val i = (Var.newNoname (), Type.int)
		     val len = (Var.newNoname (), Type.int)
		     val v1 = (Var.newNoname (), vty)
		     val v2 = (Var.newNoname (), vty)
		     val args = Vector.new4 (i, len, v1, v2)
		       
		     val di = DirectExp.var i
		     val dlen = DirectExp.var len
		     val dv1 = DirectExp.var v1
		     val dv2 = DirectExp.var v2

		     val body =
		        let
			   fun sub (v, i) =
			      DirectExp.primApp {prim = Prim.vectorSub,
						 targs = Vector.new1 ty,
						 args = Vector.new2 (v, i),
						 ty = ty}
			in
			   DirectExp.disjoin 
			   (DirectExp.eq (di, dlen, Type.int),
			    DirectExp.name
			    (sub (dv1, di), fn v1i =>
			     DirectExp.name
			     (sub (dv2, di), fn v2i =>
			      DirectExp.conjoin
			      (equal (v1i, v2i, ty),
			       DirectExp.call
			       {func = loop,
				args = Vector.new4 
				       (DirectExp.add (di, DirectExp.int 1),
					dlen, dv1, dv2),
			        ty = Type.bool}))))
			end
		     val (start, blocks) = DirectExp.sendReturn body
		     val blocks = Vector.fromList blocks
		  in
		     val _ = List.push
		             (newFunctions,
			      shrink (Function.new {name = loop,
						    args = args,
						    start = start,
						    blocks = blocks,
						    returns = returns}))
		  end
	       in
		  name
	       end
      and equal (x1: Var.t, x2: Var.t, ty: Type.t): DirectExp.t =
	 let
	    val dx1 = DirectExp.var (x1, ty)
	    val dx2 = DirectExp.var (x2, ty)
	    fun prim (p, targs) =
	       DirectExp.primApp {prim = p,
				  targs = targs, 
				  args = Vector.new2 (dx1, dx2),
				  ty = Type.bool}
	    fun eq () = prim (Prim.eq, Vector.new1 ty)
	    fun hasConstArg () = #isConst (varInfo x1) orelse #isConst (varInfo x2)
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
	  | Type.Vector ty =>
	       DirectExp.call {func = vectorEqualFunc ty,
			       args = Vector.new2 (dx1, dx2),
			       ty = Type.bool}
	  | Type.Ref _ => eq ()
	  | Type.Datatype tycon =>
	       if isEnum tycon orelse hasConstArg ()
		  then eq ()
	       else DirectExp.call {func = equalFunc tycon,
				    args = Vector.new2 (dx1, dx2),
				    ty = Type.bool}
	  | Type.Tuple tys =>
	       let
		  val max = Vector.length tys - 1
		  (* test components i, i+1, ... *)
		  fun loop (i: int): DirectExp.t =
		     if i > max
		        then DirectExp.truee
		     else let
			     val ty = Vector.sub (tys, i)
			  in
			     DirectExp.name
			     (DirectExp.select {tuple = dx1, offset = i, ty = ty}, 
			      fn x1i =>
			      DirectExp.name
			      (DirectExp.select {tuple = dx2, offset = i, ty = ty},
			       fn x2i =>
			       DirectExp.conjoin 
			       (equal (x1i, x2i, ty), 
				loop (i + 1))))
			  end
	       in
		  loop 0
	       end
	  | _ => Error.bug "equal of strange type"
	 end


      fun loopBind (Statement.T {var, ty, exp}) =
	 let
	    fun const () = setVarInfo (valOf var, {isConst = true})
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


      fun doit blocks =
	 let
	    val _ =
	       Vector.foreach
	       (blocks, fn Block.T {statements, ...} =>
		Vector.foreach (statements, loopBind))
	    val blocks = 
	       Vector.fold
	       (blocks, [], 
		fn (Block.T {label, args, statements, transfer}, blocks) =>
		let
		   fun finish ({label, args, statements}, transfer) =
		      Block.T {label = label,
			       args = args,
			       statements = Vector.fromListRev statements,
			       transfer = transfer}
		   val (blocks, las) =
		      Vector.fold
		      (statements, 
		       (blocks, {label = label, args = args, statements = []}),
		       fn (stmt as Statement.T {var, ty, exp}, 
			   (blocks, las as {label, args, statements})) =>
		       let
			 fun normal () = (blocks,
					  {label = label,
					   args = args,
					   statements = stmt::statements})
		       in
			 case exp of
			    PrimApp {prim, targs, args, ...} =>
			       (case (Prim.name prim, Vector.length targs) of
				   (Prim.Name.MLton_equal, 1) =>
				      let
					 val ty = Vector.sub (targs, 0)
					 fun arg i = Vector.sub (args, i)
					 val l = Label.newNoname ()
					 val (start',bs') =
					    DirectExp.sendGoto
					    (equal (arg 0, arg 1, ty), l)
				      in
					(finish (las, 
						 Goto {dst = start',
						       args = Vector.new0 ()})
					 :: (bs' @ blocks),
					 {label = l,
					  args = Vector.new1 (valOf var, Type.bool),
					  statements = []})
				      end
				 | _ => normal ())
			  | _ => normal ()
		       end)
		in
		   finish (las, transfer)
		   :: blocks
		end)
	 in
	    Vector.fromList blocks
	 end

      val functions =
	 List.revMap 
	 (functions, fn f =>
	  let
	     val {name, args, start, blocks, returns} = Function.dest f
	  in
	     shrink (Function.new {name = name,
				   args = args,
				   start = start,
				   blocks = doit blocks,
				   returns = returns})
	  end)
      val program =
	 Program.T {datatypes = datatypes,
		    globals = globals,
		    functions = (!newFunctions) @ functions,
		    main = main}
      val _ = destroyType ()
      val _ = Program.clear program
   in
      program
   end

end
