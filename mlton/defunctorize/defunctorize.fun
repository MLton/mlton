functor Defunctorize (S: DEFUNCTORIZE_STRUCTS): DEFUNCTORIZE = 
struct

open S

local
   open CoreML
in
   structure Const = Const
   structure Cdec = Dec
   structure Cexp = Exp
   structure IntSize = IntSize
   structure Clambda = Lambda
   structure Cpat = Pat
   structure Prim = Prim
   structure Record = Record
   structure Ctype = Type
   structure WordSize = WordSize
end

structure IntX = Const.IntX
structure Field = Record.Field

local
   open Xml
in
   structure Xcases = Cases
   structure Con = Con
   structure Xdec = Dec
   structure Xexp = DirectExp
   structure Xlambda = Lambda
   structure Xpat = Pat
   structure XprimExp = PrimExp
   structure Tycon = Tycon
   structure Xtype = Type
   structure Tyvar = Tyvar
   structure Var = Var
   structure XvarExp = VarExp
end

structure Region =
   struct
      open Region

      fun toFilePos r = Option.map (left r, SourcePos.toString)
   end

structure NestedPat = NestedPat (open Xml)
structure MatchCompile =
   MatchCompile (open CoreML
		 structure Type = Xtype
		 structure NestedPat = NestedPat
		 structure Cases =
		    struct
		       type exp = Xexp.t

		       open Xcases
		       type t = exp t
		       val int = Int
		       val word = Word
		       fun con v =
			  Con (Vector.map
			       (v, fn {con, targs, arg, rhs} =>
				(Xpat.T {con = con,
					 targs = targs,
					 arg = arg},
				 rhs)))
		    end
		structure Exp =
		   struct
		      open Xexp
		      val lett = let1
		      val var = monoVar

		      fun detuple {tuple, body} =
			 Xexp.detuple
			 {tuple = tuple,
			  body = fn xts => body (Vector.map
						 (xts, fn (x, t) =>
						  (XvarExp.var x, t)))}
		   end)

structure Xexp =
   struct
      open Xexp
	 
      local
	 fun exn (c: Con.t): Xexp.t =
	    conApp {arg = NONE,
		    con = c,
		    targs = Vector.new0 (),
		    ty = Xtype.exn}
      in
	 val bind = exn Con.bind
	 val match = exn Con.match
      end
   end

fun casee {caseType: Xtype.t,
	   cases: (NestedPat.t * Xexp.t) vector,
	   conTycon,
	   noMatch,
	   region: Region.t,
	   test = (test: Xexp.t, testType: Xtype.t),
	   tyconCons}: Xexp.t =
   let
      fun raiseExn f =
	 let
	    val e = Var.newNoname ()
	 in
	    Vector.concat
	    [cases,
	     Vector.new1 (NestedPat.make (NestedPat.Var e, testType),
			  Xexp.raisee ({exn = f e,
					filePos = Region.toFilePos region},
				       caseType))]
	 end
      val cases =
	 let
	    datatype z = datatype Cexp.noMatch
	 in
	    case noMatch of
	       Impossible => cases
	     | RaiseAgain => raiseExn (fn e => Xexp.monoVar (e, Xtype.exn))
	     | RaiseBind => raiseExn (fn _ => Xexp.bind)
	     | RaiseMatch => raiseExn (fn _ => Xexp.match)
	 end
      fun matchCompile () =		     		     
	 let
	    val (cases, decs) =
	       Vector.mapAndFold
	       (cases, [], fn ((p: NestedPat.t, e: Xexp.t), decs) =>
		let
		   val args = Vector.fromList (NestedPat.varsAndTypes p)
		   val (vars, tys) = Vector.unzip args
		   val func = Var.newNoname ()
		   val arg = Var.newNoname ()
		   val argType = Xtype.tuple tys
		   val funcType = Xtype.arrow (argType, caseType)
		   val dec =
		      Xdec.MonoVal
		      {var = func,
		       ty = funcType,
		       exp =
		       XprimExp.Lambda
		       (Xlambda.make
			{arg = arg,
			 argType = argType,
			 body =
			 Xexp.toExp
			 (Xexp.detupleBind
			  {tuple = Xexp.monoVar (arg, argType),
			   components = vars,
			   body = e})})}
		   fun finish rename =
		      Xexp.app
		      {func = Xexp.monoVar (func, funcType),
		       arg =
		       Xexp.tuple {exps = (Vector.map
					   (args, fn (x, t) =>
					    Xexp.monoVar (rename x, t))),
				   ty = argType},
		       ty = caseType}
		in ((p, finish), dec :: decs)
		end)
	    val testVar = Var.newNoname ()
	 in
	    Xexp.let1
	    {var = testVar,
	     exp = test,
	     body = 
	     Xexp.lett
	     {decs = decs,
	      body = MatchCompile.matchCompile {caseType = caseType,
						cases = cases,
						conTycon = conTycon,
						region = region,
						test = testVar,
						testType = testType,
						tyconCons = tyconCons}}}
	 end
      datatype z = datatype NestedPat.node
      fun lett (x, e) = Xexp.let1 {var = x, exp = test, body = e}
      fun wild e = lett (Var.newNoname (), e)
      fun normal () =
	 if Vector.isEmpty cases
	    then Error.bug "case with no patterns"
	 else
	    let
	       val (p, e) = Vector.sub (cases, 0)
	    in
	       case NestedPat.node p of
		  Wild => wild e
		| Var x => lett (x, e)
		| Tuple ps =>
		     if Vector.forall (ps, NestedPat.isVar)
			then
			   (* It's a flat tuple pattern.
			    * Generate the selects.
			    *)
			   let
			      val t = Var.newNoname ()
			      val tuple = XvarExp.mono t
			      val tys = Xtype.deTuple testType
			      val (_, decs) =
				 Vector.fold2
				 (ps, tys, (0, []),
				  fn (p, ty, (i, decs)) =>
				  case NestedPat.node p of
				     Var x =>
					(i + 1,
					 Xdec.MonoVal
					 {var = x,
					  ty = ty,
					  exp = (XprimExp.Select
						 {tuple = tuple,
						  offset = i})}
					 :: decs)
				   | _ => Error.bug "infer flat tuple")
			   in Xexp.let1 {var = t, exp = test,
					 body = Xexp.lett {decs = decs,
							   body = e}}
			   end
		     else matchCompile ()
				   | _ => matchCompile ()
	    end
				 fun make (ac, default) =
				    Xexp.casee {test = test,
						default = default,
						ty = caseType,
						cases = Xcases.Con (Vector.fromList ac)}
				 fun step (_, (p, e), ac) =
				    case NestedPat.node p of
				       NestedPat.Wild =>
					  Vector.Done
					  (case ac of
					      [] => wild e
					    | _ => make (ac, SOME (e, region)))
				     | _ => Vector.Done (normal ())
				 fun done ac = make (ac, NONE)
   in
      Vector.fold' (cases, 0, [], step, done)
   end

val casee =
   Trace.trace ("Defunctorize.casee",
		Region.layout o #region,
		Xml.Exp.layout o Xexp.toExp)
   casee

fun 'a sortByField (v: (Field.t * 'a) vector): 'a vector =
   Vector.map (QuickSort.sortVector (v, fn ((f, _), (f', _)) =>
				     Field.<= (f, f')),
	       #2)

fun valDec (tyvars: Tyvar.t vector,
	    x: Var.t,
	    e: Xexp.t,
	    et: Xtype.t,
	    e': Xexp.t): Xexp.t =
   Xexp.lett {body = e',
	      decs = [Xdec.PolyVal {exp = Xexp.toExp e,
				    ty = et,
				    tyvars = tyvars,
				    var = x}]}

fun defunctorize (CoreML.Program.T {decs}) =
   let
      val loopTy = Ctype.hom {con = fn (c, ts) => if Tycon.equals (c, Tycon.char)
						     then Xtype.word8
						  else Xtype.con (c, ts),
			      var = Xtype.var}
      val {get = conTycon, set = setConTycon, ...} =
	 Property.getSetOnce (Con.plist,
			      Property.initRaise ("conTycon", Con.layout))
      val {get = tyconCons: Tycon.t -> Con.t vector,
	   set = setTyconCons, ...} =
	 Property.getSetOnce (Tycon.plist,
			      Property.initRaise ("tyconCons", Tycon.layout))
      val setConTycon =
	 Trace.trace2 ("setConTycon", Con.layout, Tycon.layout, Unit.layout)
	 setConTycon
      val datatypes = ref []
      (* Process all the datatypes. *)
      fun loopDec (d: Cdec.t) =
	 let
(*	    datatype z = datatype Cdec.t *)
	    open Cdec
	 in
	    case d of
	       Datatype dbs =>
		  Vector.foreach
		  (dbs, fn {cons, tycon, tyvars} =>
		   let
		      val _ = setTyconCons (tycon, Vector.map (cons, #con))
		      val cons =
			 Vector.map
			 (cons, fn {arg, con} =>
			  (setConTycon (con, tycon)
			   ; {arg = Option.map (arg, loopTy),
			      con = con}))
		      val _ = 
			 if Tycon.equals (tycon, Tycon.reff)
			    then ()
			 else
			    List.push (datatypes, {cons = cons,
						   tycon = tycon,
						   tyvars = tyvars})
		   in
		      ()
		   end)
	     | Exception {con, ...} => setConTycon (con, Tycon.exn)
	     | Fun {decs, ...} => Vector.foreach (decs, loopLambda o #lambda)
	     | Val {rvbs, vbs, ...} =>
		  (Vector.foreach (rvbs, loopLambda o #lambda)
		   ; Vector.foreach (vbs, loopExp o #exp))
	 end
      and loopExp (e: Cexp.t): unit =
	 let
	    datatype z = datatype Cexp.node
	 in
	    case Cexp.node e of
	       App (e, e') => (loopExp e; loopExp e')
	     | Case {rules, test, ...} =>
		  (loopExp test
		   ; Vector.foreach (rules, loopExp o #2))
	     | Con _ => ()
	     | Const _ => ()
	     | EnterLeave (e, _) => loopExp e
	     | Handle {handler, try, ...} => (loopExp handler; loopExp try)
	     | Lambda l => loopLambda l
	     | Let (ds, e) => (Vector.foreach (ds, loopDec); loopExp e)
	     | List es => Vector.foreach (es, loopExp)
	     | PrimApp {args, ...} => Vector.foreach (args, loopExp)
	     | Raise {exn, ...} => loopExp exn
	     | Record r => Record.foreach (r, loopExp)
	     | Seq es => Vector.foreach (es, loopExp)
	     | Var _ => ()
	 end
      and loopLambda (l: Clambda.t): unit =
	 loopExp (#body (Clambda.dest l))
      fun loopPat (p: Cpat.t): NestedPat.t =
	 let
	    val (p, t) = Cpat.dest p
	    val t' = loopTy t
	    datatype z = datatype Cpat.node
	    val p = 
	       case p of
		  Con {arg, con, targs} =>
		     NestedPat.Con {arg = Option.map (arg, loopPat),
				    con = con,
				    targs = Vector.map (targs, loopTy)}
		| Const f => NestedPat.Const (f ())
		| Layered (x, p) => NestedPat.Layered (x, loopPat p)
		| List ps =>
		     let
			val targs = Vector.map (#2 (valOf (Ctype.deConOpt t)),
						loopTy)
		     in
			Vector.foldr
			(ps,
			 NestedPat.Con {arg = NONE,
					con = Con.nill,
					targs = targs},
			 fn (p, np) =>
			 NestedPat.Con {arg = SOME (NestedPat.tuple
						    (Vector.new2
						     (loopPat p,
						      NestedPat.make (np, t')))),
					con = Con.cons,
					targs = targs})
		     end
		| Record r =>
		     NestedPat.Tuple
		     (Vector.map
		      (Ctype.deRecord t, fn (f, t: Ctype.t) =>
		       case Record.peek (r, f) of
			  NONE => NestedPat.make (NestedPat.Wild, loopTy t)
			| SOME p => loopPat p))
		| Tuple ps => NestedPat.Tuple (Vector.map (ps, loopPat))
		| Var x => NestedPat.Var x
		| Wild => NestedPat.Wild
	 in
	    NestedPat.make (p, t')
	 end
      val _ = Vector.foreach (decs, loopDec)
      (* Now, do the actual defunctorization. *)
      fun loopDec (d: Cdec.t, e: Xexp.t, et: Xtype.t): Xexp.t =
	 let
	    fun prefix (d: Xdec.t) =
	       Xexp.lett {decs = [d], body = e}
	    fun processLambdas v =
	       Vector.map
	       (v, fn {lambda, var} =>
		let
		   val {arg, argType, body, bodyType} = loopLambda lambda
		in
		   {lambda = Xlambda.make {arg = arg,
					   argType = argType,
					   body = Xexp.toExp body},
		    ty = Xtype.arrow (argType, bodyType),
		    var = var}
		end)
(* Use open Cdec instead of the following due to an SML/NJ 110.43 bug *)
(*	    datatype z = datatype Cdec.t *)
	    open Cdec
	 in
	    case d of
	       Datatype _ => e
	     | Exception {arg, con} =>
		  prefix (Xdec.Exception {arg = Option.map (arg, loopTy),
					  con = con})
	     | Fun {decs, tyvars} =>
		  prefix (Xdec.Fun {decs = processLambdas decs,
				    tyvars = tyvars ()})
	     | Val {rvbs, tyvars, vbs} =>
	       let
		  val tyvars = tyvars ()
		  val bodyType = et
		  fun patDec (p: NestedPat.t,
			      e: Xexp.t,
			      r: Region.t,
			      body: Xexp.t,
			      bodyType: Xtype.t) =
		     casee {caseType = bodyType,
			    cases = Vector.new1 (p, body),
			    conTycon = conTycon,
			    noMatch = Cexp.RaiseBind,
			    region = r,
			    test = (e, NestedPat.ty p),
			    tyconCons = tyconCons}
		  val e =
		     Vector.foldr
		     (vbs, e, fn ({exp, pat, patRegion}, e) =>
		      let
			 val (exp, expType) = loopExp exp
			 val pat = loopPat pat
			 fun vd (x: Var.t) = valDec (tyvars, x, exp, expType, e)
		      in
			 if Vector.isEmpty tyvars
			    then patDec (pat, exp, patRegion, e, bodyType)
			 else
			    case NestedPat.node pat of
			       NestedPat.Wild => vd (Var.newNoname ())
			     | NestedPat.Var x => vd x
			     | _ =>
				  (* Polymorphic pattern.
				   *  val 'a Foo (y1, y2) = e
				   * Expands to
				   *  val 'a x = e
				   *  val Foo _ = x
				   *  val 'a y1 = case x of Foo (y1', _) => y1'
				   *  val 'a y2 = case x of Foo (_, y2') => y2'
				   *)
				  let
				     val x = Var.newNoname ()
				     val xt = expType
				     val targs = Vector.map (tyvars, Xtype.var)
				     val e =
					List.fold
					(NestedPat.varsAndTypes pat, e,
					 fn ((y, yt), e) =>
					 let
					    val y' = Var.new y
					    val pat =
					       NestedPat.removeOthersReplace
					       (pat, {old = y, new = y'})
					 in
					    valDec
					    (tyvars,
					     y,
					     patDec (pat,
						     Xexp.var {targs = targs,
							       ty = xt,
							       var = x},
						     patRegion,
						     Xexp.monoVar (y', yt),
						     yt),
					     yt,
					     e)
					 end)
				     fun instantiatePat () =
					let
					   val pat = NestedPat.removeVars pat
					   fun con (_, c, ts) = Xtype.con (c, ts)
					   fun var (t, a) =
					      if (Vector.exists
						  (tyvars, fn a' =>
						   Tyvar.equals (a, a')))
						 then Xtype.unit
					      else t
					   val {destroy, hom} =
					      Xtype.makeHom {con = con,
							     var = var}
					   val pat =
					      NestedPat.replaceTypes
					      (pat, hom)
					   val _ = destroy ()
					in
					   pat
					end
				     val e =
					if NestedPat.isRefutable pat
					   then
					       let
						  val targs =
						     Vector.map (tyvars, fn _ =>
								 Xtype.unit)
						  val pat = instantiatePat ()
					       in
						  patDec
						  (pat,
						   Xexp.var
						   {targs = targs,
						    ty = NestedPat.ty pat,
						    var = x},
						   patRegion,
						   e,
						   bodyType)
					       end
					else e
				  in
				     valDec (tyvars, x, exp, expType, e)
				  end
		      end)
	       in
		  if 0 = Vector.length rvbs
		     then e
		  else
		     Xexp.lett {decs = [Xdec.Fun {decs = processLambdas rvbs,
						  tyvars = tyvars}],
				body = e}
	       end
	 end
      and loopDecs (ds: Cdec.t vector, (e: Xexp.t, t: Xtype.t)): Xexp.t =
         Vector.foldr (ds, e, fn (d, e) => loopDec (d, e, t))
      and loopExp (e: Cexp.t): Xexp.t * Xtype.t =
	 let
	    val (n, ty) = Cexp.dest e
	    val ty = loopTy ty
	    fun conApp {arg, con, targs, ty} =
	       if Con.equals (con, Con.reff)
		  then Xexp.primApp {args = Vector.new1 arg,
				     prim = Prim.reff,
				     targs = targs,
				     ty = ty}
	       else Xexp.conApp {arg = SOME arg,
				 con = con,
				 targs = targs,
				 ty = ty}
	    datatype z = datatype Cexp.node
	    val exp =
	       case n of
		  App (e1, e2) =>
		     let
			val (e2, _) = loopExp e2
		     in
			case Cexp.node e1 of
			   Con (con, targs) =>
			      conApp {arg = e2,
				      con = con,
				      targs = Vector.map (targs, loopTy),
				      ty = ty}
			 | _ => 
			      Xexp.app {arg = e2,
					func = #1 (loopExp e1),
					ty = ty}
		     end
		| Case {noMatch, region, rules, test} =>
		     casee {caseType = ty,
			    cases = Vector.map (rules, fn (pat, exp) =>
						(loopPat pat,
						 #1 (loopExp exp))),
			    conTycon = conTycon,
			    noMatch = noMatch,
			    region = region,
			    test = loopExp test,
			    tyconCons = tyconCons}
		| Con (con, targs) =>
		     let
			val targs = Vector.map (targs, loopTy)
		     in
			case Xtype.deArrowOpt ty of
			   NONE =>
			      Xexp.conApp {arg = NONE,
					   con = con,
					   targs = targs,
					   ty = ty}
			 | SOME (argType, bodyType) =>
			      let
				 val arg = Var.newNoname ()
			      in
				 Xexp.lambda
				 {arg = arg,
				  argType = argType,
				  body = (conApp
					  {arg = Xexp.monoVar (arg, argType),
					   con = con,
					   targs = targs,
					   ty = bodyType}),
				  bodyType = bodyType}
			      end
		     end
		| Const f =>
		     let
			val c = f ()
		     in
			if Xtype.equals (ty, Xtype.bool)
			   then
			      (case c of
				  Const.Int i =>
				     if 0 = IntX.toInt i
					then Xexp.falsee ()
				     else Xexp.truee ()
				| _ => Error.bug "strange boolean constant")
			else Xexp.const c
		     end
		| EnterLeave (e, si) =>
		     let
			val (e, t) = loopExp e
		     in
			Xexp.fromExp (Xml.Exp.enterLeave (Xexp.toExp e, t, si),
				      t)
		     end
		| Handle {catch = (x, t), handler, try} =>
		     Xexp.handlee {catch = (x, loopTy t),
				   handler = #1 (loopExp handler),
				   try = #1 (loopExp try),
				   ty = ty}
		| Lambda l => Xexp.lambda (loopLambda l)
		| Let (ds, e) => loopDecs (ds, loopExp e)
		| List es =>
		     let
			val targs = #2 (valOf (Xtype.deConOpt ty))
			val eltTy = Vector.sub (targs, 0)
		     in
			Vector.foldr
			(es,
			 Xexp.conApp {arg = NONE,
				      con = Con.nill,
				      targs = targs,
				      ty = ty},
			 fn (e, l) =>
			 Xexp.conApp
			 {arg = (SOME
				 (Xexp.tuple
				  {exps = Vector.new2 (#1 (loopExp e), l),
				   ty = Xtype.tuple (Vector.new2 (eltTy, ty))})),
			  con = Con.cons,
			  targs = targs,
			  ty = ty})
		     end
		| PrimApp {args, prim, targs} =>
		     let
			val args = Vector.map (args, #1 o loopExp)
			val targs = Vector.map (targs, loopTy)
			fun app prim =
			   Xexp.primApp {args = args,
					 prim = prim,
					 targs = targs,
					 ty = ty}
			fun id () = Vector.sub (args, 0)
			datatype z = datatype Prim.Name.t
			datatype z = datatype WordSize.t
		     in
			case Prim.name prim of
			   C_CS_charArrayToWord8Array => id ()
			 | Char_chr =>
			      app (Prim.intToWord (IntSize.default, W8))
			 | Char_ge => app (Prim.wordGe W8)
			 | Char_gt => app (Prim.wordGt W8)
			 | Char_le => app (Prim.wordLe W8)
			 | Char_lt => app (Prim.wordLt W8)
			 | Char_ord =>
			      app (Prim.wordToInt (W8, IntSize.default))
			 | Char_toWord8 => id ()
			 | String_toWord8Vector => id ()
			 | Word8_toChar => id ()
			 | Word8Vector_toString => id ()
			 | _ => app prim
		     end
		| Raise {exn, region} =>
		     Xexp.raisee ({exn = #1 (loopExp exn),
				   filePos = Region.toFilePos region},
				  ty)
		| Record r =>
		     (* The components of the record have to be evaluated left to 
		      * right as they appeared in the source program, but then
		      * ordered according to sorted field name within the tuple.
		      *)
		     let
			val fes = Record.toVector r
		     in
			Xexp.seq
			(Vector.map (fes, #1 o loopExp o #2), fn es =>
			 Xexp.tuple {exps = (sortByField
					     (Vector.map2
					      (fes, es, fn ((f, _), e) => (f, e)))),
				     ty = ty})
		     end
		| Seq es => Xexp.sequence (Vector.map (es, #1 o loopExp))
		| Var (var, targs) =>
		     Xexp.var {targs = Vector.map (targs (), loopTy),
			       ty = ty,
			       var = var ()}
	 in
	    (exp, ty)
	 end
      and loopLambda (l: Clambda.t) =
	 let
	    val {arg, argType, body} = Clambda.dest l
	    val (body, bodyType) = loopExp body
	 in
	    {arg = arg,
	     argType = loopTy argType,
	     body = body,
	     bodyType = bodyType}
	 end
      val body = loopDecs (decs, (Xexp.unit (), Xtype.unit))
   in
      Xml.Program.T {body = Xexp.toExp body,
		     datatypes = Vector.fromList (!datatypes),
		     overflow = NONE}
   end

end
