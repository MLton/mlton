(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Infer (S: INFER_STRUCTS): INFER =
struct
      
open S

open CoreML.Atoms

datatype z = datatype WordSize.t
   
structure Srecord = SortedRecord
structure Field = Record.Field
structure Env = TypeEnv (open CoreML
			 structure XmlType = Xml.Type)
structure Scheme = Env.InferScheme
structure Type = Env.Type
structure PScheme = Prim.Scheme

local
   open Ast
in
   structure Aconst = Const
end
   
local open CoreML
in structure Cdec = Dec
   structure Cexp = Exp
   structure Cmatch = Match
   structure Ctype = Type
   structure Cpat = Pat
   structure Cprogram = Program
end

local open Xml
in
   structure Xcases = Cases
   structure Xdec = Dec
   structure Xexp = DirectExp
   structure Xtype = Type
   structure Xconst = Const
   structure Xlambda = Lambda
   structure Xpat = Pat
   structure XprimExp = PrimExp
   structure XvarExp = VarExp
end

structure BuildConst =
   struct
      datatype t =
	 Bool of bool
       | Int of int
   end

structure Type =
   struct
      open Type

      fun fromCoreML t =
	 case t of
	    Ctype.Var a => var a
	  | Ctype.Con (c, ts) => con (c, Vector.map (ts, fromCoreML))
	  | Ctype.Record r => record {flexible = false,
				      record = Srecord.map (r, fromCoreML),
				      region = Region.bogus}
   end

fun instantiatePrim (PScheme.T {tyvars, ty}, region) =
   Scheme.instantiate (Scheme.make {canGeneralize = true,
				    tyvars = tyvars,
				    ty = Type.fromCoreML ty},
		       region)

structure VarRange = Env.VarRange

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

type patCode = (unit -> NestedPat.node) * Type.t * Region.t
type expCode = (unit -> Xexp.t) * Type.t * Region.t

(* decCode can't quite be of type  unit -> Xdec.t list
 * because expanding val p = e requires nesting the remaining declarations
 * inside a case expression
 *)
type decCode = Xexp.t * Xtype.t -> Xexp.t * Xtype.t

type ruleCode = patCode * expCode
type rulesCode = ruleCode list

fun finishPat ((p, ty, r): patCode): NestedPat.t =
   NestedPat.new (p (), Type.toXml (ty, r))
   
fun finishExp ((e, ty, _): expCode): Xexp.t = e ()
   
local
   open Layout
in
   fun layoutPatCode ((_, t, _): patCode) = seq [str "Patcode ", Type.layout t]
   fun layoutExpCode ((_, t, _): expCode) = seq [str "Expcode ", Type.layout t]
   fun layoutDecCode (_: decCode) = str "<dec>"
end

fun newType () = Type.new {equality = false,
			   canGeneralize = true}

(* Warning:
 * stringToIntInf will raise the Subscript exception on inputs "" and "~". 
 * This code is messier than it needs to be because it works around a bug in the 
 * SML/NJ implementation of IntInf.scan StringCvt.HEX.  In particular, it handles
 * the ~ sign and possible 0x itself.
 *)
fun stringToIntInf (str: string): IntInf.t =
   let
      val size = String.size str
      fun reader offset =
	 if offset = size
	    then NONE
	 else SOME (String.sub (str, offset), offset + 1)
      val (start, negate) =
	 if String.sub (str, 0) = #"~"
	    then (1, true)
	 else (0, false)
      (* We carefully avoid calling IntInf.scan on the 0x in a hex constant
       * because of an SML/NJ bug.
       *)
      val (base, start) =
	 if (String.sub (str, start) = #"0"
	     andalso (case reader (start + 1) of
			 SOME (#"x", next) => true
		       | _ => false))
	    then (StringCvt.HEX, start + 2)
	 else (StringCvt.DEC, start)
      val (v, _) = valOf (IntInf.scan (base, reader) start)
      val v = if negate then IntInf.~ v else v
   in
      v
   end

fun makeXconst (c: Aconst.t, ty: Type.t): Xconst.t =
   let
      fun error m =
	 Control.error (Aconst.region c,
			Layout.str (concat [m, ": ", Aconst.toString c]),
			Layout.empty)
      val ty = Type.toXml (ty, Aconst.region c)
      fun choose (all, sizeTy, name, make) =
	 case List.peek (all, fn s => Xtype.equals (ty, sizeTy s)) of
	    NONE => Error.bug (concat ["strange ", name, " type: ",
				       Layout.toString (Xtype.layout ty)])
	  | SOME s => make s
   in
      case Aconst.node c of
	 Aconst.Char c =>
	    Xconst.Word (WordX.make (LargeWord.fromChar c, WordSize.W8))
       | Aconst.Int i =>
	    if Xtype.equals (ty, Xtype.intInf)
	       then Xconst.IntInf i
	    else
	       choose (IntSize.all, Xtype.int, "int", fn s =>
		       Xconst.Int
		       (IntX.make (i, s)
			handle Overflow =>
			   (error (concat [Xtype.toString ty, " too big"])
			    ; IntX.zero s)))
       | Aconst.Real r =>
	    choose (RealSize.all, Xtype.real, "real", fn s =>
		    Xconst.Real (RealX.make (r, s)))
       | Aconst.String s => Xconst.string s
       | Aconst.Word w =>
	    choose (WordSize.all, Xtype.word, "word", fn s =>
		    Xconst.Word
		    (if IntInf.<= (w, LargeWord.toIntInf (WordSize.max s))
			then WordX.fromLargeInt (w, s)
		     else (error (concat [Xtype.toString ty, " too big"])
			   ; WordX.zero s)))
   end

fun 'a sortByField (v: (Field.t * 'a) vector): 'a vector =
   Vector.map (QuickSort.sortVector (v, fn ((f, _), (f', _)) =>
				     Field.<= (f, f')),
	       #2)

(*---------------------------------------------------*)
(*                       infer                       *)
(*---------------------------------------------------*)

local
   structure Env =
      struct
	 open Env
	 val layout: t -> Layout.t = Layout.ignore
      end
in 
   val traceInferExp =
      Trace.trace2 ("inferExp", Cexp.layout, Env.layout, layoutExpCode)

   val traceInferDec =
      Trace.trace2 ("inferDec", Cdec.layout, Env.layout,
		   Layout.tuple2 (layoutDecCode, Env.layout))

   val ll = List.layout (Layout.tuple2 (Var.layout, Type.layout))
   val traceInferPat =
      Trace.trace2 ("inferPat", Cpat.layout, ll,
		    Layout.tuple2 (layoutPatCode, ll))
end

fun infer {program = p: CoreML.Program.t,
	   lookupBuildConstant, lookupConstant}: Xml.Program.t =
   let
      val matchCompileCalls = ref 0
      val {get = tyconCons: Tycon.t -> Con.t vector,
	   set = setTyconCons, destroy = destroyTycon} =
	 Property.destGetSetOnce (Tycon.plist,
				  Property.initRaise ("tyconCons", Tycon.layout))
      val {get = conInfo: Con.t -> {tycon: Tycon.t,
				    scheme: PScheme.t},
	   set = setConInfo, destroy = destroyCon} =
	 Property.destGetSetOnce (Con.plist,
				  Property.initRaise ("conInfo", Con.layout))
      val conTycon = #tycon o conInfo
      val conScheme = #scheme o conInfo
      val conTycon = Trace.trace ("conTycon", Con.layout, Tycon.layout) conTycon
      val conScheme =
	 Trace.trace ("conScheme", Con.layout, PScheme.layout) conScheme
      fun instCon c = instantiatePrim (conScheme c, Region.bogus)
      (*---------------------------------------------------*)
      (*                     inferPat                      *)
      (*---------------------------------------------------*)
      (*
       * inferPat yields
       * - a thunk that can generate a typed nested pattern
       * - the type of the pattern
       * - a list of variables in the pattern and their types
       *)
      fun inferPat arg: patCode * (Var.t * Type.t) list =
	 traceInferPat
	 (fn (p: Cpat.t, ac) =>
	  let
	     val region = Cpat.region p
	     fun make ((p, t), ac) = ((p, t, region), ac)
	  in
	     case Cpat.node p of
		Cpat.Wild =>
		   make ((fn () => NestedPat.Wild, newType ()), ac)
	      | Cpat.Var x =>
		   make (let val t = newType ()
			 in ((fn () => NestedPat.Var x, t),
			     (x, t) :: ac)
			 end)
	      | Cpat.Const c =>
		   make
		   (let
		       val ty = Type.ofConst c
		    in
		       ((fn () => NestedPat.Const (makeXconst (c, ty)),
			 ty),
			ac)
		    end)
	      | Cpat.Con {con, arg} =>
		   make
		   (let
		       val {instance, args} = instCon con
		    in
		       case arg of
			  NONE => ((fn () => NestedPat.Con {con = con,
							    targs = args (),
							    arg = NONE},
				    instance),
				   ac)
			| SOME p => 
			     let
				val r = Cpat.region p
				val (t1, t2) =
				   case Type.dearrowOpt instance of
				      SOME z => z
				    | NONE =>
					 let
					    open Layout
					    val _ =
					       Control.error
					       (region,
						str "constant constructor applied to argument in pattern",
						empty)
					    fun new () =
					       Type.new {equality = false,
							 canGeneralize = true}
					 in
					    (new (), new ())
					 end
				val (p as (_, tp, _), ac) = inferPat (p, ac)
				val _ = Type.unify (t1, tp, r)
			     in
				((fn () => (NestedPat.Con
					    {con = con,
					     targs = args (),
					     arg = SOME (finishPat p)}),
				  t2),
				 ac)
			     end
		    end)
	      | Cpat.Record {flexible, record} =>
		   let
		      val (fs, ps) = Record.unzip record
		      val (ps, ac) = Vector.mapAndFold (ps, ac, inferPat)
		      val t =
			 Type.record
			 {flexible = flexible,
			  record = Srecord.zip (fs, Vector.map (ps, #2)),
			  region = region}
		   in
		      make
		      ((fn () =>
			NestedPat.Tuple
			(if not flexible
			    then sortByField (Vector.map2
					      (fs, ps, fn (f, p) =>
					       (f, finishPat p)))
			 else
			    let
			       val record = Record.zip (fs, ps)
			    in Vector.map
			       (Type.derecord (t, region), fn (f, t) =>
				case Record.peek (record, f) of
				   NONE => NestedPat.new (NestedPat.Wild, t)
				 | SOME p => finishPat p)
			    end),
			    t),
		       ac)
		   end
	      | Cpat.Constraint (p, t') =>
		   let
		      val r = Cpat.region p
		      val ((p, t, _), ac) = inferPat (p, ac)
		      val _ = Type.unify (t, Type.fromCoreML t', r)
		   in
		      make ((p, t), ac)
		   end
	      | Cpat.Layered (x, p) =>
		   let
		      val (p as (_, t, _), ac) = inferPat (p, ac)
		   in
		      make
		      ((fn () => NestedPat.Layered (x, finishPat p), t),
		       (x, t) :: ac)
		   end
	  end) arg
      fun delayExp (e as (_, t, r): expCode): expCode =
	 (fn () => Xexp.lambda {arg = Var.newNoname (),
				argType = Xtype.unit,
				body = finishExp e,
				bodyType = Type.toXml (t, r)},
	  Type.arrow (Type.unit, t),
	  r)
      fun letExp (d: decCode, e as (_, t, r): expCode): expCode =
	 (fn () => #1 (d (finishExp e, Type.toXml (t, r))), t, r)

      local
	 fun make (c: Con.t) (): Xexp.t =
	    Xexp.conApp {con = c, targs = Vector.new0 (),
			 arg = NONE, ty = Xtype.exn}
      in val match = make Con.match
	 val bind = make Con.bind
      end

      (* store all the overloads as promises in a list, so that we can
       * force them after performing the unifications.  This is necessary because
       * forcing an overload can cause further unifications to be done, and all
       * unifications must be done before converting to Xml
       *)
      val overloads: (unit -> Xexp.t) list ref = ref []

      (* Helps to find type errors. *)
      val currentFunction: Var.t list ref = ref []

      fun varExp (x: Var.t, env: Env.t, r: Region.t): expCode =
	 let
	    val vr as VarRange.T {scheme, kind} = Env.lookupVarRange (env, x)
	 in
	    case kind of
	       VarRange.Normal =>
		  let
		     val {instance, args} = Scheme.instantiate (scheme, r)
		  in (fn () =>
		      let
			 val args = args ()
		      in
			 Xexp.var {var = x,
				   targs = args,
				   ty = Type.toXml (instance, r)}
		      end,
		      instance, r)
		  end
	     | VarRange.Recursive targs =>
		  let
		     val ty = Scheme.ty scheme
		  in
		     (fn () => Xexp.var {var = x,
					 targs = targs (),
					 ty = Type.toXml (ty, r)},
		      ty, r)
		  end
	     | VarRange.Delayed =>
		  let
		     val {instance, args} = Scheme.instantiate (scheme, r)
		     val (_, t') = Type.dearrow instance
		  in
		     (fn () =>
		      Xexp.app
		      {func = Xexp.var {var = x,
					targs = args (),
					ty = Xtype.arrow (Xtype.unit,
							  Type.toXml (t', r))},
		       arg = Xexp.unit (),
		       ty = Type.toXml (t', r)},
		      t', r)
		  end
	     | VarRange.Overload yts =>
		  let
		     val {instance, args} = Scheme.instantiate (scheme, r)
		     val promise =
			Promise.lazy
			(fn () =>
			 Vector.loop
			 (yts,
			  fn (y, t) =>
			  if Type.canUnify (instance, t)
			     then (Type.unify (instance, t, r)
				   ; SOME (Xexp.monoVar (y, Type.toXml (t, r))))
			  else NONE,
			  fn () =>
			  (let
			      open Layout
			      val _ =
				 Control.error
				 (r,
				  seq [str "impossible use of overloaded var: ",
				       str (Var.originalName x)],
				  Type.layoutPretty instance)
			   in
			      Xexp.unit ()
			   end)))
		     val _ = List.push (overloads, promise)
		  in
		     (promise, instance, r)
		  end
	 end
      val varExp = Trace.trace ("varExp", Var.layout o #1, layoutExpCode) varExp
      (*------------------------------------*)
      (*               casee                *)
      (*------------------------------------*)
      fun casee {test: Xexp.t,
		 testType: Xtype.t,
		 caseType: Xtype.t,
		 cases: (NestedPat.t * Xexp.t) vector,
		 region: Region.t}: Xexp.t =
	 let
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
			     (Xlambda.new
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
		  val _ = Int.inc matchCompileCalls
	       in
		  Xexp.let1
		  {var = testVar,
		   exp = test,
		   body = 
		   Xexp.lett
		   {decs = decs,
		    body = MatchCompile.matchCompile {conTycon = conTycon,
						      tyconCons = tyconCons,
						      test = testVar,
						      testType = testType,
						      caseType = caseType,
						      cases = cases,
						      region = region}}}
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
				    val tys = Xtype.detuple testType
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
      fun forceRules ({argType: Type.t,
		       resultType,
		       rules: (patCode * expCode) vector,
		       filePos},
		      p: NestedPat.node,
		      e: Xexp.t,
		      region): Xml.Lambda.t =
	 let
	    val arg = Var.newNoname ()
	    val argType = Type.toXml (argType, region)
	    val resultType = Type.toXml (resultType, region)
	 in
	    Xml.Lambda.new
	    {arg = arg, argType = argType,
	     body = (Xexp.toExp
		     (casee {test = Xexp.monoVar (arg, argType),
			     testType = argType,
			     cases = (Vector.concat
				      [Vector.map (rules, fn (p, e) =>
						   (finishPat p, finishExp e)),
				       Vector.new1
				       (NestedPat.new (p, argType),
					Xexp.raisee ({exn = e,
						      filePos = SOME filePos},
						     resultType))]),
			     caseType = resultType,
			     region = region}))}
	 end
      fun forceRulesMatch (rs, region) =
	 forceRules (rs, NestedPat.Wild, match (), region)
      fun forceRulesReraise (rs, region) =
	 let
	    val x = Var.newNoname ()
	 in
	    forceRules (rs, NestedPat.Var x, Xexp.monoVar (x, Xtype.exn),
			region)
	 end
      val emptyDec: decCode = fn e => e
      fun appendDec (d: decCode, d': decCode): decCode = d o d'
      fun cons (thunk: unit -> Xdec.t list): decCode = 
	 fn (e, t) => (Xexp.lett {decs = thunk (), body = e}, t)
      fun valDec (var: Var.t,
		  scheme: Scheme.t,
		  bound: unit -> Tyvar.t vector,
		  exp as (_, t, r): expCode,
		  kind: VarRange.kind,
		  env: Env.t): decCode * Env.t =
	 (cons (fn () =>
		[Xdec.PolyVal {tyvars = bound (),
			       var = var,
			       ty = Type.toXml (t, r),
			       exp = Xexp.toExp (finishExp exp)}]),
	  Env.extendVarRange (env, var, VarRange.T {scheme = scheme,
						    kind = kind}))
      fun patDec (p as (_, tp, rp): patCode,
		  e as (_, te, re): expCode,
		  filePos: string): decCode =
	 (Type.unify (tp, te, rp)
	  ; (fn (body, ty) =>
	     let
		val p = finishPat p
	     in
		(casee {test = finishExp e,
			testType = Type.toXml (te, re),
			caseType = ty,
			cases = (Vector.new2
				 ((p, body),
				  (NestedPat.new
				   (NestedPat.Wild, NestedPat.ty p),
				   Xexp.raisee ({exn = bind (),
						 filePos = SOME filePos},
						ty)))),
			region = Region.append (rp, re)},
		 ty)
	     end))
      fun multiExtend (env, xts) =
	 List.fold (xts, env, fn ((x, t), env) =>
		    Env.extendVar (env, x, Scheme.fromType t))
      fun inferValDec (tyvars: Tyvar.t vector,
		       pat: Cpat.t,
		       exp: Cexp.t,
		       filePos: string,
		       e: expCode as (_, te, _),
		       env: Env.t): decCode * Env.t =
	 let
	    val region = Cpat.region pat
	    val (p as (_, tp, _), xts) = inferPat (pat, [])
	    fun simple () = (patDec (p, e, filePos),
			     multiExtend (env, xts))
	    val _ = Type.unify (te, tp, region)
	 in if Cexp.isExpansive exp
	       then if Vector.isEmpty tyvars
		       then simple ()
		    else
		       let
			  open Layout
			  val _ = 
			     Control.error
			     (Cexp.region exp,
			      str "can't generalize an expansive expression",
			      empty)
		       in
			  simple ()
		       end
	    else
	       let
		  val {bound, mayHaveTyvars, scheme} =
		     Env.close (env, te, tyvars, region)
		  fun vd x = valDec (x, scheme, bound, e, VarRange.Normal, env)
		  fun polymorphic () =
		     (* Polymorphic pattern.
		      *  val 'a Foo (y1, y2) = e
		      * Expands to
		      *  val x = e
		      *  val Foo _ = x
		      *  val y1 = fn () => let val Foo (y1', _) = x in y1' end
		      *  val y2 = fn () => let val Foo (_, y2') = x in y2' end
		      * Uses of y1 and y2 are thawed.
		      *)
		     let
			val x = Var.newNoname ()
			val (d, env) = vd x
			val d =
			   if Cpat.isRefutable pat
			      then
				 appendDec
				 (d,
				  patDec
				  (#1 (inferPat (Cpat.removeVars pat, [])),
				   varExp (x, env, region),
				   filePos))
			   else d
		     in List.fold
			(Cpat.vars pat, (d, env), fn (y, (d, env)) =>
			 let
			    val y' = Var.new y
			    val p = Cpat.removeOthersReplace (pat, y, y')
			    val (p, xts) = inferPat (p, [])
			    val e as (_, te, _) =
			       delayExp
			       (letExp (patDec (p, varExp (x, env, region),
						filePos),
					varExp (y', multiExtend (env, xts),
						region)))
			    val {bound, scheme, ...} =
			       Env.close (env, te, Vector.new0 (), region)
			    val (d', env) =
			       valDec (y, scheme, bound, e, VarRange.Delayed,
				       env)
			 in (appendDec (d, d'), env)
			 end)
		     end
	       in
		  case (mayHaveTyvars, Cpat.node pat) of
		     (false, _) => simple ()
		   | (_, Cpat.Wild) => vd (Var.newNoname ())
		   | (_, Cpat.Var x) => vd x
		   | (_, Cpat.Constraint (p, _)) =>
			(case Cpat.node p of
			    Cpat.Var x => vd x
			  | _ => polymorphic ())
		   | _ => polymorphic ()
	       end
	 end
      fun processConArg (tycon, tyvars: Tyvar.t vector, {con, arg}) =
	 let
	    val result = Ctype.con (tycon, Vector.map (tyvars, Ctype.var))
	    val (ty, arg) =
	       case arg of
		  NONE => (result, NONE)
		| SOME arg => (Ctype.arrow (arg, result),
			       SOME (Type.toXml (Type.fromCoreML arg,
						 Region.bogus)))
	    val _ =
	       setConInfo (con, {tycon = tycon,
				 scheme = PScheme.T {tyvars = tyvars, ty = ty}})
	    val _ = conScheme con
	 in
	    {con = con, arg = arg}
	 end
      fun processException ca =
	 processConArg (Tycon.exn, Vector.new0 (), ca)
      (* accumulate the datatype declarations *)
      val dbsRef = ref []
      (*------------------------------------*)
      (*              inferDec              *)
      (*------------------------------------*)
      fun inferDec arg: decCode * Env.t =
	 traceInferDec
	 (fn (d: Cdec.t, env: Env.t) =>
	  case Cdec.node d of
	     Cdec.Datatype dbs =>
		(Vector.foreach
		 (dbs, fn {tyvars, tycon, cons} =>
		  let
		     val cons = Vector.map (cons, fn ca =>
					    processConArg (tycon, tyvars, ca))
		     val _ = setTyconCons (tycon, Vector.map (cons, #con))
		     val db = {tyvars = tyvars,
			       tycon = tycon,
			       cons = cons}
		  in if Tycon.equals (tycon, Tycon.reff)
			then ()
		     else List.push (dbsRef, db)
		  end)
		 ; (emptyDec, env))
	   | Cdec.Exception (e as {con, ...}) =>
		let
		   val ca = processException e
		in
		   (cons (fn () => [Xdec.Exception ca]),
		    (* There is no need to extend the environment with the type
		     * of the exception argument, since all tyvars in it must
		     * be in scope and hence already occur in the type env.
		     *)
		    env)
		end
	   | Cdec.Fun {tyvars, decs} =>
		if 0 = Vector.length decs
		   then (emptyDec, env)
		else
		let
		   (* type args to recursive calls *)
		   val argsRef: (unit -> Tyvar.t vector) option ref = ref NONE
		   val args =
		      Promise.lazy
		      (fn () => Vector.map (valOf (!argsRef) (), Xtype.var))
		   val env' = Env.extendTyvars (env, tyvars)
		   val (decs, env') =
		      Vector.mapAndFold
		      (decs, env', fn ({match, profile, types, var}, env) =>
		       let
			  val argType = newType ()
			  val resultType = newType ()
			  val t = Type.arrow (argType, resultType)
			  val _ =
			     Vector.foreach
			     (types, fn t' =>
			      Type.unify (t, Type.fromCoreML t',
					  Cmatch.region match))
		       in
			  ({argType = argType,
			    match = match,
			    profile = profile,
			    resultType = resultType,
			    var = var},
			   Env.extendVarRange
			   (env, var,
			    VarRange.T {scheme = Scheme.fromType t,
					kind = VarRange.Recursive args}))
		       end)
		   val region = Cmatch.region (#match (Vector.sub (decs, 0)))
		   val decs =
		      Vector.map
		      (decs, fn {argType, match, profile, resultType, var} =>
		       let
			  val saved = !currentFunction
			  val _ = currentFunction := var :: saved
			  val rs = inferMatchUnify (match, env',
						    argType, resultType)
			  val _ = currentFunction := saved
		       in
			  {profile = profile,
			   region = Cmatch.region match,
			   rules = rs,
			   ty = Type.arrow (argType, resultType),
			   var = var}
		       end)
		   val {bound, schemes} =
		      Env.closes (env, Vector.map (decs, #ty), tyvars, region)
		   val _ = argsRef := SOME bound
		   val env =
		      Vector.fold2
		      (decs, schemes, env, fn ({var, ...}, scheme, env) =>
		       Env.extendVar (env, var, scheme))
		in (cons (fn () =>
			  [Xdec.Fun
			   {tyvars = bound (),
			    decs = (Vector.map
				    (decs,
				     fn {var, profile, region, rules, ty} =>
				     let
					val ty = Type.toXml (ty, region)
					val {arg, argType, body, ...} =
					   Xlambda.dest
					   (forceRulesMatch (rules, region))
					val body =
					   case profile of
					      NONE => body
					    | SOME si =>
						 Xml.Exp.enterLeave
						 (body,
						  #2 (Xtype.dearrow ty),
						  si)
					val lambda =
					   Xlambda.new
					   {arg = arg,
					    argType = argType,
					    body = body}
				     in
					{var = var,
					 ty = ty,
					 lambda = lambda}
				     end))}]),
		    env)
		end
	   | Cdec.Overload {var, scheme = CoreML.Scheme.T {tyvars, ty}, ovlds} =>
		(emptyDec,
		 let
		    val ty = Type.fromCoreML ty
		 in
		    Env.extendVarRange
		    (env, var,
		     VarRange.T
		     {scheme = Scheme.make {canGeneralize = false,
					    tyvars = tyvars,
					    ty = ty},
		      kind =
		      VarRange.Overload
		      (Vector.map (ovlds, fn y =>
				   (y, Scheme.ty (Env.lookupVar (env, y)))))})
		 end)
	   | Cdec.Val {tyvars, pat, exp, filePos} =>
		inferValDec (tyvars, pat, exp, filePos,
			     inferExp (exp, Env.extendTyvars (env, tyvars)),
			     env)
	     ) arg
      and inferDecs (ds: Cdec.t vector, env: Env.t): decCode * Env.t =
	 Vector.fold (ds, (emptyDec, env), fn (d, (d', env)) =>
		      let val (d, env) = inferDec (d, env)
		      in (appendDec (d', d), env)
		      end)
      (*------------------------------------*)
      (*              inferExp              *)
      (*------------------------------------*)
      and inferExp arg: expCode =
	 traceInferExp
	 (fn (e, env) =>
	  let
	     val region = Cexp.region e
	  in
	     case Cexp.node e of
		Cexp.App (e1, e2) => apply (e1, env, SOME (inferExp (e2, env)))
	      | Cexp.Con _ => apply (e, env, NONE)
	      | Cexp.Const c =>
		   let
		      val ty = Type.ofConst c
		   in
		      (fn () => Xexp.const (makeXconst (c, ty)),
		       ty, region)
		   end
	      | Cexp.Constraint (e, t') =>
		   let
		      val ans as (_, t, _) = inferExp (e, env)
		      val _ = Type.unify (t, Type.fromCoreML t', region)
		   in
		      ans
		   end
	      | Cexp.Record r =>
		   (* This code is messy because the components of the record
		    * have to be evaluated left to right as they appeared in the
		    * source program, but then ordered according to sorted field
		    * name within the tuple.
		    *)
		   let
		      val fes = Record.toVector r
		      val es = Vector.map (fes, fn (_, e) => inferExp (e, env))
		      val ty =
			 Type.record
			 {flexible = false,
			  record = (Srecord.fromVector
				    (Vector.map2
				     (fes, es, fn ((f, _), (_, t, _)) =>
				      (f, t)))),
			  region = Region.bogus}
		   in (fn () =>
		       Xexp.seq
		       (Vector.map (es, finishExp), fn es =>
			Xexp.tuple {exps = (sortByField
					    (Vector.map2
					     (fes, es, fn ((f, _), e) => (f, e)))),
				    ty = Type.toXml (ty, region)}),
		       ty, region)
		   end
	      | Cexp.Fn {match = m, profile} =>
		   let
		      val rs as {argType, resultType, rules, ...} =
			 inferMatch (m, env)
		   in
		      (fn () =>
		       let
			  val {arg, argType, body, ...} =
			     Xlambda.dest (forceRulesMatch (rs, region))
			  val resultType = Type.toXml (resultType, region)
			  val body =
			     case profile of
				NONE => body
			      | SOME si =>
				   Xml.Exp.enterLeave (body, resultType, si)
		       in
			  Xexp.lambda {arg = arg,
				       argType = argType,
				       body = Xexp.fromExp (body, resultType),
				       bodyType = resultType}
		       end,
		       Type.arrow (argType, resultType),
		       region)
		   end
	      | Cexp.Handle (e, m) =>
		   let
		      val e as (_, t, _) = inferExp (e, env)
		      val rs as {argType, resultType, ...} = inferMatch (m, env)
		      val _ = Type.unify (t, resultType, region)
		      val _ = Type.unify (argType, Type.exn, region)
		   in (fn () =>
		       let
			  val {arg, body, ...} =
			     Xlambda.dest
			     (forceRulesReraise (rs, Cmatch.region m))
			  val t' = Type.toXml (t, region)
		       in Xexp.handlee
			  {try = finishExp e,
			   catch = (arg, Type.toXml (Type.exn, Region.bogus)),
			   handler = Xexp.fromExp (body, t'),
			   ty = t'}
		       end,
		       t, region)
		   end
	      | Cexp.Let (ds, e) =>
		   let val (ds, env) = inferDecs (ds, env)
		   in letExp (ds, inferExp (e, env))
		   end
	      | Cexp.Prim _ => apply (e, env, NONE)
	      | Cexp.Raise {exn, filePos} =>
		   let
		      val e as (_, t, _) = inferExp (exn, env)
		      val resultType = newType ()
		      val _ = Type.unify (t, Type.exn, region)
		   in
		      (fn () => Xexp.raisee ({exn = finishExp e,
					      filePos = SOME filePos},
					     Type.toXml (resultType, region)),
		       resultType, region)
		   end
	      | Cexp.Var x => varExp (x, env, region)
	  end) arg
      and applyOne (e as (_, t, region): expCode,
		    e' as (_, t', _): expCode): expCode =
	 let
	    val resultType = newType ()
	    val _ = Type.unify (t, Type.arrow (t', resultType), region)
	 in
	    (fn () => Xexp.app {func = finishExp e,
				arg = finishExp e',
				ty = Type.toXml (resultType, region)},
	     resultType, region)
	 end
      and apply (e1: Cexp.t, env: Env.t, arg: expCode option): expCode =
	 let
	    val region = Cexp.region e1
	    fun eta (ty: Type.t,
		     make: (Xexp.t * Xtype.t) option * Xtype.t -> Xexp.t)
	       : expCode =
	       case (Type.dearrowOpt ty, arg) of
		  (NONE, NONE) =>
		     (fn () => make (NONE, Type.toXml (ty, region)), ty, region)
		| (NONE, SOME _) =>
		     let
			open Layout
			val _ =
			   Control.error
			   (region,
			    str "attempt to apply non-function",
			    empty)
		     in
			(fn () =>
			 (* This point is unreachable because checkForErrors
			  * will abort after unification.
			  *)
			 Error.bug "unreachable",
			 ty, region)
		     end
		| (SOME (t1, t2), SOME (argExp as (_, argTy, _))) =>
		     (Type.unify (t1, argTy, region)
		      ; (fn () => make (SOME (finishExp argExp,
					      Type.toXml (t1, region)),
					Type.toXml (t2, region)),
			 t2, region))
		| (SOME (t1, t2), NONE) =>
		     let
			val x = Var.newNoname ()
		     in
			(fn () =>
			 let
			    val t1 = Type.toXml (t1, region)
			    val t2 = Type.toXml (t2, region)
			 in
			    Xexp.lambda
			    {arg = x,
			     argType = t1,
			     body = make (SOME (Xexp.monoVar (x, t1), t1), t2),
			     bodyType = t2}
			 end,
			 ty, region)
		     end
	 in
	    case Cexp.node e1 of
	       Cexp.App (e1, e2) =>
		  let
		     val e = apply (e1, env, SOME (inferExp (e2, env)))
		  in
		     case arg of
			NONE => e
		      | SOME e' => applyOne (e, e')
		  end
	     | Cexp.Con con =>
		  let
		     val {instance, args} = instCon con
		  in
		     eta (instance, fn (arg, resultType) =>
			  if Con.equals (con, Con.reff)
			     then (case arg of
				      NONE => Error.bug "bad ref"
				    | SOME (arg, _) =>
					 Xexp.primApp {prim = Prim.reff,
						       targs = args (),
						       args = Vector.new1 arg,
						       ty = resultType})
			  else Xexp.conApp {con = con,
					    targs = args (),
					    arg = (case arg of
						      NONE => NONE
						    | SOME (x, _) => SOME x),
					    ty = resultType})
		  end
	     | Cexp.Prim prim =>
		  let
		     val {instance, args = targs} =
			instantiatePrim (Prim.scheme prim, region)
		  in
		     eta (instance, fn (arg, resultType) =>
			  let
			     datatype z = datatype Prim.Name.t
			     fun make (args: Xexp.t vector): Xexp.t =
				let
				   fun app p =
				      Xexp.primApp {prim = p,
						    targs = targs (),
						    args = args,
						    ty = resultType}
				   fun id () = Vector.sub (args, 0)
				in
				   case Prim.name prim of
				      BuildConstant c =>
					 let
					    datatype z = datatype BuildConst.t
					 in
					    case lookupBuildConstant c of
					       Bool b =>
						  if b
						     then Xexp.truee ()
						  else Xexp.falsee ()
					     | Int i =>
						  Xexp.const
						  (Const.int
						   (IntX.make
						    (IntInf.fromInt i,
						     IntSize.default)))
					 end
				    | C_CS_charArrayToWord8Array => id ()
				    | Char_chr =>
					 app (Prim.intToWord
					      (IntSize.default, W8))
				    | Char_ge => app (Prim.wordGe W8)
				    | Char_gt => app (Prim.wordGt W8)
				    | Char_le => app (Prim.wordLe W8)
				    | Char_lt => app (Prim.wordLt W8)
				    | Char_ord =>
					 app (Prim.wordToInt
					      (W8, IntSize.default))
				    | Char_toWord8 => id ()
				    | Constant c =>
					 Xexp.const (lookupConstant c)
				    | String_toWord8Vector => id ()
				    | Word8_toChar => id ()
				    | Word8Vector_toString => id ()
				    | _ => app prim
				end
			  in
			     case (Prim.numArgs prim, arg) of
				(NONE, NONE) => make (Vector.new0 ())
			      | (SOME n, SOME (arg, argType)) =>
				   (case n of
				       0 => make (Vector.new0 ())
				     | 1 => make (Vector.new1 arg)
				     | _ => 
					  Xexp.detuple
					  {tuple = arg,
					   body =
					   fn components =>
					   make (Vector.map (components,
							     Xexp.varExp))})
			      | _ =>
				   (* FIXME -- should use Control.error? *)
				   Error.bug "primApp mismatch"
			  end)
		  end
	     | _ =>
		  let val e1 = inferExp (e1, env)
		  in case arg of
		     NONE => e1
		   | SOME e2 => applyOne (e1, e2)
		  end
	 end
      and inferMatch (m, env): {argType: Type.t,
				filePos: string,
				resultType: Type.t,
				rules: (patCode * expCode) vector} =
	 inferMatchUnify (m, env, newType (), newType ())
      and inferMatchUnify (m: Cmatch.t, env, argType, resultType) =
	 {argType = argType,
	  resultType = resultType,
	  filePos = Cmatch.filePos m,
	  rules = Vector.map (Cmatch.rules m, fn (p, e) =>
			      let
				 val region = Cpat.region p
				 val (p as (_, tp, _), xts) = inferPat (p, [])
				 val env = multiExtend (env, xts)
				 val e as (_, te, _)  = inferExp (e, env)
				 val _ = Type.unify (tp, argType, region)
				 val _ = Type.unify (te, resultType, region)
			      in
				 (p, e)
			      end)}
      (*------------------------------------*)
      (*    main code for type inference    *)
      (*------------------------------------*)
      val Cprogram.T {decs} = p
      val _ = Control.checkForErrors "type variable scope inference"
      val (ds, env) =
	 Control.trace (Control.Pass, "unification")
	 inferDecs (decs, Env.empty)
      val _ = List.foreach (!overloads, fn p => (p (); ()))
      val _ = overloads := []
      val _ = Control.checkForErrors "type check"
      val (body, _) =
	 Control.trace (Control.Pass, "finishInfer")
	 ds (Xexp.unit (), Xtype.unit)
      val _ = Control.checkForErrors "type check"
      val xml = Xml.Program.T {datatypes = Vector.fromList (!dbsRef),
			       body = Xexp.toExp body,
			       overflow = NONE}
      val _ = destroyCon ()
      val _ = destroyTycon ()
   in
      xml
   end

val infer =
   Trace.trace ("infer",
		CoreML.Program.layout o #program,
		Xml.Program.layout) infer

structure Scheme = PScheme
   
end
