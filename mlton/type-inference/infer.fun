(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Infer (S: INFER_STRUCTS): INFER =
struct
      
open S

open CoreML.Atoms

structure Srecord = SortedRecord
structure Field = Record.Field
structure Scope = Scope (structure CoreML = CoreML)
structure Env = TypeEnv (open CoreML
			 structure XmlType = Xml.Type)
structure Scheme = Env.InferScheme
structure Type = Env.Type

structure PScheme = Prim.Scheme

local open Ast
in structure Aconst = Const
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

structure Type =
   struct
      open Type

      fun fromCoreML t =
	 case t of
	    Ctype.Var a => var a
	  | Ctype.Con (c, ts) => con (c, Vector.map (ts, fromCoreML))
	  | Ctype.Record r => record {flexible = false,
				      record = Srecord.map (r, fromCoreML)}
   end

fun instantiate arg =
   let val {instance, args} = Scheme.instantiate arg
   in {instance = instance,
       args = fn () => Vector.map (args, Type.toXml)}
   end

val instantiate =
   Trace.trace ("instantiate",
		Scheme.layout o #scheme,
		Type.layout o #instance)
   instantiate

fun instantiatePrim (PScheme.T {tyvars, ty}) =
   instantiate {scheme = Scheme.T {tyvars = tyvars, ty = Type.fromCoreML ty},
		canGeneralize = true}

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
		       val char = Char
		       val int = Int
		       val word = Word
		       val word8 = Word8
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

type patCode = (unit -> NestedPat.node) * Type.t
type expCode = (unit -> Xexp.t) * Type.t

(* decCode can't quite be of type  unit -> Xdec.t list
 * because expanding val p = e requires nesting the remaining declarations
 * inside a case expression
 *)
type decCode = Xexp.t * Xtype.t -> Xexp.t * Xtype.t

type ruleCode = patCode * expCode
type rulesCode = ruleCode list

fun finishPat ((p, ty): patCode): NestedPat.t =
   NestedPat.new (p (), Type.toXml ty)
   
fun finishExp ((e, ty): expCode): Xexp.t = e ()
   
local open Layout
in
   fun layoutPatCode ((_, t): patCode) = seq [str "Patcode ", Type.layout t]
   val layoutExpCode: expCode -> t = tuple2 (fn _ => str "<exp>", Type.layout)
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
   let val size = String.size str
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
   in v
   end

fun makeXconst (c: Aconst.t, ty: Type.t): Xconst.t =
   let val tycon = Xtype.detycon (Type.toXml ty)
      datatype z = datatype Xconst.Node.t
   in Xconst.make
      (case c of
	  Aconst.Char c => Char c
	| Aconst.Int s =>
	     if Tycon.equals (tycon, Tycon.intInf)
		then IntInf (stringToIntInf s)
	     else
		Int
		(let
		    val radix =
		       if String.isPrefix {string = s, prefix = "0x"}
			  orelse String.isPrefix {string = s, prefix = "~0x"}
			  then StringCvt.HEX
		       else StringCvt.DEC
		 in case StringCvt.scanString (Pervasive.Int32.scan radix) s of
		    SOME n => if Tycon.equals (tycon, Tycon.int)
				 then n
			      else Error.bug "strange int"
		  | NONE => Error.bug "invalid integer constant"
		 end handle Overflow => Error.bug "integer constant too big")
	| Aconst.Real r => Real r
	| Aconst.String s => String s
	| Aconst.Word w =>
	     Word (if Tycon.equals (tycon, Tycon.word)
		     then w
		  else if Tycon.equals (tycon, Tycon.word8)
			  then if w = Word.andb (w, 0wxFF)
				  then w
			       else Error.bug "word8 too big"
		       else Error.bug ("strange word " ^ Tycon.toString tycon)),
       tycon)
   end

fun 'a sortByField (v: (Field.t * 'a) vector): 'a vector =
   Vector.map (Vector.sort (v, fn ((f, _), (f', _)) => Field.<= (f, f')),
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

fun infer {program = p: CoreML.Program.t, lookupConstant}: Xml.Program.t =
   let
      val matchCompileCalls = ref 0
      val {get = tyconCons: Tycon.t -> Con.t vector,
	   set = setTyconCons, destroy = destroyTycon} =
	 Property.destGetSetOnce (Tycon.plist,
				  Property.initRaise ("tyconCons", Tycon.layout))
      val {get = conInfo: Con.t -> {tycon: Tycon.t,
				    scheme: Prim.Scheme.t},
	   set = setConInfo, destroy = destroyCon} =
	 Property.destGetSetOnce (Con.plist,
				  Property.initRaise ("conInfo", Con.layout))
      val conTycon = #tycon o conInfo
      val conScheme = #scheme o conInfo
      val conTycon = Trace.trace ("conTycon", Con.layout, Tycon.layout) conTycon
      val conScheme =
	 Trace.trace ("conScheme", Con.layout, Prim.Scheme.layout) conScheme
      fun instCon c  = instantiatePrim (conScheme c)
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
	  case p of
	     Cpat.Wild => ((fn () => NestedPat.Wild, newType ()),
			   ac)
	   | Cpat.Var x => let val t = newType ()
			   in ((fn () => NestedPat.Var x, t),
			       (x, t) :: ac)
			   end
	   | Cpat.Const c =>
		let val ty = Type.ofConst c
		in ((fn () => NestedPat.Const (makeXconst (c, ty)), ty),
		    ac)
		end
	   | Cpat.Con {con, arg} =>
		let val {instance, args} = instCon con
		in case arg of
		   NONE => ((fn () => NestedPat.Con {con = con,
						     targs = args (),
						     arg = NONE},
			     instance),
			    ac)
		 | SOME p => 
		      let val (t1, t2) = Type.dearrow instance
			 val (p as (_, tp), ac) = inferPat (p, ac)
		      in Type.unify (t1, tp)
			 ; ((fn () => NestedPat.Con {con = con,
						    targs = args (),
						    arg = SOME (finishPat p)},
			     t2),
			    ac)
		      end
		end
	   | Cpat.Record {flexible, record} =>
		let
		   val (fs, ps) = Record.unzip record
		   val (ps, ac) = Vector.mapAndFold (ps, ac, inferPat)
		   val t =
		      Type.record
		      {flexible = flexible,
		       record = Srecord.zip (fs, Vector.map (ps, #2))}
		in ((fn () =>
		     NestedPat.Tuple
		     (if not flexible
			 then sortByField (Vector.map2
					   (fs, ps, fn (f, p) =>
					    (f, finishPat p)))
		      else let
			      val {flexible, record = record'} = Type.derecord t
			   in
			      if flexible
				 then (Layout.output (Cpat.layout p,
						      Out.error)
				       ; Out.newline Out.error
				       ; Error.bug "unresolved flexible pattern")
			      else
				 let
				    val record = Record.zip (fs, ps)
				 in Vector.map
				    (Srecord.toVector record', fn (f, t) =>
				     case Record.peek (record, f) of
					NONE => NestedPat.new (NestedPat.Wild,
							       Type.toXml t)
				      | SOME p => finishPat p)
				 end
			   end),
			 t),
		    ac)
		end
	   | Cpat.Constraint (p, t') =>
		let val ((p, t), ac) = inferPat (p, ac)
		in Type.unify (t, Type.fromCoreML t')
		   ; ((p, t), ac)
		end
	   | Cpat.Layered (x, p) =>
		let val (p as (_, t), ac) = inferPat (p, ac)
		in ((fn () => NestedPat.Layered (x, finishPat p),
		     t),
		    (x, t) :: ac)
		end) arg


      fun delayExp (e as (_, t): expCode): expCode =
	 (fn () => Xexp.lambda {arg = Var.newNoname (),
			       argType = Xtype.unit,
			       body = finishExp e,
			       bodyType = Type.toXml t},
	  Type.arrow (Type.unit, t))
	 
      fun letExp (d: decCode, e as (_, t): expCode): expCode =
	 (fn () => #1 (d (finishExp e, Type.toXml t)), t)

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
      fun varExp (x: Var.t, env: Env.t): expCode =
	 let
	    val vr as VarRange.T {scheme, kind} = Env.lookupVarRange (env, x)
	 in
	    case kind of
	       VarRange.Normal =>
		  let val {instance, args} = instantiate {scheme = scheme,
							  canGeneralize = true}
		  in (fn () => Xexp.var {var = x,
					 targs = args (),
					 ty = Type.toXml instance},
		      instance)
		  end
	     | VarRange.Recursive tyvars =>
		  let
		     val ty = Scheme.ty scheme
		  in
		     (fn () => Xexp.var {var = x,
					 targs = Vector.map (!tyvars, Xtype.var),
					 ty = Type.toXml ty},
		      ty)
		  end
	     | VarRange.Delayed =>
		  let
		     val {instance, args} = instantiate {scheme = scheme,
							 canGeneralize = true}
		     val (_, t') = Type.dearrow instance
		  in
		     (fn () =>
		      Xexp.app {func = Xexp.var {var = x,
						 targs = args (),
						 ty = Xtype.arrow (Xtype.unit,
								   Type.toXml t')},
				arg = Xexp.unit (),
				ty = Type.toXml t'},
		      t')
		  end
	     | VarRange.Overload yts =>
		  let
		     val {instance, args} = instantiate {scheme = scheme,
							 canGeneralize = false}
		     val promise =
			Promise.lazy
			(fn () =>
			 Vector.loop
			 (yts,
			  fn (y, t) =>
			  if Type.canUnify (instance, t)
			     then (Type.unify (instance, t)
				   ; SOME (Xexp.monoVar (y, Type.toXml t)))
			  else NONE,
			     fn () => (let open Layout
				       in output (align [Var.layout x,
							 VarRange.layout vr],
						  Out.error)
				       end
					  ; Out.newline Out.error
				       ; Error.bug "impossible use of overloaded var")))
		     val _ = List.push (overloads, promise)
		  in
		     (promise, instance)
		  end
	 end
      val varExp = Trace.trace ("varExp", Var.layout o #1, layoutExpCode) varExp
      (*------------------------------------*)
      (*               casee                *)
      (*------------------------------------*)
      fun casee {test: Xexp.t,
		 testType: Xtype.t,
		 caseType: Xtype.t,
		 cases: (NestedPat.t * Xexp.t) vector}: Xexp.t =
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
	       in Xexp.let1
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
						      cases = cases}}}
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
		  NestedPat.Con {con, targs, arg = NONE} =>
		     Vector.Continue ((Xpat.T {con = con,
					       targs = targs,
					       arg = NONE},
				       e) :: ac)
		| NestedPat.Wild =>
		     Vector.Done
		     (case ac of
			 [] => wild e
		       | _ => make (ac, SOME e))
		| _ => Vector.Done (normal ())
	    fun done ac = make (ac, NONE)
	 in Vector.fold' (cases, 0, [], step, done)
	 end
      fun forceRules ({argType, resultType, rules, filePos},
		      p: NestedPat.node,
		      e: Xexp.t): Xml.Lambda.t =
	 let
	    val arg = Var.newNoname ()
	    val argType = Type.toXml argType
	    val resultType = Type.toXml resultType
	 in Xml.Lambda.new
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
						      filePos = filePos},
						     resultType))]),
			     caseType = resultType}))}
	 end
      fun forceRulesMatch rs =
	 forceRules (rs, NestedPat.Wild, match ())
      fun forceRulesReraise rs =
	 let val x = Var.newNoname ()
	 in forceRules (rs, NestedPat.Var x, Xexp.monoVar (x, Xtype.exn))
	 end
      val emptyDec: decCode = fn e => e
      fun appendDec (d: decCode, d': decCode): decCode = d o d'
      fun cons (thunk: unit -> Xdec.t list): decCode = 
	 fn (e, t) => (Xexp.lett {decs = thunk (), body = e}, t)
      fun valDec (tyvars: Tyvar.t list,
		  var: Var.t,
		  exp as (_, t): expCode,
		  kind: VarRange.kind,
		  env: Env.t): decCode * Env.t =
	 let
	    val tyvars = Vector.fromList tyvars
	 in (cons (fn () => [Xdec.PolyVal {tyvars = tyvars,
					   var = var,
					   ty = Type.toXml t,
					   exp = Xexp.toExp (finishExp exp)}]),
	     Env.extendVarRange (env, var,
				 VarRange.T
				 {scheme = Scheme.T {tyvars = tyvars, ty = t},
				  kind = kind}))
	 end
      fun patDec (p as (_, tp): patCode,
		  e as (_, te): expCode,
		  filePos): decCode =
	 (Type.unify (tp, te)
	  ; (fn (body, ty) =>
	     let val p = finishPat p
	     in (casee {test = finishExp e,
			testType = Type.toXml te,
			caseType = ty,
			cases = (Vector.new2
				 ((p, body),
				  (NestedPat.new
				   (NestedPat.Wild, NestedPat.ty p),
				   Xexp.raisee ({exn = bind (),
						 filePos = filePos},
						ty))))},
		 ty)
	     end))
      fun multiExtend (env, xts) =
	 List.fold (xts, env, fn ((x, t), env) =>
		    Env.extendVar (env, x, Scheme.fromType t))
      fun inferValDec (tyvars: Tyvar.t vector,
		       pat: Cpat.t,
		       exp: Cexp.t,
		       filePos: string,
		       e: expCode as (_, te),
		       env: Env.t): decCode * Env.t =
	 let
	    val (p as (_, tp), xts) = inferPat (pat, [])
	    fun simple () = (patDec (p, e, filePos), multiExtend (env, xts))
	    val _ = Type.unify (te, tp)
	 in if Cexp.isExpansive exp
	       then if Vector.isEmpty tyvars
		       then simple ()
		    else (Layout.output (CoreML.Exp.layout exp, Out.error)
			  ; Out.newline Out.error
			  ; Error.bug "can't generalize an expansive exp")
	    else
	       let
		  val tyvarsClose = Env.close (env, te, tyvars)
		  fun vd x = valDec (tyvarsClose, x, e, VarRange.Normal, env)
	       in case (tyvarsClose, pat) of
		  ([], _) => simple ()
		| (_, Cpat.Wild) => vd (Var.newNoname ())
		| (_, Cpat.Var x) => vd x
		| (_, Cpat.Constraint (Cpat.Var x, _)) => vd x
		| _ =>
		     (* Polymorphic pattern.
		      *   val 'a Foo (y1, y2) = e
		      * Expands to
		      *   val x = e
		      *   val Foo _ = x
		      *   val y1 = fn () => let val Foo (y1', _) = x in y1' end
		      *   val y2 = fn () => let val Foo (_, y2') = x in y2' end
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
				   varExp (x, env),
				   filePos))
			   else d
		     in List.fold
			(Cpat.vars pat, (d, env), fn (y, (d, env)) =>
			 let
			    val y' = Var.new y
			    val p = Cpat.removeOthersReplace (pat, y, y')
			    val (p, xts) = inferPat (p, [])
			    val e as (_, te) =
			       delayExp
			       (letExp (patDec (p, varExp (x, env), filePos),
					varExp (y', multiExtend (env, xts))))
			    val (d', env) =
			       valDec (Env.close (env, te, Vector.new0 ()),
				       y, e, VarRange.Delayed, env)
			 in (appendDec (d, d'), env)
			 end)
		     end
	       end
	 end
      fun processConArg (tycon, tyvars: Tyvar.t vector, {con, arg}) =
	 let
	    val result = Ctype.con (tycon, Vector.map (tyvars, Ctype.var))
	    val (ty, arg) =
	       case arg of
		  NONE => (result, NONE)
		| SOME arg => (Ctype.arrow (arg, result),
			       SOME (Type.toXml (Type.fromCoreML arg)))
	    val _ =
	       setConInfo (con, {tycon = tycon,
				 scheme = PScheme.T {tyvars = tyvars, ty = ty}})
	    val _ = conScheme con
	 in
	    {con = con, arg = arg}
	 end
      fun processException ca = processConArg (Tycon.exn, Vector.new0 (), ca)
      (* accumulate the datatype declarations *)
      val dbsRef = ref []
      (*------------------------------------*)
      (*              inferDec              *)
      (*------------------------------------*)
      fun inferDec arg: decCode * Env.t =
	 traceInferDec
	 (fn (d: Cdec.t, env: Env.t) =>
	  case d of
	     Cdec.Val {tyvars, pat, exp, filePos} =>
		inferValDec (tyvars, pat, exp, filePos, inferExp (exp, env), env)
	   | Cdec.Datatype dbs =>
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
		let val ca = processException e
		in (cons (fn () => [Xdec.Exception ca]),
		    env)
		end
	   | Cdec.Overload {var, scheme=CoreML.Scheme.T {tyvars, ty}, ovlds} =>
		(emptyDec,
		 let val ty = Type.fromCoreML ty
		 in Env.extendVarRange
		    (env, var,
		     VarRange.T
		     {scheme = Scheme.T {tyvars = tyvars, ty = ty},
		      kind =
		      VarRange.Overload
		      (Vector.map (ovlds, fn y =>
				   (y, Scheme.ty (Env.lookupVar (env, y)))))})
		 end)
	   | Cdec.Fun {tyvars, decs} =>
		let
		   (* type args to recursive calls *)
		   val args: Tyvar.t vector ref = ref (Vector.new0 ())
		   val (decs, env') =
		      Vector.mapAndFold
		      (decs, env, fn ({var, types, match}, env) =>
		       let
			  val argType = newType ()
			  val resultType = newType ()
			  val t = Type.arrow (argType, resultType)
			  val _ =
			     Vector.foreach
			     (types, fn t' => Type.unify (t, Type.fromCoreML t'))
		       in
			  ({var = var,
			    argType = argType,
			    resultType = resultType,
			    match = match},
			   Env.extendVarRange
			   (env, var,
			    VarRange.T {scheme = Scheme.fromType t,
					kind = VarRange.Recursive args}))
		       end)
		   val decs =
		      Vector.map
		      (decs, fn {var, match, argType, resultType} =>
		       let
			  val rs = inferMatchUnify (match, env',
						    argType, resultType)
		       in {var = var,
			   ty = Type.arrow (argType, resultType),
			   rules = rs}
		       end)
		   val tyvarsClose =
		      Tyvars.toList
		      (Vector.fold
		       (decs, Tyvars.empty, fn ({ty, ...}, ac) =>
			Tyvars.+ (ac,
				  Tyvars.fromList
				  (Env.close (env, ty, tyvars)))))
		   val tyvarsCloseV = Vector.fromList tyvarsClose
		   val _ = args := tyvarsCloseV
		   val env =
		      Vector.fold
		      (decs, env, fn ({var, ty, ...}, env) =>
		       Env.extendVar (env, var,
				      Scheme.T {tyvars = tyvarsCloseV,
						ty = ty}))
		in (cons (fn () =>
			  [Xdec.Fun
			   {tyvars = Vector.fromList tyvarsClose,
			    decs =
			    Vector.map
			    (decs, fn {var, ty, rules} =>
			     {var = var,
			      ty = Type.toXml ty,
			      lambda = forceRulesMatch rules})}]),
		    env)
		end) arg
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
	 case e of
	    Cexp.Var x => varExp (x, env)
	  | Cexp.Prim _ => apply (e, env, NONE)
	  | Cexp.Con _ => apply (e, env, NONE)
	  | Cexp.App (e1, e2) => apply (e1, env, SOME (inferExp (e2, env)))
	  | Cexp.Const c => let val ty = Type.ofConst c
			    in (fn () => Xexp.const (makeXconst (c, ty)),
				ty)
			    end
	  | Cexp.Record r =>
	       (* This code is messy because the components of the record have
		* to be evaluated left to right as they appeared in the source
		* program, but then ordered according to sorted field name within
		* the tuple.
		*)
	       let
		  val fes = Record.toVector r
		  val es = Vector.map (fes, fn (_, e) => inferExp (e, env))
		  val ty =
		     Type.record
		     {flexible = false,
		      record = (Srecord.fromVector
				(Vector.map2 (fes, es, fn ((f, _), (_, t)) =>
					      (f, t))))}
	       in (fn () =>
		   Xexp.seq
		   (Vector.map (es, finishExp), fn es =>
		    Xexp.tuple {exps = (sortByField
					(Vector.map2
					  (fes, es, fn ((f, _), e) => (f, e)))),
			       ty = Type.toXml ty}),
		   ty)
	       end
	  | Cexp.Fn m =>
	       let
		  val rs as {argType, resultType, rules, ...} =
		     inferMatch (m, env)
	       in (fn () => let
			       val {arg, argType, body} =
				  Xlambda.dest (forceRulesMatch rs)
			       val resultType = Type.toXml resultType
			    in Xexp.lambda
			       {arg = arg,
				argType = argType,
				body = Xexp.fromExp (body, resultType),
				bodyType = resultType}
			    end,
		   Type.arrow (argType, resultType))
	       end
	  | Cexp.Let (ds, e) =>
	       let val (ds, env) = inferDecs (ds, env)
	       in letExp (ds, inferExp (e, env))
	       end
	  | Cexp.Constraint (e, t') =>
	       let
		  val ans as (_, t) = inferExp (e, env)
		  val _ = Type.unify (t, Type.fromCoreML t')
	       in
		  ans
	       end
	  | Cexp.Handle (e, m) =>
	       let
		  val (e, t) = inferExp (e, env)
		  val rs as {argType, resultType, ...} = inferMatch (m, env)
		  val _ = Type.unify (t, resultType)
		  val _ = Type.unify (argType, Type.exn)
	       in (fn () => let
			       val {arg, body, ...} =
				  Xlambda.dest (forceRulesReraise rs)
			       val t' = Type.toXml t
			    in Xexp.handlee {try = finishExp (e, t),
					     catch = (arg, Type.toXml Type.exn),
					     handler = Xexp.fromExp (body, t'),
					     ty = t'}
			    end,
			 t)
	       end
	  | Cexp.Raise {exn, filePos} =>
	       let
		  val e as (_, t) = inferExp (exn, env)
		  val resultType = newType ()
		  val _ = Type.unify (t, Type.exn)
	       in
		  (fn () => Xexp.raisee ({exn = finishExp e,
					  filePos = filePos},
					 Type.toXml resultType),
		   resultType)
	       end) arg
      and applyOne (e as (_, t): expCode, e' as (_, t'): expCode): expCode =
	 let
	    val resultType = newType ()
	    val _ = Type.unify (t, Type.arrow (t', resultType))
	 in
	    (fn () => Xexp.app {func = finishExp e,
				arg = finishExp e',
				ty = Type.toXml resultType},
	     resultType)
	 end
      and apply (e1: Cexp.t, env: Env.t, arg: expCode option): expCode =
	 let
	    fun eta (ty: Type.t,
		     make: (Xexp.t * Xtype.t) option * Xtype.t -> Xexp.t)
	       : expCode =
	       case (Type.dearrowOpt ty, arg)                 of
		  (NONE, NONE) => (fn () => make (NONE, Type.toXml ty), ty)
		| (NONE, SOME _) => Error.bug "application mismatch"
		| (SOME (t1, t2), SOME (argExp as (_, argTy))) =>
		     (Type.unify (t1, argTy)
		      ; (fn () => make (SOME (finishExp argExp, Type.toXml t1),
					Type.toXml t2),
			 t2))
		| (SOME (t1, t2), NONE) =>
		     let
			val x = Var.newNoname ()
		     in
			(fn () =>
			 let
			    val t1 = Type.toXml t1
			    val t2 = Type.toXml t2
			 in
			    Xexp.lambda
			    {arg = x,
			     argType = t1,
			     body = make (SOME (Xexp.monoVar (x, t1), t1), t2),
			     bodyType = t2}
			 end,
			 ty)
		     end
	 in
	    case e1 of
	       Cexp.Con con =>
		  let val {instance, args} = instCon con
		  in eta (instance, fn (arg, resultType) =>
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
			instantiatePrim (Prim.scheme prim)
		  in eta (instance, fn (arg, resultType) =>
			 let
			    fun make (args: Xexp.t vector): Xexp.t =
			       case Prim.name prim of
				  Prim.Name.Constant c =>
				     let
					datatype z = datatype LookupConstant.Const.t
				     in case lookupConstant c of
					Bool b => if b then Xexp.truee ()
						  else Xexp.falsee ()
				      | Int i => Xexp.const (Const.fromInt i)
				      | Real r => Xexp.const (Const.fromReal r)
				      | String s =>
					   Xexp.const (Const.fromString s)
				      | Word w => Xexp.const (Const.fromWord w)
				     end
				| _ => 
				     Xexp.primApp {prim = prim,
						   targs = targs (),
						   args = args,
						   ty = resultType}
			 in
			    case (Prim.numArgs prim, arg) of
			         (NONE,              NONE)     =>
				    make (Vector.new0 ())
			       | (SOME n,            SOME (arg, argType)) =>
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
			       | _                             =>
				    Error.bug "primApp mismatch"
			 end)
		  end
	     | Cexp.App (e1, e2) =>
		  let val e = apply (e1, env, SOME (inferExp (e2, env)))
		  in case arg of
		     NONE => e
		   | SOME e' => applyOne (e, e')
		  end
	     | _ => let val e1 = inferExp (e1, env)
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
      and inferMatchUnify (Cmatch.T {rules, filePos}, env, argType, resultType) =
	 {argType = argType,
	  resultType = resultType,
	  filePos = filePos,
	  rules = Vector.map (rules, fn (p, e) =>
			      let
				 val (p as (_, tp), xts) = inferPat (p, [])
				 val env = multiExtend (env, xts)
				 val e as (_, te)  = inferExp (e, env)
				 val _ = Type.unify (tp, argType)
				 val _ = Type.unify (te, resultType)
			      in
				 (p, e)
			      end)}
      (*------------------------------------*)
      (*    main code for type inference    *)
      (*------------------------------------*)
      val Cprogram.T {decs} = Scope.scope p
      val (ds, env) =
	 Control.trace (Control.Pass, "unification")
	 inferDecs (decs, Env.empty)
      val _ = List.foreach (!overloads, fn p => (p (); ()))
      val _ = overloads := []
      val (body, _) =
	 Control.trace (Control.Pass, "finishInfer")
	 ds (Xexp.unit (), Xtype.unit)
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
