(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
(*
 * All local variables in the Sxml are renamed to new variables in Cps,
 * unless they are global, as determined by the Globalization pass.
 * Renaming must happen because an Sxml variable will be bound in the Cps
 * once for each lambda it occurs in.  The main trickiness is caused because a
 * property is used to implement the renamer map.  Hence, a variable binding
 * must always be visited with "newVar" or "newScope" before it is looked up
 * with "getNewVar".  "newScope" also handles resetting the variable to its
 * old value once the processing of the lambda is done.
 *)
functor ClosureConvert (S: CLOSURE_CONVERT_STRUCTS): CLOSURE_CONVERT = 
struct

open S

structure Globalize = Globalize (open Sxml)

local open Sxml
in
   structure Scases = Cases
   structure Sexp = Exp
   structure Sdec = Dec
   structure Slambda = Lambda
   structure Spat = Pat
   structure SprimExp = PrimExp
   structure SvarExp = VarExp
   structure Stype = Type
   open Atoms
end

local open Cps
in
   structure Ccases = Cases
   structure Cause = Cause
   structure Ctype = Type
   structure Func = Func
   structure Cdec = Dec
   structure Cexp = DirectExp
   structure Cprogram = Program
   structure CPrimExp = PrimExp
end

structure Value = AbstractValue (structure Cps = Cps
				 structure Sxml = Sxml)
local open Value
in structure Lambdas = Lambdas
end

(* Accum.t is one of the results returned internally by the converter -- an
 * accumulation of toplevel Cps globals and function declarations.
 *)
structure Accum =
   struct
      structure AL = AppendList

      datatype t = T of {globals: {var: Var.t, ty: Ctype.t, exp: Cexp.t} AL.t,
			 functions: Cps.Function.t list}

      val empty = T {globals = AL.empty, functions = []}

      fun addGlobals (T {globals, functions}, gs) =
	 T {globals = AL.append (globals, AL.fromList gs),
	    functions = functions}

      fun addGlobal (ac, g) = addGlobals (ac, [g])

      fun addFunc (T {globals, functions}, f) =
	 T {globals = globals, functions = f :: functions}

      fun done (T {globals, functions}) =
	 {functions = functions,
	  globals =
	  let
	     val exp =
		(* Must shrink because coercions may be inserted at constructor
		 * applications.  I'm pretty sure the shrinking will eliminate
		 * any case expressions/local functions.
		 * The "NoDelete" is because the shrinker is just processing
		 * globals and hence cannot safely delete a variable that
		 * has no occurrences, since there may still be occurrences in
		 * functions.
		 *)
		Cps.shrinkExpNoDelete
		(Cexp.toExp (Cexp.lett
			     {decs = AL.toList globals,
			      body = Cexp.tuple {exps = Vector.new0 (),
						 ty = Ctype.unit}}))
	     val decs = Cps.Exp.decs exp
	  in 
	     List.map (decs,
		       fn Cdec.Bind b => b
			| _ => Error.bug "globalization produced a non binding")
	  end}
   end

(* val traceConvertExp =
 *    Trace.trace2 ("convertExp", Sexp.layout, Instance.layout, Cexp.layout)
 *)

val convertPrimExpInfo = Trace.info "ClosureConvert.convertPrimExp"
val valueTypeInfo = Trace.info "valueType"

structure LambdaFree = LambdaFree (Sxml)

local
   open LambdaFree
in
   structure Status = Status
end

structure LambdaInfo =
   struct
      datatype t =
	 T of {
	       con: Con.t ref,
	       frees: Var.t vector ref,
	       (* name is the original name in the source (i.e. SXML) program,
		* so the closure conversion output has some readability.
		*)
	       name: Func.t,
	       recs: Var.t vector ref,
	       (* The type of its environment record. *)
	       ty: Ctype.t option ref
	       }

      fun frees (T {frees, ...}) = !frees
   end

structure VarInfo =
   struct
      type t = {frees: Var.t list ref ref,
		isGlobal: bool ref,
		replacement: Var.t ref,
		status: Status.t ref,
		value: Value.t}

      local
	 fun make sel (r: t) = sel r
      in
	 val value = make #value
      end
   end

fun closureConvert (program as Sxml.Program.T {datatypes, body}): Cps.Program.t =
   let
      val {get = conArg: Con.t -> Value.t option, set = setConArg} =
	 Property.getSetOnce (Con.plist,
			      Property.initRaise ("conArg", Con.layout))
      val {get = varInfo: Var.t -> VarInfo.t, set = setVarInfo} =
	 Property.getSetOnce
	 (Var.plist, Property.initRaise ("closure convert info", Var.layout))
      val varInfo =
	 Trace.trace ("ClosureConvert.varInfo", Var.layout, Layout.ignore)
	 varInfo
      val varExpInfo = varInfo o SvarExp.var
      val isGlobal = ! o #isGlobal o varInfo
      val isGlobal = Trace.trace ("isGlobal", Var.layout, Bool.layout) isGlobal
      val value = #value o varInfo
      val varExp = value o SvarExp.var
      val expValue = varExp o Sexp.result
      (* ---------------------------------- *)
      (*             lambdaInfo             *)
      (* ---------------------------------- *)
      val {get = lambdaInfo: Slambda.t -> LambdaInfo.t,
	   set = setLambdaInfo} =
	 Property.getSetOnce
	 (Slambda.plist,
	  Property.initRaise ("closure convert info", Layout.ignore))
      val allLambdas: Slambda.t list ref = ref []
      (* Do the flow analysis.
       * Initialize lambdaInfo and varInfo.
       *)
      val _ =
	 Vector.foreach
	 (datatypes, fn {cons, ...} =>
	  Vector.foreach
	  (cons, fn {con, arg} =>
	   setConArg (con, (case arg of
			       NONE => NONE
			     | SOME t => SOME (Value.fromType t)))))
      val _ =
	 let
	    open Sxml
	    val bogusFrees = ref []
	    fun newVar (x, v) =
	       setVarInfo (x, {frees = ref bogusFrees,
			       isGlobal = ref false,
			       replacement = ref x,
			       status = ref Status.init,
			       value = v})
	    val newVar =
	       Trace.trace2 ("ClosureConvert.newVar",
			     Var.layout, Layout.ignore, Unit.layout)
	       newVar
	    fun varExps xs = Vector.map (xs, varExp)
	    fun merge (vs: Value.t list, ty: Type.t): Value.t =
	       let val v = Value.fromType ty
	       in List.foreach (vs, fn v' => Value.coerce {from = v', to = v})
		  ; v
	       end
	    fun loopExp (e: Exp.t): Value.t =
	       let val {decs, result} = Exp.dest e
	       in List.foreach (Exp.decs e, loopDec)
		  ; varExp result
	       end
	    and loopDec (d: Dec.t): unit =
	       let
		  datatype z = datatype Dec.t
	       in
		  case d of
		     Fun {decs, ...} =>
			(Vector.foreach (decs, fn {var, ty, ...} =>
					 newVar (var, Value.fromType ty))
			 ; (Vector.foreach
			    (decs, fn {var, lambda, ...} =>
			     Value.unify (value var,
					  loopLambda (lambda, var)))))
		   | MonoVal b => loopBind b
		   | _ => Error.bug "closure convert saw bogus Dec"
	       end
	    and loopBind {var, ty, exp} =
	       let
		  fun set v = newVar (var, v)
		  fun new () =
		     let val v = Value.fromType ty
		     in set v; v
		     end
		  datatype z = datatype PrimExp.t
	       in
		  case exp of
		     App {func, arg} =>
			let val arg = varExp arg
			   val result = new ()
			in Value.addHandler
			   (varExp func, fn l =>
			    let
			       val lambda = Value.Lambda.dest l
			       val {arg = formal, body, ...} =
				  Lambda.dest lambda
			    in Value.coerce {from = arg,
					     to = value formal}
			       ; Value.coerce {from = expValue body,
					       to = result}
			    end)
			end
		   | Case {cases, default, ...} =>
			let
			   val result = new ()
			   fun branch e =
			      Value.coerce {from = loopExp e, to = result}
			   fun handlePat (Pat.T {con, arg, ...}) =
			      case (arg,      conArg con) of
				 (NONE,        NONE)       => ()
			       | (SOME (x, _), SOME v)     => newVar (x, v)
			       | _ => Error.bug "constructor mismatch"
			   val _ = Cases.foreach' (cases, branch, handlePat)
			   val _ = Option.app (default, branch)
			in ()
			end
		   | ConApp {con, arg, ...} =>
			(case (arg,    conArg con) of
			    (NONE,   NONE)       => ()
			  | (SOME x, SOME v)     =>
			       Value.coerce {from = varExp x, to = v}
			  | _ => Error.bug "constructor mismatch"
			 ; new (); ())
		   | Const c => (new (); ())
		   | Handle {try, catch = (x, t), handler} =>
			let
			   val result = new ()
			in Value.coerce {from = loopExp try, to = result}
			   ; newVar (x, Value.fromType t)
			   ; Value.coerce {from = loopExp handler, to = result}
			end
		   | Lambda l => set (loopLambda (l, var))
		   | PrimApp {prim, args, ...} =>
			set (Value.primApply {prim = prim,
					      args = varExps args,
					      resultTy = ty})
		   | Raise _ => (new (); ())
		   | Select {tuple, offset} =>
			set (Value.select (varExp tuple, offset))
		   | Tuple xs =>
			if Value.typeIsFirstOrder ty
			   then (new (); ())
		      else set (Value.tuple (Vector.map (xs, varExp)))
		   | Var x => set (varExp x)
	       end
	    and loopLambda (lambda: Lambda.t, x: Var.t): Value.t =
	       let
		  val _ = List.push (allLambdas, lambda)
		  val {arg, argType, body, ...} = Lambda.dest lambda
		  val _ =
		     setLambdaInfo
		     (lambda,
		      LambdaInfo.T {con = ref Con.bogus,
				    frees = ref (Vector.new0 ()),
				    name = Func.newString (Var.originalName x),
				    recs = ref (Vector.new0 ()),
				    ty = ref NONE})
		  val _ = newVar (arg, Value.fromType argType)
	       in
		  Value.lambda (lambda,
				Type.arrow (argType, Value.ty (loopExp body)))
	       end
	    val _ =
	       Control.trace (Control.Pass, "flow analysis")
	       loopExp body
	 in ()
	 end
      val _ =
	 Control.diagnostics
	 (fn display =>
	  Sexp.foreachBoundVar
	  (body, fn (x, _, _) => display (let open Layout
					  in seq [Var.layout x,
						  str " ",
						  Value.layout (value x)]
					  end)))
      (* Find the overflow exception. *)
      val overflowVar =
	 DynamicWind.withEscape
	 (fn escape =>
	  (Sexp.foreachPrimExp
	   (body, fn (x, e) =>
	    case e of
	       SprimExp.ConApp {con, ...} =>
		  if Con.equals (con, Con.overflow)
		     then escape x
		  else ()
	     | _ => ())
	   ; Error.bug "couldn't find overflow exception"))
      val _ =
	 Control.trace (Control.Pass, "free variables")
	 LambdaFree.lambdaFree
	 (program,
	  overflowVar,
	  fn x => let val {frees, status, ...} = varInfo x
		  in {frees = frees, status = status}
		  end,
          fn l => let val LambdaInfo.T {frees, recs, ...} = lambdaInfo l
		  in {frees = frees, recs = recs}
		  end)
      val _ =
	 Control.trace (Control.Pass, "globalize")
	 Globalize.globalize {program = program,
			      lambdaFree = LambdaInfo.frees o lambdaInfo,
			      varGlobal = #isGlobal o varInfo}
      local
	 fun removeGlobal v = Vector.keepAll (v, not o isGlobal)
	 val _ =
	    List.foreach (!allLambdas, fn l =>
			  let
			     val LambdaInfo.T {frees, recs, ...} = lambdaInfo l
			  in
			     frees := removeGlobal (!frees)
			     ; recs := removeGlobal (!recs)
			  end)
      in
      end
      val {get = lambdasInfoOpt} =
	 Property.get (Lambdas.plist, Property.initFun (fn _ => ref NONE))
      val {hom = convertType, destroy = destroyConvertType} =
	 Stype.makeMonoHom {con = fn (_, c, ts) => Ctype.con (c, ts)}
      fun convertTypes ts = List.map (ts, convertType)
      (* newDatatypes accumulates the new datatypes built for sets of lambdas. *)
      val newDatatypes = ref []
      fun valueType arg: Ctype.t =
	 Trace.traceInfo (valueTypeInfo,
			  Layout.ignore,
			  Ctype.layout,
			  Trace.assertTrue)
	 (fn (v: Value.t) =>
	 let
	    val r = Value.cpsType v
	 in
	    case !r of
	       SOME t => t
	     | NONE =>
		  let
		     val t = 
			case Value.dest v of
			   Value.Type t => convertType t
			 | Value.Ref v => Ctype.reff (valueType v)
			 | Value.Array v => Ctype.array (valueType v)
			 | Value.Vector v => Ctype.vector (valueType v)
			 | Value.Tuple vs =>
			      Ctype.tuple (Vector.map (vs, valueType))
			 | Value.Lambdas ls => #ty (lambdasInfo ls)
		  in r := SOME t; t
		  end
	 end) arg
      and lambdasInfo (ls: Lambdas.t): {cons: {lambda: Slambda.t,
					       con: Con.t} vector,
					ty: Ctype.t} =
	 let
	    val r = lambdasInfoOpt ls
	 in
	    case !r of
	       SOME info => info
	     | NONE => 
		  let
		     val tycon = Tycon.newString "lambdas"
		     val cons =
			Vector.fromListMap
			(Lambdas.toList ls, fn l =>
			 {lambda = Value.Lambda.dest l,
			  con = Con.newString "Env"})
		     val ty = Ctype.con (tycon, Vector.new0 ())
		     val info = {ty = ty, cons = cons}
		     val _ = r := SOME info
		     (* r must be set before the following, because calls to
		      * lambdaInfoType may refer to the type of this lambdasInfo.
		      *)
		     val cons =
			Vector.map
			(cons, fn {con, lambda} =>
			 {con = con,
			  args = Vector.new1 (lambdaInfoType
					      (lambdaInfo lambda))})
		     val _ = List.push (newDatatypes, {tycon = tycon,
						       cons = cons})
		  in
		     info
		  end
	 end
      and varInfoType ({value, ...}: VarInfo.t) = valueType value
      and lambdaInfoType (LambdaInfo.T {frees, ty, ...}): Ctype.t =
	 case !ty of
	    NONE =>
	       let val t = Ctype.tuple (Vector.map
					(!frees, varInfoType o varInfo))
	       in ty := SOME t; t
	       end
	  | SOME t => t
      fun valueLambdasInfo v =
 	 case Value.dest v of
	    Value.Lambdas l => lambdasInfo l
	  | _ => Error.bug "valueLambdasInfo of non-lambda"
      val varLambdasInfo = valueLambdasInfo o value
      val emptyTypes = Vector.new0 ()
      val datatypes =
	 Vector.map
	 (datatypes, fn {tycon, cons, ...} =>
	  {tycon = tycon,
	   cons = (Vector.map
		   (cons, fn {con, ...} =>
		    {con = con,
		     args = (case conArg con of
				NONE => emptyTypes
			      | SOME v => Vector.new1 (valueType v))}))})
      (* Variable renaming *)
      val getNewVar = ! o #replacement o varInfo
      fun newVarInfo (x: Var.t, {isGlobal, replacement, ...}: VarInfo.t): Var.t =
	 if !isGlobal
	    then x
	 else let val x' = Var.new x
	      in replacement := x'; x'
	      end
      fun newVar x = newVarInfo (x, varInfo x)
      val newVar = Trace.trace ("newVar", Var.layout, Var.layout) newVar
      fun newScope (xs: Var.t vector, f: Var.t vector -> 'a): 'a =
	 let
	    val old = Vector.map (xs, ! o #replacement o varInfo)
	    val res = f (Vector.map (xs, newVar))
	    val _ = Vector.foreach2 (xs, old, fn (x, x') =>
				     #replacement (varInfo x) := x')
	 in
	    res
	 end
      (*------------------------------------*)
      (*               coerce               *)
      (*------------------------------------*)
      val traceCoerce =
	 Trace.trace3 ("coerce", Cexp.layout, Value.layout, Value.layout,
		       Cexp.layout)
      (*       val traceCoerceTuple =
       * 	 let val layoutValues = List.layout (", ", Value.layout)
       * 	 in Trace.trace3 ("coerceTuple", Cexp.layout,
       * 			 layoutValues, layoutValues, Cexp.layout)
       * 	 end
       *)
      fun coerce arg: Cexp.t =
	 traceCoerce
	 (fn (e: Cexp.t, from: Value.t, to: Value.t) =>
	  if Value.equals (from, to)
	     then e
	  else 
	     case (Value.dest from, Value.dest to) of
		(Value.Tuple vs, Value.Tuple vs') =>
		   coerceTuple (e, valueType from, vs, valueType to, vs')
	      | (Value.Lambdas ls, Value.Lambdas ls') =>
		   if Lambdas.equals (ls, ls')
		      then e
		   else
		      let
			 val {cons, ...} = lambdasInfo ls
			 val {cons = cons', ty, ...} = lambdasInfo ls'
			 val _ =
			    Vector.foreach
			    (cons', fn {lambda, con, ...} =>
			     let
				val LambdaInfo.T {con = r, ...} =
				   lambdaInfo lambda
			     in r := con
			     end)
			 val exp =
			    Cexp.casee
			    {cause = Cause.Coerce,
			     test = e, default = NONE,
			     ty = ty,
			     cases =
			     Cexp.Con
			     (Vector.map
			      (cons, fn {lambda, con} =>
			       let
				  val info as LambdaInfo.T {con = r, ...} =
				     lambdaInfo lambda
				  val tuple = (Var.newNoname (),
					       lambdaInfoType info)
			       in {con = con,
				   args = Vector.new1 tuple,
				   body = (Cexp.conApp
					   {con = !r,
					    ty = ty,
					    args =
					    Vector.new1 (Cexp.var tuple)})}
			       end))}
		      in exp
		      end
	      | _ => Error.bug "impossible coercion") arg
      and coerceTuple arg =
	 (*	 traceCoerceTuple *)
	 (fn (e: Cexp.t,
	      ty: Ctype.t, vs: Value.t vector,
	      ty': Ctype.t, vs': Value.t vector) =>
	  if Ctype.equals (ty, ty')
	     then e
	  else 
	     Cexp.detuple
	     {tuple = e,
	      body =
	      fn components => 
	      Cexp.tuple
	      {exps = Vector.map3 (components, vs, vs',
				   fn (x, v, v') =>
				   coerce (Cexp.var (x, valueType v), v, v')),
	       ty = ty'}}) arg
      fun convertVarInfo (info as {replacement, ...}: VarInfo.t) =
	 Cexp.var (!replacement, varInfoType info)
      val convertVar = convertVarInfo o varInfo
      val convertVarExp = convertVar o SvarExp.var
      (*------------------------------------*)      	       
      (*               apply                *)
      (*------------------------------------*)
      fun apply {func, arg, resultVal}: Cexp.t =
	 let
	    val func = varExpInfo func
	    val arg = varExpInfo arg
	    val funcVal = VarInfo.value func
	    val argVal = VarInfo.value arg
	    val argExp = convertVarInfo arg
	    val ty = valueType resultVal
	    val {cons, ...} = valueLambdasInfo funcVal
	 in Cexp.casee
	    {cause = Cause.Dispatch,
	     test = convertVarInfo func,
	     ty = ty,
	     default = NONE,
	     cases =
	     Cexp.Con
	     (Vector.map
	      (cons, fn {lambda, con} =>
	       let
		  val {arg = param, body, ...} = Slambda.dest lambda
		  val info as LambdaInfo.T {name, ...} = lambdaInfo lambda
		  val result = expValue body
		  val env = (Var.newString "env", lambdaInfoType info)
	       in {con = con,
		   args = Vector.new1 env,
		   body = coerce (Cexp.call
				  {func = name,
				   args = Vector.new2 (Cexp.var env,
						       coerce (argExp, argVal,
							       value param)),
				   ty = valueType result},
				  result, resultVal)}
	       end))}
	 end
      (*------------------------------------*)
      (*             convertExp             *)
      (*------------------------------------*)
      fun lambdaInfoTuple (info as LambdaInfo.T {frees, ...}): Cexp.t =
	 Cexp.tuple {exps = Vector.map (!frees, convertVar),
		     ty = lambdaInfoType info}
      fun recursives (old: Var.t vector, new: Var.t vector, env) =
	 Vector.fold2
	 (old, new, [], fn (old, new, ac) =>
	  let
	     val {cons, ty, ...} = varLambdasInfo old
	  in
	     if 1 = Vector.length cons
		then
		   let val {con, ...} = Vector.sub (cons, 0)
		   in
		      {var = new,
		       ty = ty,
		       exp = Cexp.conApp {con = con, ty = ty,
					  args = Vector.new1 (Cexp.var env)}}
		      :: ac
		   end
	     else Error.bug "val rec lambda must be singleton"
	  end)
      val recursives =
	 Trace.trace ("recursives",
		      fn (a, b, _) =>
		      Layout.tuple [Vector.layout Var.layout a,
				    Vector.layout Var.layout b],
		      Layout.ignore)
	 recursives
      val shrinkExp = Cps.shrinkExp (Vector.new0 ())
      (* Closure convert an expression, returning:
       *   - the target cps expression
       *   - a list of global declarations (in order)
       *   - a list of function declarations
       * Accumulate the globals onto the end of the given ones.
       *)
      fun convertExp (e: Sexp.t, ac: Accum.t): Cexp.t * Accum.t =
	 let
	    val {decs, result} = Sexp.dest e
	    (* Process decs left to right, since bindings of variables
	     * must be visited before uses.
	     *)
	    val (decs, ac) =
	       List.fold
	       (decs, ([], ac), fn (d, (binds, ac)) =>
		case d of
		   Sdec.MonoVal {var, ty, exp} =>
		      let
			 val info as {isGlobal, value, ...} = varInfo var
			 val (exp, ac) = convertPrimExp (exp, value, ac)
			 val bind = {var = newVarInfo (var, info),
				     ty = valueType value,
				     exp = exp}
		      in if !isGlobal
			    then (binds, Accum.addGlobal (ac, bind))
			 else (bind :: binds, ac)
		      end
		 | Sdec.Fun {decs, ...} =>
		      if Vector.isEmpty decs
			 then (binds, ac)
		      else
			 let 
			    val {var, ty, lambda} = Vector.sub (decs, 0)
			    val info = lambdaInfo lambda
			    val tupleVar = Var.newString "tuple"
			    val tupleTy = lambdaInfoType info
			    val binds' =
			       {var = tupleVar,
				ty = tupleTy,
				exp = lambdaInfoTuple info}
			       :: (recursives
				   (Vector.map (decs, #var),
				    Vector.map (decs, newVar o #var),
				    (tupleVar, tupleTy)))
			    val (binds, ac) =
			       if isGlobal var
				  then (binds, Accum.addGlobals (ac, binds'))
			       else (List.fold (binds', binds, op ::), ac)
			 in (binds,
			     Vector.fold (decs, ac, fn ({lambda, ...}, ac) =>
					  convertLambda (lambda,
							 lambdaInfo lambda,
							 ac)))
			 end
		 | _ => Error.bug "closure convert saw strange dec")
	 in (Cexp.lett {decs = List.rev decs,
			body = convertVarExp result},
	     ac)
	 end
      and convertPrimExp arg : Cexp.t * Accum.t =
	 Trace.traceInfo (convertPrimExpInfo,
			  SprimExp.layout o #1,
			  Layout.ignore,
			  Trace.assertTrue)
	 (fn (e: SprimExp.t, v: Value.t, ac: Accum.t) =>
	 let
	    val ty = valueType v
	    fun convertJoin (e, ac) =
	       let val (e', ac) = convertExp (e, ac)
	       in (coerce (e', expValue e, v), ac)
	       end
	    fun simple e = (e, ac)
	 in case e of
	    SprimExp.Var y => simple (convertVarExp y)
	  | SprimExp.Const c => simple (Cexp.const c)
	  | SprimExp.PrimApp {prim, targs, args} =>
	       let
		  open Prim.Name
		  fun arg i = Vector.sub (args, i)
		  val v1 = Vector.new1
		  val v2 = Vector.new2
		  val v3 = Vector.new3
		  val (targs, args) =
		     case Prim.name prim of
			Array_update =>
			   let
			      val a = varExpInfo (arg 0)
			      val y = varExpInfo (arg 2)
			      val v = Value.dearray (VarInfo.value a)
			   in (v1 (valueType v),
			       v3 (convertVarInfo a,
				   convertVarExp (arg 1),
				   coerce (convertVarInfo y,
					   VarInfo.value y, v)))
			   end
		      | Ref_assign =>
			   let
			      val r = varExpInfo (arg 0)
			      val y = varExpInfo (arg 1)
			      val v = Value.deref (VarInfo.value r)
			   in (v1 (valueType v),
			       v2 (convertVarInfo r,
				   coerce (convertVarInfo y,
					   VarInfo.value y, v)))
			   end
		      | Ref_ref =>
			   let
			      val y = varExpInfo (arg 0)
			      val v = Value.deref v
			   in (v1 (valueType v),
			       v1 (coerce
				   (convertVarInfo y, VarInfo.value y, v)))
			   end
		      | MLton_serialize =>
			   let
			      val y = varExpInfo (arg 0)
			      val v = Value.serialValue (Vector.sub (targs, 0))
			   in (v1 (valueType v),
			       v1 (coerce
				   (convertVarInfo y, VarInfo.value y, v)))
			   end
		      | _ =>
			   let
			      val args = Vector.map (args, varExpInfo)
			   in (Prim.extractTargs
			       {prim = prim,
				args = Vector.map (args, varInfoType),
				result = ty,
				dearray = Ctype.dearray,
				deref = Ctype.deref,
				devector = Ctype.devector},
			       Vector.map (args, convertVarInfo))
			   end
		  val overflow =
		     if Prim.mayOverflow prim
			then SOME (Cexp.raisee (convertVar overflowVar))
		     else NONE
	       in simple (Cexp.primApp' {prim = prim,
					 overflow = overflow,
					 ty = ty,
					 targs = targs,
					 args = args})
	       end
	  | SprimExp.Tuple xs =>
	       simple (Cexp.tuple {exps = Vector.map (xs, convertVarExp),
				   ty = ty})
	  | SprimExp.Select {tuple, offset} =>
	       simple (Cexp.select {tuple = convertVarExp tuple,
				    offset = offset,
				    ty = ty})
	  | SprimExp.ConApp {con = con, arg, ...} =>
	       simple
	       (Cexp.conApp
		{con = con,
		 ty = ty,
		 args = (case (arg, conArg con) of
			    (NONE, NONE) => Vector.new0 ()
			  | (SOME arg, SOME conArg) =>
			       let
				  val arg = varExpInfo arg
				  val argVal = VarInfo.value arg
				  val arg = convertVarInfo arg
			       in if Value.equals (argVal, conArg)
				     then Vector.new1 arg
				  else Vector.new1 (coerce (arg, argVal, conArg))
			       end
			  | _ => Error.bug "constructor mismatch")})
	  | SprimExp.Raise y => simple (Cexp.raisee (convertVarExp y))
	  | SprimExp.Handle {try, catch = (catch, _), handler} =>
	       let
		  val catchInfo = varInfo catch
		  val (try, ac) = convertJoin (try, ac)
		  val catch = (newVarInfo (catch, catchInfo),
			       varInfoType catchInfo)
		  val (handler, ac) = convertJoin (handler, ac)
	       in (Cexp.handlee {try = try, ty = ty,
				 catch = catch, handler = handler},
		   ac)
	       end
	  | SprimExp.Case {test, cases, default} =>
	       let
		  val (default, ac) =
		     case default of
			NONE => (NONE, ac)
		      | SOME e => let val (e, ac) =  convertJoin (e, ac)
				  in (SOME e, ac)
				  end
		  fun doCases (cases, finish, make) =
		     let
			val (cases, ac) =
			   Vector.mapAndFold
			   (cases, ac, fn ((x, e), ac) =>
			    let
			       val make = make x
			       val (body, ac) = convertJoin (e, ac)
			    in (make body, ac)
			    end)
		     in (finish cases, ac)
		     end
		  fun doit (l, f) = doCases (l, f, fn i => fn e => (i, e))
		  val (cases, ac) =
		     case cases of
			Scases.Char l => doit (l, Cexp.Char)
		      | Scases.Con cases =>
			   doCases
			   (cases, Cexp.Con,
			    fn Spat.T {con, arg, ...} =>
			    let
			       val args =
				  case (conArg con, arg) of
				     (NONE, NONE) => Vector.new0 ()
				   | (SOME v, SOME (arg, _)) =>
					Vector.new1 (newVar arg, valueType v)
				   | _ => Error.bug "constructor mismatch"
			    in fn body => {con = con, args = args, body = body}
			    end)
		      | Scases.Int l => doit (l, Cexp.Int)
		      | Scases.Word l => doit (l, Cexp.Word)
		      | Scases.Word8 l => doit (l, Cexp.Word8)
	       in (Cexp.casee
		   {cause = Cause.User,
		    test = convertVarExp test,
		    ty = ty, cases = cases, default = default},
		   ac)
	       end
	  | SprimExp.Lambda l =>
	       let
		  val info = lambdaInfo l
		  val ac = convertLambda (l, info, ac)
		  val {cons, ...} = valueLambdasInfo v
	       in case Vector.peek (cons, fn {con, lambda = l'} =>
				    Slambda.equals (l, l')) of
		  NONE => Error.bug "lambda must exist in its own set"
		| SOME {con, ...} =>
		     (Cexp.conApp {con = con, ty = ty,
				   args = Vector.new1 (lambdaInfoTuple info)},
		      ac)
	       end
	  | SprimExp.App {func, arg} =>
	       (apply {func = func, arg = arg, resultVal = v},
		ac)
	 end) arg
      and convertLambda (lambda: Slambda.t,
			 info as LambdaInfo.T {frees, name, recs, ...},
			 ac: Accum.t): Accum.t =
	 let
	    val {arg = argVar, body, ...} = Slambda.dest lambda
	    val argVarInfo = varInfo argVar
	    val env = (Var.newString "env", lambdaInfoType info)
	    val args = Vector.new2 (env,
				    (newVarInfo (argVar, argVarInfo),
				     varInfoType argVarInfo))
	    val returns = Vector.new1 (valueType (expValue body))
	    val recs = !recs
	 in newScope
	    (!frees, fn components =>
	     newScope
	     (recs, fn recs' =>
	      let
		 val decs = recursives (recs, recs', env)
		 val (body, ac) = convertExp (body, ac)
	      in Accum.addFunc
		 (ac,
		  Cps.Function.T
		  {name = name, 
		   args = args,
		   returns = returns,
		   body =
		   shrinkExp
		   (Cexp.toExp
		    (Cexp.lett
		     {decs = decs,
		      body = Cexp.detupleBind {tuple = Cexp.var env,
					       components = components,
					       body = body}}))})
	      end))
	 end
      (*------------------------------------*)
      (*    main body of closure convert    *)
      (*------------------------------------*)
      val main = Func.newString "main"
      val {functions, globals} =
	 Control.trace (Control.Pass, "convert")
	 (fn () =>
	  let
	     val (body, ac) = convertExp (body, Accum.empty)
	     val ac =
		Accum.addFunc
		(ac,
		 Cps.Function.T
		 {name = main,
		  args = Vector.new0 (),
		  returns = Vector.new1 Ctype.unit,
		  body = Cps.shrinkExp (Vector.new0 ()) (Cexp.toExp body)})
	  in Accum.done ac
	  end) ()
      val datatypes = Vector.concat [datatypes, Vector.fromList (!newDatatypes)]
      val program =
	 Cprogram.T {datatypes = datatypes,
		     globals = Vector.fromList globals,
		     functions = Vector.fromList functions,
		     main = main}
      val _ = destroyConvertType ()
      val _ = Value.destroy ()
      val _ = Cprogram.clear program
   in
      program
   end

end
