(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor XmlTree (S: XML_TREE_STRUCTS): XML_TREE =
struct

open S
local open Ast
in
   structure Adec = Dec
   structure Aexp = Exp
   structure Amatch = Match
   structure Apat = Pat
end

structure Type =
   struct
      structure T = HashType (S)
      open T

      datatype dest =
	 Var of Tyvar.t
       | Con of Tycon.t * t vector

      fun dest t =
	 case Dest.dest t of
	    Dest.Var a => Var a
	  | Dest.Con x => Con x
   end

structure Pat =
   struct
      structure Apat = Ast.Pat
	 
      datatype t = T of {con: Con.t,
			 targs: Type.t vector,
			 arg: (Var.t * Type.t) option}
	 
      fun con (T {con, ...}) = con

      local fun make c = T {con = c, targs = Vector.new0 (), arg = NONE}
      in val truee = make Con.truee
	 val falsee = make Con.falsee
      end

      fun toAst (T {con, arg, ...}) =
	 let val con = Con.toAst con
	 in case arg of
	    NONE => Apat.con con
	  | SOME (x, t) =>
	       Apat.app (con, Apat.var (Var.toAst x))
	 (*Apat.app (con, Apat.constraint (Apat.var (Var.toAst x), Type.toAst t)) *)
	 end

      val layout = Apat.layout o toAst
   end

structure Cases = Cases (type con = Pat.t)

(*---------------------------------------------------*)
(*                      VarExp                       *)
(*---------------------------------------------------*)

structure VarExp =
   struct
      datatype t = T of {var: Var.t,
			 targs: Type.t vector}

      fun mono var = T {var = var, targs = Vector.new0 ()}

      local
	 fun make f (T r) = f r
      in
	 val targs = make #targs
	 val var = make #var
      end
   
      val toAst = Aexp.var o Var.toAst o var

      fun toAsts (xs: t list): Aexp.t list = List.map (xs, toAst)

      fun layout (T {var, targs, ...}) =
	 if !Control.showTypes
	    then let open Layout
		 in
		    if Vector.isEmpty targs
		       then Var.layout var
		    else seq [Var.layout var,
			      Vector.layout Type.layout targs]
		 end
	 else Var.layout var
   end
   
(*---------------------------------------------------*)
(*           Expressions and Declarations            *)
(*---------------------------------------------------*)

datatype exp =
   Exp of {decs: dec list,
	   result: VarExp.t}
and primExp =
    App of {func: VarExp.t,
	    arg: VarExp.t}
  | Case of {test: VarExp.t,
	     cases: exp Cases.t,
	     default: exp option}
  | ConApp of {con: Con.t,
	       targs: Type.t vector,
	       arg: VarExp.t option}
  | Const of Const.t
  | Handle of {try: exp,
	       catch: Var.t * Type.t,
	       handler: exp}
  | Lambda of lambda
  | PrimApp of {prim: Prim.t,
		targs: Type.t vector,
		args: VarExp.t vector}
  | Raise of {exn: VarExp.t,
	      filePos: string}
  | Select of {tuple: VarExp.t,
	       offset: int}
  | Tuple of VarExp.t vector
  | Var of VarExp.t
and dec =
   MonoVal of {var: Var.t,
	       ty: Type.t,
	       exp: primExp}
  | PolyVal of {tyvars: Tyvar.t vector,
		ty: Type.t,
		var: Var.t,
		exp: exp}
  | Fun of {tyvars: Tyvar.t vector,
	    decs: {var: Var.t,
		   ty: Type.t,
		   lambda: lambda} vector}
  | Exception of {con: Con.t,
		  arg: Type.t option}
and lambda = Lam of {plist: PropertyList.t,
		     arg: Var.t,
		     argType: Type.t,
		     body: exp}

(*---------------------------------------------------*)
(*                 Conversion to Ast                 *)
(*---------------------------------------------------*)

fun expToAst (Exp {decs, result, ...}): Aexp.t =
   Aexp.lett (decsToAst decs, VarExp.toAst result)
and expsToAsts es = List.map (es, expToAst)
and decsToAst decs = Vector.fromListMap (decs, decToAst)
and decToAst d : Adec.t =
   case d of
      MonoVal {var, ty, exp} =>
	 Adec.make
	 (Adec.Val
	  {tyvars = Vector.new0 (),
	   vbs = (Vector.new1
		  {filePos = "",
		   exp = primExpToAst exp,
		   pat = if !Control.showTypes
			    then Apat.constraint (Apat.var (Var.toAst var),
						  Type.toAst ty)
			 else  Apat.var (Var.toAst var)}),
	   rvbs = Vector.new0 ()})
    | PolyVal {tyvars, var, exp, ...} =>
	 Adec.vall (tyvars, Var.toAst var, expToAst exp)
    | Fun {tyvars, decs} =>
	 Adec.make
	 (Adec.Fun
	  (tyvars,
	   Vector.map
	   (decs, fn {var, ty, lambda = Lam {arg, argType, body, ...}, ...} =>
	    {filePos = "",
	     clauses =
	     Vector.new1
	     {pats = (Vector.new2
		      (Apat.var (Var.toAst var),
		       if !Control.showTypes
			  then Apat.constraint (Apat.var (Var.toAst arg),
						Type.toAst argType)
		       else Apat.var (Var.toAst arg))),
	      resultType = SOME (Type.toAst (#2 (Type.dearrow ty))),
	      body = expToAst body}})))
    | Exception {con, arg} =>
	 Adec.exceptionn (Con.toAst con, Type.optionToAst arg)
and primExpToAst e : Aexp.t =
   case e of
      Const c => Const.toAstExp c
    | Var x => VarExp.toAst x
    | Tuple xs => Aexp.tuple (Vector.map (xs, VarExp.toAst))
    | Select {tuple, offset} =>
	 Aexp.select {tuple = VarExp.toAst tuple,
		      offset = offset}
    | Lambda lambda => Aexp.fnn (lambdaToAst lambda)
    | ConApp {con, arg, ...} =>
	 let val con = Aexp.con (Con.toAst con)
	 in case arg of
	    NONE => con
	  | SOME e => Aexp.app (con, VarExp.toAst e)
	 end
    | PrimApp {prim, args, ...} =>
	 let
	    val p = Aexp.longvid (Ast.Longvid.short
				  (Ast.Longvid.Id.fromString (Prim.toString prim)))
	 in
	    case Prim.numArgs prim of
	       NONE => p
	     | SOME _ => Aexp.app (p, Aexp.tuple (Vector.map
						  (args, VarExp.toAst)))
	 end
    | App {func, arg} => Aexp.app (VarExp.toAst func, VarExp.toAst arg)
    | Raise {exn, filePos} => Aexp.raisee {exn = VarExp.toAst exn,
					   filePos = filePos}
    | Handle {try, catch, handler} =>
	 Aexp.handlee
	 (expToAst try,
	  Amatch.T {filePos = "",
		    rules = Vector.new1 (Apat.var (Var.toAst (#1 catch)),
					 expToAst handler)})
    | Case {test, cases, default, ...} =>
	 let
	    fun doit (l, f) =
	       Vector.map (l, fn (i, exp) => (f i, expToAst exp))
	    datatype z = datatype Cases.t
	    val cases =
	       case cases of
		  Char l => doit (l, Ast.Pat.const o Ast.Const.Char)
		| Con l => Vector.map (l, fn (pat, exp) =>
				       (Pat.toAst pat, expToAst exp))
		| Int l => doit (l, Ast.Pat.const o Ast.Const.fromInt)
		| Word l => doit (l, Ast.Pat.const o Ast.Const.Word)
		| Word8 l =>
		     doit (l, Ast.Pat.const o Ast.Const.Word o Word8.toWord)
	    val cases =
	       case default of
		  NONE => cases
		| SOME e =>
		     Vector.concat [cases,
				    Vector.new1 (Ast.Pat.wild, expToAst e)]
	 in Aexp.casee (VarExp.toAst test, Amatch.T {rules = cases,
						     filePos = ""})
	 end

and lambdaToAst (Lam {arg, body, argType, ...}): Amatch.t =
   Amatch.T
   {filePos = "",
    rules = Vector.new1 ((if !Control.showTypes
			     then Apat.constraint (Apat.var (Var.toAst arg),
						   Type.toAst argType)
			  else Apat.var (Var.toAst arg), 
			     expToAst body))}

fun layoutLambda f = Aexp.layout (Aexp.fnn (lambdaToAst f))

(*---------------------------------------------------*)
(*                   Declarations                    *)
(*---------------------------------------------------*)

structure Dec =
   struct
      type exp = exp
      datatype t = datatype dec

      val toAst = decToAst
      val layout = Ast.Dec.layout o toAst
   end

(*---------------------------------------------------*)
(*                    Expressions                    *)
(*---------------------------------------------------*)

structure PrimExp =
   struct
      type exp = exp
      datatype t = datatype primExp

      val toAst = primExpToAst
      val layout = Aexp.layout o toAst
   end

structure Exp =
   struct
      datatype t = datatype exp

      val new = Exp
      fun dest (Exp r) = r
      val decs = #decs o dest
      val result = #result o dest

      fun fromPrimExp (exp: PrimExp.t, ty: Type.t): t =
	 let val var = Var.newNoname ()
	 in Exp {decs = [Dec.MonoVal {var = var, ty = ty, exp = exp}],
		 result = VarExp.mono var}
	 end
      
      local
	 fun make f (Exp {decs, result}, d) =
	    Exp {decs = f (d, decs),
		 result = result}
      in val prefix = make (op ::)
	 val prefixs = make (op @)
      end

      val toAst = expToAst
      val layout = Ast.Exp.layout o toAst

      (*------------------------------------*)
      (*              foreach               *)
      (*------------------------------------*)
      fun foreach {exp: t,
		   handleExp: t -> unit,
		   handlePrimExp: Var.t * PrimExp.t -> unit,
		   handleBoundVar: Var.t * Tyvar.t vector * Type.t -> unit,
		   handleVarExp: VarExp.t -> unit}: unit =
	 let
	    fun monoVar (x, t) = handleBoundVar (x, Vector.new0 (), t)
	    fun handleVarExps xs = Vector.foreach (xs, handleVarExp)
	    fun loopExp e =
	       let val {decs, result} = dest e
	       in List.foreach (decs, loopDec)
		  ; handleVarExp result
		  ; handleExp e
	       end
	    and loopPrimExp (x: Var.t, e: PrimExp.t): unit =
	       (handlePrimExp (x, e)
		; (case e of
		      Const _ => ()
		    | Var x => handleVarExp x
		    | Tuple xs => handleVarExps xs
		    | Select {tuple, ...} => handleVarExp tuple
		    | Lambda lambda => loopLambda lambda
		    | PrimApp {args, ...} => handleVarExps args
		    | ConApp {arg, ...} => (case arg of
					       NONE => ()
					     | SOME x => handleVarExp x)
		    | App {func, arg} => (handleVarExp func
					  ; handleVarExp arg)
		    | Raise {exn, ...} => handleVarExp exn
		    | Handle {try, catch, handler, ...} =>
			 (loopExp try
			  ; monoVar catch
			  ; loopExp handler)
		    | Case {test, cases, default} =>
			 (handleVarExp test
			  ; Cases.foreach' (cases, loopExp,
					    fn Pat.T {arg, ...} =>
					    case arg of
					       NONE => ()
					     | SOME x => monoVar x)
			  ; Option.app (default, loopExp))))
	    and loopDec d =
	       case d of
		  MonoVal {var, ty, exp} =>
		     (monoVar (var, ty); loopPrimExp (var, exp))
		| PolyVal {var, tyvars, ty, exp} =>
		     (handleBoundVar (var, tyvars, ty)
		      ; loopExp exp)
		| Exception _ => ()
		| Fun {tyvars, decs, ...} =>
		     (Vector.foreach (decs, fn {var, ty, lambda} =>
				      handleBoundVar (var, tyvars, ty))
		      ; Vector.foreach (decs, fn {lambda, ...} =>
					loopLambda lambda))
	    and loopLambda (Lam {arg, argType, body, ...}): unit =
	       (monoVar (arg, argType); loopExp body)
	 in loopExp exp
	 end

      fun ignore _ = ()

      fun foreachPrimExp (e, f) =
	 foreach {exp = e,
		  handlePrimExp = f,
		  handleExp = ignore,
		  handleBoundVar = ignore,
		  handleVarExp = ignore}

      fun foreachVarExp (e, f) =
	 foreach {exp = e,
		  handlePrimExp = ignore,
		  handleExp = ignore,
		  handleBoundVar = ignore,
		  handleVarExp = f}

      fun foreachBoundVar (e, f) =
	 foreach {exp = e,
		  handlePrimExp = ignore,
		  handleExp = ignore,
		  handleBoundVar = f,
		  handleVarExp = ignore}

      fun foreachExp (e, f) =
	 foreach {exp = e,
		  handlePrimExp = ignore,
		  handleExp = f,
		  handleBoundVar = ignore,
		  handleVarExp = ignore}

      fun hasPrim (e, f) =
	 DynamicWind.withEscape
	 (fn escape =>
	  (foreachPrimExp (e, fn (_, e) =>
			   case e of
			      PrimApp {prim, ...} => if f prim then escape true
						     else ()
			    | _ => ())
	   ; false))

      fun size e =
	 let val n: int ref = ref 0
	    fun inc () = n := 1 + !n
	 in foreachPrimExp (e, fn _ => inc ());
	    !n
	 end

      (*      val size = Trace.trace ("size", Layout.ignore, Int.layout) size *)

      fun clear (e: t): unit =
	 let open PrimExp
	    val clear = PropertyList.clear
	    fun clearTyvars ts = Vector.foreach (ts, Tyvar.clear)
	    fun clearPat (Pat.T {arg, ...}) =
	       case arg of
		  NONE => ()
		| SOME (x, _) => Var.clear x
	    fun clearExp e = clearDecs (decs e)
	    and clearDecs ds = List.foreach (ds, clearDec)
	    and clearDec d =
	       case d of
		  MonoVal {var, exp, ...} => (Var.clear var; clearPrimExp exp)
		| PolyVal {var, tyvars, exp, ...} =>
		     (Var.clear var
		      ; clearTyvars tyvars
		      ; clearExp exp)
		| Fun {tyvars, decs} =>
		     (clearTyvars tyvars
		      ; Vector.foreach (decs, fn {var, lambda, ...} =>
					(Var.clear var
					 ; clearLambda lambda)))
		| Exception {con, ...} => Con.clear con
	    and clearPrimExp e =
	       case e of
		  Lambda l => clearLambda l
		| Case {cases, default, ...} =>
		     (Cases.foreach' (cases, clearExp, clearPat)
		      ; Option.app (default, clearExp))
		| Handle {try, catch, handler, ...} => 
		     (clearExp try
		      ; Var.clear (#1 catch)
		      ; clearExp handler)
		| _ => ()
	    and clearLambda (Lam {arg, body, ...}) =
	       (Var.clear arg; clearExp body)
	 in clearExp e
	 end
   end

(*---------------------------------------------------*)
(*                      Lambda                       *)
(*---------------------------------------------------*)

structure Lambda =
   struct
      type exp = exp
      datatype t = datatype lambda

      fun arg (Lam {arg, ...}) = arg
      fun argType (Lam {argType, ...}) = argType 
      fun body (Lam {body, ...}) = body

      type node = {arg: Var.t,
		   argType: Type.t,
		   body: exp}

      fun new {arg, argType, body} =
	 Lam {plist = PropertyList.new (),
	      arg = arg, argType = argType, body = body}

      fun dest (Lam {arg, argType, body, ...}) =
	 {arg = arg, argType = argType, body = body}
	 
      fun plist (Lam {plist, ...}) = plist
	 
      val layout = layoutLambda
      fun equals (f:t, f':t) = PropertyList.equals (plist f, plist f')
   end

(* ------------------------------------------------- *)
(*                     DirectExp                     *)
(* ------------------------------------------------- *)
structure DirectExp =
   struct
      open Dec PrimExp

      structure Cont =
	 struct
	    type t = PrimExp.t * Type.t -> Exp.t

	    fun nameGen (k: VarExp.t * Type.t -> Exp.t): t =
	       fn (e, t) =>
	       case e of
		  Var x => k (x, t)
		| _ => let val x = Var.newNoname ()
		       in Exp.prefix (k (VarExp.mono x, t),
				      MonoVal {var = x, ty = t, exp = e})
		       end
		    
	    fun name (k: VarExp.t * Type.t -> Exp.t): t = nameGen k

	    val id: t = name (fn (x, _) => Exp {decs = [], result = x})
	       
	    fun return (k: t, xt) = k xt
	 end

      type t = Cont.t -> Exp.t

      fun send (e: t, k: Cont.t): Exp.t = e k

      fun toExp e = send (e, Cont.id)

      val layout = Exp.layout o toExp

      fun fromExp (Exp {decs, result}, ty): t =
	 fn k => Exp.prefixs (k (Var result, ty), decs)

      fun sendName (e, k) = send (e, Cont.name k)

      fun simple (e: PrimExp.t * Type.t) k = Cont.return (k, e)
	 
      fun const c = simple (Const c, Type.ofConst c)

      val string = const o Const.fromString
	 
      fun varExp (x, t) = simple (Var x, t)

      fun var {var, targs, ty} = varExp (VarExp.T {var = var, targs = targs}, ty)

      fun monoVar (x, t) = var {var = x, targs = Vector.new0 (), ty = t}

      fun convertsGen (es: t vector,
		       k: (VarExp.t * Type.t) vector -> Exp.t): Exp.t =
	 let
	    val n = Vector.length es
	    fun loop (i, xs) =
	       if i = n
		  then k (Vector.fromListRev xs)
	       else sendName (Vector.sub (es, i),
			      fn x => loop (i + 1, x :: xs))
	 in loop (0, [])
	 end

      fun converts (es: t vector,
		    make: (VarExp.t * Type.t) vector -> PrimExp.t * Type.t): t =
	 fn k => convertsGen (es, k o make)

      fun convert (e: t, make: VarExp.t * Type.t -> PrimExp.t * Type.t): t =
	 fn k => send (e, Cont.name (k o make))

      fun convertOpt (e, make) =
	 case e of
	    NONE => simple (make NONE)
	  | SOME e => convert (e, make o SOME o #1)

      fun tuple {exps: t vector, ty: Type.t}: t =
	 if 1 = Vector.length exps
	    then Vector.sub (exps, 0)
	 else converts (exps, fn xs =>
			(PrimExp.Tuple (Vector.map (xs, #1)), ty))

      fun select {tuple, offset, ty} =
	 convert (tuple, fn (tuple, _) =>
		  (Select {tuple = tuple, offset = offset}, ty))

      fun conApp {con, targs, arg, ty} =
	 convertOpt (arg, fn arg =>
		     (ConApp {con = con, targs = targs, arg = arg}, ty))

      local
	 fun make c () = 
	    conApp {con = c,
		    targs = Vector.new0 (),
		    arg = NONE,
		    ty = Type.bool}
      in
	 val truee = make Con.truee
	 val falsee = make Con.falsee
      end

      fun primApp {prim, targs, args, ty} =
	 converts (args, fn args =>
		   (PrimApp {prim = prim,
			     targs = targs,
			     args = Vector.map (args, #1)},
		    ty))

      fun convert2 (e1, e2, make) =
	 converts (Vector.new2 (e1, e2),
		   fn xs => make (Vector.sub (xs, 0), Vector.sub (xs, 1)))
	 
      fun app {func, arg, ty} =
	 convert2 (func, arg, fn ((func, _), (arg, _)) =>
		   (App {func = func, arg = arg}, ty))

      fun casee {test, cases, default, ty} =
	 convert (test, fn (test, _) =>
		  (Case
		   {test = test,
		    cases = Cases.map (cases, toExp),
		    default = (case default of
				  NONE => NONE
				| SOME e => SOME (toExp e))},
		   ty))

      fun raisee ({exn: t, filePos}, t: Type.t): t =
	 convert (exn, fn (x, _) => (Raise {exn = x, filePos = filePos}, t))
	 
      fun handlee {try, catch, handler, ty} =
	 simple (Handle {try = toExp try,
			 catch = catch,
			 handler = toExp handler},
		 ty)

      fun unit () = tuple {exps = Vector.new0 (), ty = Type.unit}

      fun reff (e: t): t =
	 convert (e, fn (x, t) =>
		  (PrimApp {prim = Prim.reff,
			    targs = Vector.new1 t,
			    args = Vector.new1 x},
		   Type.reff t))

      fun deref (e: t): t =
	 convert (e, fn (x, t) =>
		  let val t = Type.deref t
		  in (PrimApp {prim = Prim.deref,
			       targs = Vector.new1 t,
			       args = Vector.new1 x},
		      t)
		  end)

      fun equal (e1, e2) =
	 convert2 (e1, e2, fn ((x1, t), (x2, _)) =>
		   (PrimApp {prim = Prim.equal,
			     targs = Vector.new1 t,
			     args = Vector.new2 (x1, x2)},
		    Type.bool))

      fun iff {test, thenn, elsee, ty} =
	 casee {test = test,
		cases = Cases.Con (Vector.new2 ((Pat.truee, thenn),
						(Pat.falsee, elsee))),
		default = NONE,
		ty = ty}

      fun vall {var, exp}: Dec.t list =
	 let val t = ref Type.unit
	    val Exp {decs, result} =
	       sendName (exp, fn (x, t') => (t := t';			 
					     Exp {decs = [], result = x}))
	 in decs @ [MonoVal {var = var, ty = !t, exp = Var result}]
	 end

      fun sequence es =
    	 converts (es, fn xs => let val (x, t) = Vector.last xs
				in (Var x, t)
				end)

      fun seq (es, make) =
	 fn k => convertsGen (es, fn xts =>
			      send (make (Vector.map (xts, varExp)), k))

      fun lett {decs, body} = fn k => Exp.prefixs (send (body, k), decs)

      fun let1 {var, exp, body} =
	 fn k => 
	 send (exp, fn (exp, ty) =>
	       Exp.prefix (send (body, k),
			   Dec.MonoVal {var = var, ty = ty, exp = exp}))
	 

      fun lambda {arg, argType, body, bodyType} =
	 simple (Lambda (Lambda.new {arg = arg, argType = argType,
				     body = toExp body}),
		 Type.arrow (argType, bodyType))

      fun detupleGen (e: PrimExp.t,
		      t: Type.t,
		      components: Var.t vector,
		      body: Exp.t): Exp.t =
	 Exp.prefixs
	 (body,
	  case Vector.length components of
	     0 => []
	   | 1 => [MonoVal {var = Vector.sub (components, 0), ty = t, exp = e}]
	   | _ =>
		let
		   val ts = Type.detuple t
		   val tupleVar = Var.newNoname ()
		in MonoVal {var = tupleVar, ty = t, exp = e}
		   ::
		   #2 (Vector.fold2
		       (components, ts, (0, []),
			fn (x, t, (i, ac)) =>
			(i + 1,
			 MonoVal {var = x, ty = t,
				  exp = Select {tuple = VarExp.mono tupleVar,
						offset = i}}
			 :: ac)))
		end)
	 
      fun detupleBind {tuple, components, body} =
	 fn k => send (tuple, fn (e, t) => detupleGen (e, t, components, body k))

      fun detuple {tuple: t, body}: t =
	 fn k =>
	 tuple
	 (fn (e, t) =>
	  let
	     val ts = Type.detuple t
	  in case e of
	     Tuple xs => send (body (Vector.zip (xs, ts)), k)
	   | _ => let
		     val components = Vector.map (ts, fn _ => Var.newNoname ())
		  in detupleGen (e, t, components,
				 send (body (Vector.map2
					     (components, ts, fn (x, t) =>
					      (VarExp.mono x, t))),
				       k))
		  end
	  end)
   end

structure Exp =
   struct
      open Exp
	 
      fun unit () =
	 let open DirectExp
	 in toExp (tuple {exps = Vector.new0 (), ty = Type.unit})
	 end
   end

(*---------------------------------------------------*)
(*                     Datatype                      *)
(*---------------------------------------------------*)

structure Datatype =
   struct
      type t = {tycon: Tycon.t,
		tyvars: Tyvar.t vector,
		cons: {con: Con.t,
		       arg: Type.t option} vector}

      fun toAst ({tyvars, tycon, cons}:t) =
	 {tyvars = tyvars,
	  tycon = Tycon.toAst tycon,
	  cons = Vector.map (cons, fn {con, arg} =>
			     (Con.toAst con, Type.optionToAst arg))}
   end

(*---------------------------------------------------*)
(*                      Program                      *)
(*---------------------------------------------------*)

structure Program =
   struct
      datatype t = T of {datatypes: Datatype.t vector,
			 body: Exp.t,
			 overflow: Var.t option}

      fun size (T {body, ...}) = Exp.size body

      fun toAst (T {datatypes, body, ...}) =
	 let
	    val body = Exp.toAst body
	 in
	    if Vector.isEmpty datatypes
	       then body
	    else
	       Aexp.lett (Vector.new1
			  (Adec.datatypee (Vector.map
					   (datatypes, Datatype.toAst))),
			  body)
	 end

      fun layout (p as T {overflow, ...}) =
	 let
	    open Layout
	 in
	    align [seq [str "Overflow: ", Option.layout Var.layout overflow],
		   Ast.Exp.layout (toAst p)]
	 end

      fun clear (T {datatypes, body, ...}) =
	 (Vector.foreach (datatypes, fn {tycon, tyvars, cons} =>
			  (Tycon.clear tycon
			   ; Vector.foreach (tyvars, Tyvar.clear)
			   ; Vector.foreach (cons, Con.clear o #con)))
	  ; Exp.clear body)

      val empty = T {datatypes = Vector.new0 (),
		     body = Exp.unit (),
		     overflow = NONE}

      fun layoutStats (T {datatypes, body, ...}) =
	 let
	    val numTypes = ref 0
	    fun inc _ = numTypes := 1 + !numTypes
	    val {hom, destroy} = Type.makeHom {var = inc, con = inc}
	    val numPrimExps = ref 0
	    open Layout
	 in
	    Vector.foreach (datatypes, fn {cons, ...} =>
			    Vector.foreach (cons, fn {arg, ...} =>
					    case arg of
					       NONE => ()
					     | SOME t => hom t))
	    ; (Exp.foreach
	       {exp = body,
		handlePrimExp = fn _ => numPrimExps := 1 + !numPrimExps,
		handleVarExp = fn _ => (),
		handleBoundVar = hom o #3,
		handleExp = fn _ => ()})
	    ; destroy ()
	    ; align [seq [str "size = ", Int.layout (!numPrimExps)],
		     seq [str "num types in program = ", Int.layout (!numTypes)],
		     Type.stats ()]
	 end
   end

end
