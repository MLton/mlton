(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor TypeCheck (S: TYPE_CHECK_STRUCTS): TYPE_CHECK = 
struct

open S
open XmlTree   
open Dec PrimExp

fun typeCheck (program as Program.T {datatypes, body, overflow}): unit =
   let
      (* tyvarInScope is used to ensure that tyvars never shadow themselves. *)
      val {get = tyvarInScope: Tyvar.t -> bool ref, ...} =
	 Property.get (Tyvar.plist,
		       Property.initFun (fn _ => ref false))
      fun bindTyvars (vs: Tyvar.t vector): unit =
	 Vector.foreach (vs, fn v =>
			 let
			    val r = tyvarInScope v
			 in
			    if !r
			       then Type.error ("tyvar already in scope",
						Tyvar.layout v)
			    else r := true
			 end)
      fun unbindTyvars (vs: Tyvar.t vector): unit =
	 Vector.foreach (vs, fn v => tyvarInScope v := false)
      (* checkType makes sure all tyvars are bound. *)
      fun checkType (ty: Type.t): unit =
	 Type.hom {ty = ty,
		   con = fn _ => (),
		   var = fn a => if !(tyvarInScope a)
				    then ()
				 else Type.error ("tyvar not in scope",
						  Tyvar.layout a)}
      fun checkTypes v = Vector.foreach (v, checkType)
      val {get = getCon: Con.t -> {tyvars: Tyvar.t vector, ty: Type.t},
	   set, ...} =
	 Property.getSetOnce (Con.plist,
			      Property.initRaise ("scheme", Con.layout))
      fun setCon ({con, arg}, tyvars, result: Type.t): unit =
	 (checkType result
	  ; set (con, {tyvars = tyvars,
		       ty = (case arg of
				NONE => result
			      | SOME ty => Type.arrow (ty, result))}))
      fun checkConExp (c: Con.t, ts: Type.t vector): Type.t =
	 let
	    val _ = checkTypes ts
	    val {tyvars, ty} = getCon c
	 in
	    Type.substitute (ty, Vector.zip (tyvars, ts))
	 end
      val {get = getVar: Var.t -> {tyvars: Tyvar.t vector, ty: Type.t},
	   set = setVar, ...} =
	 Property.getSetOnce (Var.plist,
			      Property.initRaise ("var scheme", Var.layout))
      (* val getVar = Trace.trace ("getVar", Var.layout, Layout.ignore) getVar *)
      (* val setVar = Trace.trace2 ("setVar", Var.layout, Layout.ignore, Layout.ignore) setVar *)
      fun checkVarExp (VarExp.T {var, targs}): Type.t =
	 let
	    val _ = checkTypes targs
	    val {tyvars, ty} = getVar var
	 in if Vector.length targs = Vector.length tyvars
	       then Type.substitute (ty, Vector.zip (tyvars, targs))
	    else
	       Type.error ("variable applied to wrong number of type args",
			  let open Layout
			  in align [Var.layout var,
				   seq [str "tyvars: ",
					Vector.layout Tyvar.layout tyvars],
				   seq [str "targs: ",
					Vector.layout Type.layout targs]]
			    end)
	 end
      fun checkVarExps xs = Vector.map (xs, checkVarExp)
      fun checkPat (p as Pat.T {con, targs, arg}): Type.t =
	 let
	    val t = checkConExp (con, targs)
	 in
	    case (arg, Type.dearrowOpt t) of
	         (NONE, NONE) => t
	       | (SOME (x, ty), SOME (t1, t2)) =>
		    (checkType ty
		     ; if Type.equals (t1, ty)
			  then (setVar (x, {tyvars = Vector.new0 (),
					    ty = t1}) ; t2)
		       else (Type.error
			     ("argument constraint of wrong type",
			      let open Layout
			      in align [seq [str "t1: ", Type.layout t1],
					seq [str "ty: ", Type.layout ty],
					seq [str "p: ", Pat.layout p]]
			      end)))
	       | _ => Type.error ("constructor pattern mismatch", Pat.layout p)
	 end
      val traceCheckExp =
 	 Trace.trace ("Xml.checkExp", Exp.layout, Type.layout)
      val traceCheckPrimExp = 
 	 Trace.trace2
 	 ("Xml.checkPrimExp", PrimExp.layout, Type.layout, Type.layout)
      local
	 val exnType = ref NONE
      in
	 fun isExnType t =
	    case !exnType of
	       NONE => (exnType := SOME t; true)
	     | SOME t' => Type.equals (t, t')
      end
      fun check (t: Type.t, t': Type.t, layout: unit -> Layout.t): unit =
	 if Type.equals (t, t')
	    then ()
	 else Type.error ("type mismatch",
			 Layout.align [Type.layout t,
				      Type.layout t'])
      fun checkExp arg: Type.t =
	 traceCheckExp
	 (fn (exp: Exp.t) =>
	  let val {decs, result} = Exp.dest exp
	  in List.foreach (decs, checkDec)
	     ; checkVarExp result
	  end) arg
      and checkPrimExp arg: Type.t =
	 traceCheckPrimExp
	 (fn (e: PrimExp.t, ty: Type.t) => 
	 let
	    fun error msg =
	       Type.error (msg, let open Layout
			       in seq [str "exp: ", PrimExp.layout e]
			       end)
	    fun checkApp (t1, x) =
	       let val t2 = checkVarExp x
	       in case Type.dearrowOpt t1 of
		  SOME (t2', t3) =>
		     if Type.equals (t2, t2') then t3
		     else
			Type.error
			("actual and formal not of same type",
			 let open Layout
			 in align [seq [str "actual: ", Type.layout t2],
				   seq [str "formal: ", Type.layout t2'],
				   seq [str "expression: ",
					PrimExp.layout e]]
			 end)
		| NONE => error "function not of arrow type"
	       end
	    fun checkApps (t, es) =
	       List.fold (es, t, fn (e, t) => checkApp (t, e))
	 in
	    case e of
	       Var x => checkVarExp x
	     | Const c => Type.ofConst c
	     | Tuple xs =>
		  if 1 = Vector.length xs
		     then error "unary tuple"
		  else Type.tuple (checkVarExps xs)
	     | Select {tuple, offset} =>
		  (case Type.detupleOpt (checkVarExp tuple) of
		      SOME ts => Vector.sub (ts, offset)
		    | NONE => error "selection from nontuple")
	     | Lambda l => checkLambda l
	     | PrimApp {prim, targs, args} =>
		  let
		     val _ = checkTypes targs
		  in
		     case Prim.checkApp {prim = prim,
					 targs = targs,
					 args = checkVarExps args,
					 con = Type.con,
					 equals = Type.equals,
					 dearrowOpt = Type.dearrowOpt,
					 detupleOpt = Type.detupleOpt,
					 isUnit = Type.isUnit
					 } of
			NONE => error "bad primapp"
		      | SOME t => t
		  end
	     | ConApp {con, targs, arg} =>
		  let
		     val t = checkConExp (con, targs)
		  in case arg of
		     NONE => t
		   | SOME e => checkApp (t, e)
		  end
	     | App {func, arg} => checkApp (checkVarExp func, arg)
	     | Raise {exn, ...} => if isExnType (checkVarExp exn)
				      then ty
				   else error "bad raise"
	     | Handle {try, catch = (catch, catchType), handler, ...} =>
		  let
		     val _ = if isExnType catchType
				then ()
			     else error "handle with non-exn type for catch"
		     val ty = checkExp try
		     val _ = setVar (catch, {tyvars = Vector.new0 (),
					     ty = catchType})
		     val ty' = checkExp handler
		  in if Type.equals (ty, ty') then ty
		     else error "bad handle"
		  end
	     | Case {test, cases, default} =>
		  let
		     val ty = checkVarExp test
		     fun doit (l, t) =
			(Vector.new1 t, Vector.map (l, fn (_, e) => checkExp e))
		     fun equalss v =
			if Vector.isEmpty v
			   then Error.bug "equalss"
			else
			   let
			      val t = Vector.sub (v, 0)
			   in
			      if Vector.forall (v, fn t' => Type.equals (t, t'))
				 then SOME t
			      else NONE
			   end
		     datatype z = datatype Cases.t
		     val (ptys, etys) =
			case cases of
			   Char l => doit (l, Type.char)
			 | Con cases =>
			      Vector.unzip
			      (Vector.map (cases, fn (p, e) =>
					   (checkPat p, checkExp e)))
			 | Int l => doit (l, Type.int)
			 | Word l => doit (l, Type.word)
			 | Word8 l => doit (l, Type.word8)
		  in case (equalss ptys, equalss etys) of
		     (NONE, _) => error "patterns not of same type"
		   | (_, NONE) => error "branches not of same type"
		   | (SOME pty, SOME ety) =>
			if Type.equals (ty, pty)
			   then
			      case default of
				 NONE => ety
			       | SOME (e, _) =>
				    if Type.equals (ety, checkExp e)
				       then ety
				    else error "default of wrong type"
			else error "test and patterns of different types"
		  end
	 end) arg
      and checkLambda l: Type.t =
	 let
	    val {arg, argType, body, ...} = Lambda.dest l
	    val _ = checkType argType
	    val _ = setVar (arg, {tyvars = Vector.new0 (), ty = argType})
	 in
	    Type.arrow (argType, checkExp body)
	 end
      and checkDec d =
	 let
	    val check = fn (t, t') => check (t, t', fn () => Dec.layout d)
	 in
	    case d of
	       Exception c => setCon (c, Vector.new0 (), Type.exn)
	     | MonoVal {var, ty, exp} =>
		  (checkType ty
		   ; check (ty, checkPrimExp (exp, ty))
		   ; setVar (var, {tyvars = Vector.new0 (), ty = ty}))
	     | PolyVal {tyvars, var, ty, exp} =>
		  (bindTyvars tyvars
		   ; checkType ty
		   ; if Vector.isEmpty tyvars
			then Error.bug "empty tyvars in PolyVal dec"
		     else ()
			; check (ty, checkExp exp)
			; unbindTyvars tyvars
			; setVar (var, {tyvars = tyvars, ty = ty}))
	     | Fun {tyvars, decs} =>
		  (bindTyvars tyvars
		   ; (Vector.foreach
		      (decs, fn {var, ty, lambda} =>
		       (checkType ty
			; setVar (var, {tyvars = tyvars, ty = ty}))))
		   ; Vector.foreach (decs, fn {ty, lambda, ...} =>
				     check (ty, checkLambda lambda))
		   ; unbindTyvars tyvars)
	 end
      val _ =
	 Vector.foreach
	 (datatypes, fn {tycon, tyvars, cons} =>
	  let
	     val _ = bindTyvars tyvars
	     val ty = Type.con (tycon, Vector.map (tyvars, Type.var))
	     val _ = Vector.foreach (cons, fn c => setCon (c, tyvars, ty))
	     val _ = unbindTyvars tyvars
	  in
	     ()
	  end)
      val _ =
	 if Type.equals (checkExp body, Type.unit)
	    then ()
	 else Error.bug "program must be of type unit"
      val _ =
	 case overflow of
	    NONE => true
	  | SOME x =>
	       let val {tyvars, ty} = getVar x
	       in
		  0 = Vector.length tyvars
		  andalso Type.equals (ty, Type.exn)
	       end
      val _ = Program.clear program
   in
      ()
   end

val typeCheck =
   Trace.trace ("Xml.typeCheck", Program.layout, Unit.layout) typeCheck

end
