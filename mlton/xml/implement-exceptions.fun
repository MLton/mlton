(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor ImplementExceptions (S: IMPLEMENT_EXCEPTIONS_STRUCTS):
   IMPLEMENT_EXCEPTIONS = 
struct

open S
open Dec PrimExp
structure Dexp = DirectExp

fun doit (Program.T {datatypes, body, ...}): Program.t =
   let
      (* topLevelHandler holds the ref cell containing the function of
       * type exn -> unit that should be called on unhandled exceptions.
       *)
      val topLevelHandler = Var.newNoname ()
      val exnName = Var.newString "exnName"
      (* sumType is the type of the datatype with all of the exn constructors. *)
      val {
	   dropLambda,
	   extra,
	   extraDatatypes,
	   extract,
	   extractSum,
	   inject,
	   raisee,
	   setRaise,
	   sumTycon,
	   sumType,
	   wrapBody
	   } =
	 if not (!Control.exnHistory)
	    then {
		  dropLambda = fn _ => false,
		  extra = fn _ => Error.bug "no extra",
		  extraDatatypes = Vector.new0 (),
		  extract = fn (exn, _, f) => f (Dexp.monoVar (exn, Type.exn)),
		  extractSum = fn e => e,
		  inject = fn e => e,
		  raisee = (fn {var, ty, exn, filePos} =>
			    [MonoVal {var = var, ty = ty,
				      exp = Raise {exn = exn,
						   filePos = filePos}}]),
		  setRaise = fn _ => Error.bug "no setRaise",
		  sumTycon = Tycon.exn,
		  sumType = Type.exn,
		  wrapBody = Dexp.toExp
		  }
	 else
	    let
	       val setRaiseVar = Var.newNoname ()
	       val sumTycon = Tycon.newNoname ()
	       val sumType = Type.con (sumTycon, Vector.new0 ())
	       val (extraType: Type.t, extraVar: Var.t) =
		  DynamicWind.withEscape
		  (fn escape =>
		   let
		      val _ =
			 Exp.foreachPrimExp
			 (body, fn (_, e) =>
			  case e of
			     PrimApp {prim, targs, args, ...} =>
				if Prim.name prim = Prim.Name.Exn_setInitExtra
				   then (escape
					 (Vector.sub (targs, 0),
					  VarExp.var (Vector.sub (args, 0))))
				else ()
			   | _ => ())
		   in
		      Error.bug "no Exn_setInitExtra primitive"
		   end)
	       local
		  open Type
	       in
		  val initExtraType = arrow (unit, extraType)
		  val exnCon = Con.newNoname ()
		  val exnConArgType = tuple (Vector.new2 (extraType, sumType))
		  val seType = tuple (Vector.new2 (string, exn))
		  val seuType = arrow (seType, unit)
	       end
	       val extraLambda =
		  DynamicWind.withEscape
		  (fn escape =>
		   let
		      val _ =
			 Exp.foreachPrimExp
			 (body, fn (x, e) =>
			  if Var.equals (x, extraVar)
			     then escape e
			  else ())
		   in
		      Error.bug "couldn't find extraLambda"
		   end)
	       fun dropLambda x = Var.equals (x, extraVar)
	       val initExtra = Var.newNoname ()
	       fun wrapBody body =
		   let
		      val body =
			 Dexp.let1
			 {var = setRaiseVar,
			  exp = (Dexp.reff
				 (Dexp.lambda
				  {arg = Var.newNoname (),
				   argType = seType,
				   bodyType = Type.unit,
				   body = Dexp.unit ()})),
			  body = body}
		   in Exp.prefix (Dexp.toExp body,
				  Dec.MonoVal {var = initExtra,
					       ty = initExtraType,
					       exp = extraLambda})
		   end
	       fun inject (e: Dexp.t): Dexp.t =
		  let
		     open Dexp
		     val extra =
			app {func = monoVar (initExtra, initExtraType),
			     arg = unit (),
			     ty = extraType}
		  in
		     conApp
		     {con = exnCon,
		      targs = Vector.new0 (),
		      ty = Type.exn,
		      arg = SOME (tuple
				  {exps = Vector.new2 (extra, e),
				   ty = exnConArgType})}
		  end
	       fun extractSum x =
		  Dexp.select {tuple = x, offset = 1, ty = sumType}
	       fun extract (exn: Var.t, ty, f: Dexp.t -> Dexp.t): Dexp.t =
		  let
		     open Dexp
		     val tuple = Var.newNoname ()
		  in
		     casee
		     {test = monoVar (exn, Type.exn),
		      default = NONE,
		      ty = ty,
		      cases =
		      Cases.Con (Vector.new1
				 (Pat.T {con = exnCon,
					 targs = Vector.new0 (),
					 arg = SOME (tuple, exnConArgType)},
				  f (monoVar (tuple, exnConArgType))))}
		  end
	       fun raisee {var = x, ty, exn, filePos} =
		  let
		     val exn = VarExp.var exn
		     open Dexp
		  in
		     vall
		     {var = x,
		      exp = 
		      sequence
		      (Vector.new2
		       (app {func = deref (monoVar (setRaiseVar,
						    Type.reff seuType)),
			     arg = tuple {exps = (Vector.new2
						  (string filePos,
						   monoVar (exn, Type.exn))),
					  ty = seType},
			     ty = Type.unit},
			raisee ({exn = monoVar (exn, Type.exn),
				 filePos = filePos},
				ty)))}
		  end
	       val extraDatatypes =
		  Vector.new1 {tycon = Tycon.exn,
			       tyvars = Vector.new0 (),
			       cons = Vector.new1 {con = exnCon,
						   arg = SOME exnConArgType}}
	       fun extra (x: Var.t) =
		  extract (x, extraType, fn tuple =>
			   Dexp.select {tuple = tuple,
					offset = 0,
					ty = extraType})
	       fun setRaise assign =
		  assign (setRaiseVar, seuType)
	    in
	       {
		dropLambda = dropLambda,
		extra = extra,
		extraDatatypes = extraDatatypes,
		extract = extract,
		extractSum = extractSum,
		inject = inject,
		raisee = raisee,
		setRaise = setRaise,
		sumTycon = sumTycon,
		sumType = sumType,
		wrapBody = wrapBody
		}
	    end
      val {get = exconInfo: Con.t -> {refVar: Var.t,
				      make: VarExp.t option -> Dexp.t} option,
	   set = setExconInfo, destroy} =
	 Property.destGetSetOnce (Con.plist, Property.initConst NONE)
      val setExconInfo = Trace.trace2 ("setExconInfo", Con.layout,
				       Layout.ignore, Unit.layout) setExconInfo
      val exconInfo =
	 Trace.trace ("exconInfo", Con.layout, Layout.ignore) exconInfo
      fun isExcon c =
	 case exconInfo c of
	    NONE => false
	  | SOME _ => true
      val exnValCons: {con: Con.t, arg: Type.t} list ref = ref []
      val overflow = ref NONE
      fun loopOpt e =
	 (case e of
	     NONE => NONE
	   | SOME e => SOME (loop e))
      and loops es = List.map (es, loop)
      and loop (e: Exp.t): Exp.t =
	 let val {decs, result} = Exp.dest e
	    val decs = List.concatRev (List.fold (decs, [], fn (d, ds) =>
						  loopDec d :: ds))
	 in Exp.new {decs = decs,
		     result = result}
	 end
      and loopDec (dec: Dec.t): Dec.t list =
	 case dec of
	    MonoVal b => loopMonoVal b
	  | Fun {decs, ...} =>
	       [Fun {tyvars = Vector.new0 (),
		     decs = Vector.map (decs, fn {var, ty, lambda} =>
					{var = var,
					 ty = ty,
					 lambda = loopLambda lambda})}]
	  | Exception {con, arg} =>
	       let
		  open Dexp
		  val r = Var.newString "exnRef"
		  val uniq = monoVar (r, Type.unitRef)
		  fun conApp arg =
		     inject (Dexp.conApp {con = con,
					  targs = Vector.new0 (),
					  ty = sumType,
					  arg = SOME arg})
		  val (arg, decs, make) =
		     case arg of
			NONE =>
			   (* If the exception is not value carrying, then go
			    * ahead and make it now.
			    *)
			   let
			      val exn = Var.newNoname ()
			      val _ =
				 if Con.equals (con, Con.overflow)
				    then overflow := SOME exn
				 else ()
			   in (Type.unitRef,
			       Dexp.vall {var = exn, exp = conApp uniq},
			       fn NONE => monoVar (exn, Type.exn)
				| _ => Error.bug "nullary excon applied to arg")
			   end
		      | SOME t =>
			   let
			      val tupleType =
				 Type.tuple (Vector.new2 (Type.unitRef, t))
			   in (tupleType,
			       [],
			       fn SOME x =>
			       conApp (tuple {exps = Vector.new2 (uniq,
								  varExp (x, t)),
					      ty = tupleType})
				| _ => Error.bug "unary excon not applied to arg")
			   end
	       in setExconInfo (con, SOME {refVar = r, make = make})
		  ; List.push (exnValCons, {con = con, arg = arg})
		  ; vall {var = r, exp = reff (unit ())} @ decs
	       end
	  | _ => Error.bug "implement exceptions saw unexpected dec"
      and loopMonoVal {var, ty, exp} : Dec.t list =
	 let
	    fun primExp e = [MonoVal {var = var, ty = ty, exp = e}]
	    fun keep () = primExp exp
	    fun makeExp e = Dexp.vall {var = var, exp = e}
	 in case exp of
	    Lambda l =>
	       if dropLambda var
		  then []
	       else primExp (Lambda (loopLambda l))
	  | PrimApp {prim, targs, args} =>
	       let
		  datatype z = datatype Prim.Name.t
		  fun assign (var, ty) =
		     primExp
		     (PrimApp {prim = Prim.assign,
			       targs = Vector.new1 ty,
			       args = Vector.new2 (VarExp.mono var,
						   Vector.sub (args, 0))})
	       in
		  case Prim.name prim of
		     Exn_extra => makeExp (extra (VarExp.var
						  (Vector.sub (args, 0))))
		   | Exn_name =>
			primExp (App {func = VarExp.mono exnName,
				      arg = Vector.sub (args, 0)})
		   | Exn_setInitExtra => []
		   | Exn_setRaise => setRaise assign
		   | Exn_setTopLevelHandler =>
			assign (topLevelHandler,
				Type.arrow (Type.exn, Type.unit))
		   | _ => primExp exp
	       end
	  | ConApp {con, arg, ...} =>
	       (case exconInfo con of
		   NONE => keep ()
		 | SOME {make, ...} => makeExp (make arg))
	  | Handle {try, catch = (catch, ty), handler} =>
	       primExp (Handle {try = loop try,
				catch = (catch, ty),
				handler = loop handler})
	  | Case {test, cases, default} =>
	       let
		  fun normal () =
		     primExp (Case
			      {test = test,
			       cases = Cases.map (cases, loop),
			       default = loopOpt default})
	       in case cases of
		  Cases.Con cases =>
		     if Vector.isEmpty cases
			then normal ()
		     else
			let
			   val (Pat.T {con, ...}, _) = Vector.sub (cases, 0)
			in
			   if not (isExcon con)
			      then normal ()
			   else (* convert to an exception match *)
			      let
				 open Dexp
				 val defaultVar = Var.newString "default"
				 fun callDefault () =
				    app {func = monoVar (defaultVar,
							 Type.arrow (Type.unit, ty)),
					 arg = unit (),
					 ty = ty}
				 val unit = Var.newString "unit"
				 val decs =
				    vall
				    {var = defaultVar,
				     exp =
				     lambda {arg = unit,
					     argType = Type.unit,
					     bodyType = ty,
					     body =
					     case default of
						SOME e => fromExp (loop e, ty)
					      | NONE =>
						   Error.bug "no default for exception case"}}
			      in makeExp
				 (lett
				  {decs = decs,
				   body =
				   extract
				   (VarExp.var test, ty, fn tuple =>
				    casee
				    {test = extractSum tuple,
				     ty = ty,
				     default = SOME (callDefault ()),
				     cases =
				     Cases.Con
				     (Vector.map
				      (cases, fn (Pat.T {con, arg, ...}, e) =>
				       let
					  val refVar = Var.newNoname ()
					  val body =
					     iff {test =
						  equal (monoVar (refVar, Type.unitRef),
							 monoVar
							 (#refVar (valOf (exconInfo con)),
							  Type.unitRef)),
						  ty = ty,
						  thenn = fromExp (loop e, ty),
						  elsee = callDefault ()}
					  fun make (arg, body) = 
					     (Pat.T {con = con,
						     targs = Vector.new0 (),
						     arg = SOME arg},
					      body)
				       in case arg of
					  NONE => make ((refVar, Type.unitRef), body)
					| SOME (x, t) =>
					     let
						val tuple =
						   (Var.newNoname (),
						    Type.tuple (Vector.new2
								(Type.unitRef, t)))
					     in make (tuple,
						      detupleBind
						      {tuple = monoVar tuple,
						       components =
						       Vector.new2 (refVar, x),
						       body = body})
					     end
				       end))})})
			      end
			end
		| _ => normal ()
	       end
          | Raise {exn, filePos} =>
	       raisee {var = var, ty = ty, exn = exn, filePos = filePos}
	  | _ => keep ()
	 end
      and loopLambda l =
	 let val {arg, argType, body} = Lambda.dest l
	 in Lambda.new {arg = arg,
			argType = argType,
			body = loop body}
	 end
      val body =
	 let
	    val x = (Var.newNoname (), Type.exn)
	 in
	    Dexp.handlee
	    {try = Dexp.fromExp (loop body, Type.unit),
	     ty = Type.unit,
	     catch = x,
	     handler = Dexp.app {func = (Dexp.deref
					 (Dexp.monoVar
					  (topLevelHandler,
					   let open Type
					   in reff (arrow (Type.exn, unit))
					   end))),
				 arg = Dexp.monoVar x,
				 ty = Type.unit}}
	 end
      fun bug s =
	 Dexp.primApp {prim = Prim.bug,
		       targs = Vector.new0 (),
		       args = Vector.new1 (Dexp.string s),
		       ty = Type.unit}
      val body =
	 Dexp.let1
	 {var = topLevelHandler,
	  exp = Dexp.reff (Dexp.lambda
			   {arg = Var.newNoname (),
			    argType = Type.exn,
			    bodyType = Type.unit,
			    body = bug "toplevel handler not installed"}),
	  body = body}
      val body = wrapBody body
      val (datatypes, body) =
	 case !exnValCons of
	    [] => (datatypes, body)
	  | cons =>
	       let
		  val cons = Vector.fromList cons
		  val exnNameDec =
		     MonoVal
		     {var = exnName,
		      ty = Type.arrow (Type.exn, Type.string),
		      exp =
		      Lambda
		      (Lambda.new
		       let val exn = Var.newNoname ()
		       in {arg = exn,
			   argType = Type.exn,
			   body =
			   let
			      open Dexp
			   in toExp
			      (extract
			       (exn, Type.string, fn tuple =>
				casee
				{test = extractSum tuple,
				 cases =
				 Cases.Con
				 (Vector.map
				  (cons, fn {con, arg} =>
				   (Pat.T {con = con,
					   targs = Vector.new0 (),
					   arg = SOME (Var.newNoname (), arg)},
				    const
				    (Const.fromString
				     (Con.originalName con))))),
				 default = NONE,
				 ty = Type.string}))
			   end}
		       end)}
	       in (Vector.concat
		   [Vector.new1
		    {tycon = sumTycon,
		     tyvars = Vector.new0 (),
		     cons = Vector.map (cons, fn {con, arg} =>
					{con = con, arg = SOME arg})},
		    extraDatatypes,
		    datatypes],
		   Exp.prefix (body, exnNameDec))
	       end
      val body =
	 Exp.fromPrimExp
	 (Handle {try = body,
		  catch = (Var.newNoname (), Type.exn),
		  handler =
		  let
		     val s = Var.newNoname ()
		  in Exp.prefix
		     (Exp.fromPrimExp
		      (PrimApp {prim = Prim.bug,
				targs = Vector.new0 (),
				args = Vector.new1 (VarExp.mono s)},
		       Type.unit),
		      MonoVal {var = s,
			       ty = Type.string,
			       exp = Const (Const.fromString
					    "toplevel handler not installed")})
		  end},
	  Type.unit)
      val program =
	 Program.T {datatypes = datatypes,
		    body = body,
		    overflow = !overflow}
      val _ = destroy ()
   in
      program
   end

end
