(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor ImplementExceptions (S: IMPLEMENT_EXCEPTIONS_STRUCTS):
   IMPLEMENT_EXCEPTIONS = 
struct

open S
open Dec PrimExp
structure Dexp = DirectExp

fun doit (Program.T {datatypes, body}): Program.t =
   let
      (* topLevelHandler holds the ref cell containing the function of
       * type exn -> unit that should be called on unhandled exceptions.
       *)
      val topLevelHandler = Var.newNoname ()
      val exnName = Var.newString "exnName"
      val exnNameType = Type.arrow (Type.exn, Type.string)
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
	       let open Dexp
		  val r = Var.newString "exnRef"
		  val uniq = monoVar (r, Type.unitRef)
		  val conApp =
		     fn arg =>
		     conApp {con = con,
			     targs = Vector.new0 (),
			     ty = Type.exn,
			     arg = SOME arg}
		  val (arg, decs, make) =
		     case arg of
			NONE =>
			   (* If the exception is not value carrying, then go
			    * ahead and make it now.
			    *)
			   let val exn = Var.newNoname ()
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
	 let fun primExp e = [MonoVal {var = var, ty = ty, exp = e}]
	    fun keep () = primExp exp
	    fun makeExp e = Dexp.vall {var = var, exp = e}
	 in case exp of
	    Lambda l => primExp (Lambda (loopLambda l))
	  | PrimApp {prim, targs, args} =>
	       primExp
	       (case Prim.name prim of
		   Prim.Name.Exn_name =>
		      App {func = VarExp.mono exnName,
			   arg = Vector.sub (args, 0)}
		 | Prim.Name.Exn_setTopLevelHandler =>
		      PrimApp {prim = Prim.assign,
			       targs = Vector.new1 (let open Type
						    in arrow (exn, unit)
						    end),
			       args = Vector.new2 (VarExp.mono topLevelHandler,
						   Vector.sub (args, 0))}
		 | _ => PrimApp {prim = prim, targs = targs, args = args})
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
			      in makeExp
				 (lett
				  {decs = vall {var = defaultVar,
						exp =
						lambda
						{arg = unit,
						 argType = Type.unit,
						 bodyType = ty,
						 body =
						 case default of
						    SOME e => fromExp (loop e, ty)
						  | NONE =>
						       Error.bug "no default for exception case"}},
				   
				   body = 
				   casee
				   {test = varExp (test, Type.exn),
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
				      end))}})
			      end
			end
		| _ => normal ()
	       end
	  | _ => keep ()
	 end

      and loopLambda l =
	 let val {arg, argType, body} = Lambda.dest l
	 in Lambda.new {arg = arg,
			argType = argType,
			body = loop body}
	 end

      val body =
	 Dexp.toExp
	 (Dexp.let1
	  {var = topLevelHandler,
	   exp = (Dexp.reff
		  (Dexp.lambda
		   {arg = Var.newNoname (),
		    argType = Type.exn,
		    bodyType = Type.unit,
		    body =
		    Dexp.primApp
		    {prim = Prim.bug,
		     targs = Vector.new0 (),
		     args = (Vector.new1
			     (Dexp.string "toplevel handler not installed")),
		     ty = Type.unit}})),
	   body = let val x = (Var.newNoname (), Type.exn)
		  in Dexp.handlee
		     {try = Dexp.fromExp (loop body, Type.unit),
		      ty = Type.unit,
		      catch = x,
		      handler =
		      Dexp.app {func = Dexp.deref (Dexp.monoVar
						   (topLevelHandler,
						    let open Type
						    in reff (arrow (exn, unit))
						    end)),
				arg = Dexp.monoVar x,
				ty = Type.unit}}
		  end})
      val (datatypes, body) =
	 case !exnValCons of
	    [] => (datatypes, body)
	  | cons =>
	       let
		  val cons = Vector.fromList cons
		  val exnNameDec =
		     MonoVal
		     {var = exnName,
		      ty = exnNameType,
		      exp =
		      Lambda
		      (Lambda.new
		       let val exn = Var.newNoname ()
		       in {arg = exn,
			   argType = Type.exn,
			   body =
			   let open Dexp
			   in toExp
			      (casee
			       {test = monoVar (exn, Type.exn),
				cases =
				Cases.Con
				(Vector.map
				 (cons, fn {con, arg} =>
				  (Pat.T {con = con,
					  targs = Vector.new0 (),
					  arg = SOME (Var.newNoname (), arg)},
				   const
				   (Const.fromString (Con.originalName con))))),
				default = NONE,
				ty = Type.string})
			   end}
		       end)}
	       in (Vector.concat
		   [Vector.new1 {tycon = Tycon.exn,
				 tyvars = Vector.new0 (),
				 cons = (Vector.map
					 (cons, fn {con, arg} =>
					  {con = con, arg = SOME arg}))},
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
		    body = body}
      val _ = destroy ()
   in
      program
   end

end
