(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Globalize (S: GLOBALIZE_STRUCTS): GLOBALIZE = 
struct

open S
open Dec PrimExp

fun globalize {program = Program.T {datatypes, body, ...},
	       lambdaFree,
	       varGlobal: Var.t -> bool ref} =
   let
      val noConts = 
	 not (Exp.hasPrim (body, fn p =>
			   case Prim.name p of
			      Prim.Name.Thread_switchToCont => true
			    | _ => false))
      local
	 val {get: Tycon.t -> bool, set, destroy} =
	    Property.destGetSetOnce (Tycon.plist, Property.initConst false)
	 fun makeBig tycon = set (tycon, true)
	 val _ = (Vector.foreach (datatypes, makeBig o #tycon)
		  ; makeBig Tycon.array
		  ; makeBig Tycon.vector)
      in
	 val tyconIsBig = get
	 val destroyTycon = destroy
      end
      fun typeIsSmall t =
	 let open Type
	 in case dest t of
	    Con (c, ts) =>
	       not (tyconIsBig c)
	       andalso if (Tycon.equals (c, Tycon.tuple)
			   orelse Tycon.equals (c, Tycon.reff))
			  then Vector.forall (ts, typeIsSmall)
		       else true
	  | _ => Error.bug "typeIsSmall saw type variable"
	 end
      val varIsGlobal = ! o varGlobal
      val isGlobal = varIsGlobal o VarExp.var
      fun areGlobal xs = Vector.forall (xs, isGlobal)
      fun makeGlobal x = varGlobal x := true
      val traceLoopExp =
	 Trace.trace2 ("Globalize.loopExp", Exp.layout, Bool.layout, Bool.layout)
      fun loopExp (arg: Exp.t * bool) =
	 traceLoopExp
	 (fn (e: Exp.t, once: bool) =>
	  List.fold
	  (Exp.decs e, once, fn (d, once) =>
	   case d of
	      MonoVal {var, ty, exp} =>
		 let
		    val (global, once) =
		       case exp of
			  Const _ => (true, once)
			| Var x => (isGlobal x, once)
			| Tuple xs => (areGlobal xs, once)
			| Select {tuple, ...} => (isGlobal tuple, once)
			| Lambda l =>
			     (loopLambda l
			      ; (Vector.forall (lambdaFree l, varIsGlobal),
				 once))
			| ConApp {arg, ...} =>
			     (case arg of
				 NONE => true
			       | SOME x => isGlobal x,
				    once)
			| PrimApp {prim, args, ...} =>
			     let
				val global =
				   areGlobal args andalso
				   ((Prim.isFunctional prim
				     (* Don't want to move MLton_equal into the globals
				      * because polymorphic equality isn't implemented
				      * there. 
				      *)
				     andalso Prim.name prim <> Prim.Name.MLton_equal)
				    orelse
				    (once andalso
				     (case Prim.name prim of
					 Prim.Name.Ref_ref => typeIsSmall ty
				       | _ => false)))
				val once =
				   once andalso
				   (noConts orelse
				    Prim.name prim <> Prim.Name.Thread_current)
			     in (global, once)
			     end
			| Case {cases, default, ...} =>
			     let
				val once' =
				   Cases.fold
				   (cases, false, fn (e, b) =>
				    loopExp (e, once) andalso b)
				val once' =
				   Option.fold (default, once',
						fn (e, b) =>
						loopExp (e, once) andalso b)
			     in (false, once andalso once')
			     end
			| Handle {try, handler, ...} =>
			     (false,
			      loopExp (handler, loopExp (try, once)))
			| _ => (false, once)
		    val _ = if global then makeGlobal var else ()
		 in once
		 end
	    | Fun {decs, ...} =>
		 (if Vector.isEmpty decs
		     then ()
		  else
		     let
			val {lambda, ...} = Vector.sub (decs, 0)
		     in
			if Vector.forall (lambdaFree lambda, varIsGlobal)
			   then Vector.foreach (decs, makeGlobal o #var)
			else ()
		     end
		     ; Vector.foreach (decs, loopLambda o #lambda)
		     ; once)
	    | _ => Error.bug "globalize saw strange dec")
	  ) arg
      and loopLambda (l: Lambda.t): unit =
	 (loopExp (Lambda.body l, false); ())
      val _ = loopExp (body, true)
      val _ = destroyTycon ()
   in
      ()
   end

end
