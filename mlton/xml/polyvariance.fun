(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
(*
 * Duplicate a let bound function at each variable reference
 * if cost is smaller than threshold.
 * 
 *)
functor Polyvariance (S: POLYVARIANCE_STRUCTS): POLYVARIANCE = 
struct

open S
open Dec PrimExp
   
structure Type =
   struct
      open Type

      fun containsArrow t = containsTycon (t, Tycon.arrow)

      fun isHigherOrder t =
	 case dearrowOpt t of
	    NONE => false
	  | SOME (t1, t2) => containsArrow t1 orelse isHigherOrder t2

   (*       val isHigherOrder =
    * 	 Trace.trace ("isHigherOrder", layout, Bool.layout) isHigherOrder
    *)
   end

fun lambdaSize (Program.T {body, ...}): Lambda.t -> int =
   let
      val {get = size: Lambda.t -> int, set} =
	 Property.getSetOnce (Lambda.plist,
			      Property.initRaise ("size", Lambda.layout))
      fun loopExp (e: Exp.t, n: int): int =
	 List.fold
	 (Exp.decs e, n, fn (d, n) =>
	  case d of
	     MonoVal {exp, ...} => loopPrimExp (exp, n + 1)
	   | PolyVal {exp, ...} => loopExp (exp, n + 1)
	   | Fun {decs, ...} => Vector.fold (decs, n, fn ({lambda, ...}, n) =>
					     loopLambda (lambda, n))
	   | Exception _ => n + 1)
      and loopLambda (l: Lambda.t, n): int =
	 let val m = loopExp (Lambda.body l, 0)
	 in set (l, m); m + n
	 end
      and loopPrimExp (e: PrimExp.t, n: int): int =
	 case e of
	    Case {cases, default, ...} =>
	       Cases.fold
	       (cases,
		(case default of
		    NONE => n
		  | SOME e => loopExp (e, n)),
		    fn (e, n) => loopExp (e, n))
	  | Handle {try, handler, ...} =>
	       loopExp (try, loopExp (handler, n))
	  | Lambda l => loopLambda (l, n)
	  | _ => n
   in loopExp (body, 0)
      ; size
   end

fun shouldDuplicate (program as Program.T {body, ...}, small, product)
   : Var.t -> bool =
   let
      val costs: (Var.t * int * int * int) list ref = ref []

      val lambdaSize = lambdaSize program
	 
      fun isOK (var: Var.t, size: int, numOccurrences: int): bool =
	 let val cost = (numOccurrences - 1) * (size - small)
	 in List.push (costs, (var, size, numOccurrences, cost))
	    ; cost <= product
	 end
      type info = {numOccurrences: int ref,
		   shouldDuplicate: bool ref}
      val {get = varInfo: Var.t -> info option, set = setVarInfo} =
	 Property.getSetOnce (Var.plist, Property.initConst NONE)
      fun new {var, ty, lambda}: unit =
	 if Type.isHigherOrder ty
	    then setVarInfo (var, SOME {numOccurrences = ref 0,
					shouldDuplicate = ref false})
	 else ()
      fun loopExp (e: Exp.t, numDuplicates: int): unit =
	 let
	    fun loopVar (x: VarExp.t): unit =
	       case varInfo (VarExp.var x) of
		  NONE => ()
		| SOME {numOccurrences, ...} =>
		     numOccurrences := !numOccurrences + numDuplicates
	    fun loopVars xs = Vector.foreach (xs, loopVar)
	    val {decs, result} = Exp.dest e
	    val rec loopDecs =
	       fn [] => loopVar result
		| dec :: decs =>
		     case dec of
			MonoVal {var, ty, exp} =>
			   (case exp of
			       Lambda l =>
				  (new {var = var, ty = ty, lambda = l}
				   ; loopDecs decs
				   ; let
					val body = Lambda.body l
					val numDuplicates =
					   case varInfo var of
					      NONE => numDuplicates
					       | SOME {numOccurrences,
						       shouldDuplicate} =>
						 if isOK (var, lambdaSize l,
							  !numOccurrences)
						    then (shouldDuplicate := true
							  ; !numOccurrences)
						 else numDuplicates
				     in loopExp (body, numDuplicates)
				     end)
			     | _ =>
				  let
				     val loopExp =
					fn e => loopExp (e, numDuplicates)
				  in (case exp of
					 Const _ => ()
				       | Var x => loopVar x
				       | Tuple xs => loopVars xs
				       | Select {tuple, ...} => loopVar tuple
				       | ConApp {arg, ...} =>
					    Option.app (arg, loopVar)
				       | PrimApp {args, ...} => loopVars args
				       | App {func, arg} =>
					    (loopVar func; loopVar arg)
				       | Raise x => loopVar x
				       | Case {test, cases, default} =>
					    (loopVar test
					     ; Cases.foreach (cases, loopExp)
					     ; Option.app (default, loopExp))
				       | Handle {try, handler, ...} =>
					    (loopExp try; loopExp handler)
				       | _ => Error.bug "unexpected primExp")
				     ; loopDecs decs
				  end)
		      | Fun {decs = lambdas, ...} =>
			   let
			      val _ = (Vector.foreach (lambdas, new)
				       ; loopDecs decs)
			      val dups =
				 Vector.fold
				 (lambdas, [], fn ({var, lambda, ...}, dups) =>
				  let val body = Lambda.body lambda
				  in case varInfo var of
				     NONE =>
					(loopExp (body, numDuplicates); dups)
				   | SOME info =>
					{body = body,
					 size = lambdaSize lambda,
					 info = info} :: dups
				  end)
			   in case dups of
			      [] => ()
			    | _ => 
				 let
				    val size =
				       List.fold
				       (dups, 0, fn ({body, size, ...}, n) =>
					n + size)
				    val numOccurrences =
				       List.fold
				       (dups, 0,
					fn ({info = {numOccurrences, ...}, ...},
					    n) => n + !numOccurrences)
				 in if isOK (if Vector.isEmpty lambdas
						then Error.bug "empty lambdas"
					     else
						#var (Vector.sub (lambdas, 0)),
					     size, numOccurrences)
				       then (List.foreach
					     (dups,
					      fn {body,
						  info = {shouldDuplicate, ...},
						  ...} =>
					      (shouldDuplicate := true
					       ; loopExp (body, numOccurrences))))
				    else
				       List.foreach
				       (dups, fn {body, ...} =>
					loopExp (body, numDuplicates))
				 end
			   end
		      | _ => Error.bug "strange dec"
	 in loopDecs decs
	 end
      val _ = loopExp (body, 1)
      fun sort l =
	 List.insertionSort (l, fn ((_, _, _, c), (_, _, _, c')) => c < c')
      val _ =
	 Control.diagnostic
	 (fn layout => 
	  List.foreach
	  (sort (!costs), fn (x, size, numOcc, c) =>
	   layout (let open Layout
		   in seq [Var.layout x,
			   str " ", Int.layout size,
			   str " ", Int.layout numOcc,
			   str " ", Int.layout c]
		   end)))
   in
      fn x =>
      case varInfo x of
	 NONE => false
       | SOME {shouldDuplicate, ...} => !shouldDuplicate
   end

fun duplicate (program as Program.T {datatypes, body},
	       small: int,
	       product: int) =
   let
      val shouldDuplicate = shouldDuplicate (program, small, product)
      datatype info =
	 Replace of Var.t
       | Dup of {
		 duplicates: Var.t list ref
		 }
      val {get = varInfo: Var.t -> info, set = setVarInfo} =
	 Property.getSet (Var.plist,
			  Property.initRaise ("Polyvariance.info", Var.layout))
      fun loopVar (x: VarExp.t): VarExp.t =
	 VarExp.mono
	 (let val x = VarExp.var x
	  in case varInfo x of
	     Replace y => y
	   | Dup {duplicates, ...} =>
		let val x' = Var.new x
		in List.push (duplicates, x')
		   ; x'
		end
	  end)
      fun loopVars xs = Vector.map (xs, loopVar)
      fun bind (x: Var.t): Var.t =
	 let val x' = Var.new x
	 in setVarInfo (x, Replace x')
	    ; x'
	 end
      fun bindVarType (x, t) = (bind x, t)
      fun bindPat (Pat.T {con, targs, arg}) =
	 Pat.T {con = con,
		targs = targs,
		arg = Option.map (arg, bindVarType)}
      fun new {var, ty, lambda}: unit =
	 if shouldDuplicate var
	    then setVarInfo (var, Dup {duplicates = ref []})
	 else (bind var; ())
      fun loopExp (e: Exp.t): Exp.t =
	 let val {decs, result} = Exp.dest e
	 in Exp.new (loopDecs (decs, result))
	 end
      and loopLambda (l: Lambda.t): Lambda.t =
	 let val {arg, argType, body} = Lambda.dest l
	 in Lambda.new {arg = bind arg,
			argType = argType,
			body = loopExp body}
	 end
      and loopDecs (ds: Dec.t list, result): {decs: Dec.t list,
					      result: VarExp.t} =
	 case ds of
	    [] => {decs = [], result = loopVar result}
	  | d :: ds =>
	       case d of
		  MonoVal {var, ty, exp} =>
		     (case exp of
			 Lambda l =>
			    let
			       val _ = new {var = var, ty = ty, lambda = l}
			       val {decs, result} = loopDecs (ds, result)
			       val decs =
				  case varInfo var of
				     Replace var =>
					MonoVal {var = var, ty = ty,
						 exp = Lambda (loopLambda l)}
					:: decs
				   | Dup {duplicates, ...} =>
					List.fold
					(!duplicates, decs, fn (var, decs) =>
					 MonoVal {var = var, ty = ty,
						  exp = Lambda (loopLambda l)}
					 :: decs)
			    in {decs = decs, result = result}
			    end
		       | _ => 
			    let
			       val exp =
				  case exp of
				     Const _ => exp
				   | Var x => Var (loopVar x)
				   | Tuple xs => Tuple (loopVars xs)
				   | Select {tuple, offset} =>
					Select {tuple = loopVar tuple,
						offset = offset}
				   | ConApp {con, targs, arg} =>
					ConApp {con = con,
						targs = targs,
						arg = Option.map (arg, loopVar)}
				   | PrimApp {prim, targs, args} =>
					PrimApp {prim = prim,
						 targs = targs,
						 args = loopVars args}
				   | App {func, arg} =>
					App {func = loopVar func,
					     arg = loopVar arg}
				   | Raise x => Raise (loopVar x)
				   | Case {test, cases, default} =>
					let
					   datatype z = datatype Cases.t
					   fun doit cases =
					      Vector.map (cases, fn (z, e) =>
							  (z, loopExp e))
					   val cases =
					      case cases of
						 Char cases => Char (doit cases)
					       | Con cases =>
						    Con
						    (Vector.map
						     (cases, fn (p, e) =>
						      (bindPat p, loopExp e)))
					       | Int cases => Int (doit cases)
					       | Word cases => Word (doit cases)
					       | Word8 cases =>
						    Word8 (doit cases)
					in Case {test = loopVar test,
						 cases = cases,
						 default = Option.map (default,
								       loopExp)}
					end
				   | Handle {try, catch, handler} =>
					Handle {try = loopExp try,
						catch = bindVarType catch,
						handler = loopExp handler}
				   | _ => Error.bug "unexpected primExp"
			       val var = bind var
			       val {decs, result} = loopDecs (ds, result)
			    in {decs = (MonoVal {var = var, ty = ty, exp = exp}
					:: decs),
				result = result}
			    end)
		| Fun {decs, ...} =>
		     let
			val _ = Vector.foreach (decs, new)
			val {decs = ds, result} = loopDecs (ds, result)
			val ac =
			   ref [Vector.keepAllMap
				(decs, fn {var, ty, lambda} =>
				 case varInfo var of
				    Replace var =>
				       SOME {var = var, ty = ty,
					     lambda = loopLambda lambda}
				  | Dup _ => NONE)]
			val dups =
			   Vector.keepAllMap
			   (decs, fn dec as {var, ...} =>
			    case varInfo var of
			       Replace _ => NONE
			     | Dup {duplicates, ...} => SOME (dec, !duplicates))
			val decs =
			   Vector.foreach
			   (dups, fn ({var, ty, lambda}, duplicates) =>
			    List.foreach
			    (duplicates, fn var' =>
			     let
				val vars =
				   Vector.map
				   (dups, fn ({var = var'', ...}, _) =>
				    if Var.equals (var, var'')
				       then (setVarInfo (var, Replace var')
					     ; var')
				    else bind var'')
			     in List.push
				(ac,
				 Vector.map2
				 (dups, vars,
				  fn (({ty, lambda, ...}, _), var) =>
				  {var = var, ty = ty,
				   lambda = loopLambda lambda}))
			     end))
			val decs = Vector.concat (!ac)
		     in {decs = Fun {tyvars = Vector.new0 (),
				     decs = decs} :: ds,
			 result = result}
		     end
		| _ => Error.bug "polyvariance saw bogus dec"
      val program =
	 Program.T {datatypes = datatypes,
		    body = loopExp body}
      val _ = Program.clear program
   in
      program
   end

val duplicate =
   fn p =>
   case !Control.polyvariance of
      NONE => p
    | SOME {rounds, small, product} =>
	 let
	    fun loop (p, n) =
	       if n = 0
		  then p
	       else let
		       val p = simplify (duplicate (p, small, product))
		       val _ =
			  Control.message (Control.Detail, fn () =>
					   Program.layoutStats p)
		    in
		       loop (p, n - 1)
		    end
	 in loop (p, rounds)
	 end
      
end
