(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor LambdaFree (S: LAMBDA_FREE_STRUCTS): LAMBDA_FREE = 
struct

open S
open Dec PrimExp

structure Status =
   struct
      datatype t = Unseen | Free | Bound

      val toString =
	 fn Unseen => "Unseen"
	  | Free => "Free"
	  | Bound => "Bound"

      val layout = Layout.str o toString

      val init = Unseen
   end
datatype status = datatype Status.t

fun lambdaFree (Program.T {body, ...},
		overflowVar: Var.t,
		varInfo: Var.t -> {frees: Var.t list ref ref,
				   status: Status.t ref},
		lambdaInfo: Lambda.t -> {frees: Var.t vector ref,
					 recs: Var.t vector ref}) =
   let
      fun setFree (l: Lambda.t, xs: Var.t vector): unit =
	 #frees (lambdaInfo l) := xs
      fun setRec (l: Lambda.t, xs: Var.t vector): unit =
	 #recs (lambdaInfo l) := xs
      type scope = {frees: Var.t list ref,
		    get: Var.t -> Status.t,
		    set: Var.t * Status.t -> unit}
      fun bind (x: Var.t, {set, ...}: scope) = set (x, Bound)
      fun var (x: Var.t, {get, set, frees}: scope) =
	 case get x of
	    Unseen => (set (x, Free); List.push (frees, x))
	  | _ => ()
      fun vars (xs, s) = Vector.foreach (xs, fn x => var (x, s))
      fun varExp (x: VarExp.t, s: scope) = var (VarExp.var x, s)
      fun varExpOpt (x, s) =
	 case x of
	    NONE => ()
	  | SOME x => varExp (x, s)
      fun varExps (xs, s) = Vector.foreach (xs, fn x => varExp (x, s))
      fun newScope th = 
	 let
	    val frees = ref []
	    val all = ref []
	    fun statusRef x =
	       let val {frees = frees', status, ...} = varInfo x
	       in if frees = !frees'
		     then ()
		  else (List.push (all, (frees', !frees', status, !status))
			; frees' := frees; status := Unseen)
		     ; status
	       end
	    fun get x = !(statusRef x)
	    fun set (x, s) = statusRef x := s
	    val _ = th {frees = frees, get = get, set = set}
	    val _ = List.foreach (!all, fn (r, v, r', v') => (r := v; r' := v'))
	 in
	    Vector.fromList (!frees)
	 end
      fun exp (e, s) =
	 let val {decs, result} = Exp.dest e
	 in List.foreach
	    (decs,
	     fn Exception _ => ()
	      | MonoVal {var, exp, ...} => (primExp (exp, s); bind (var, s))
	      | PolyVal {var, exp = e, ...} => (exp (e, s); bind (var, s))
	      | Fun {decs, ...} =>
		   let
		      val {get = isBound, set, destroy} =
			 Property.destGetSetOnce (Var.plist,
						  Property.initConst false)
		      val _ =
			 Vector.foreach (decs, fn {var, ...} => set (var, true))
		      val xs =
			 newScope
			 (fn s =>
			  Vector.foreach
			  (decs, fn {lambda = l, ...} =>
			   setRec (l,
				   Vector.keepAll
				   (lambda l, fn x =>
				    if isBound x
				       then true
				    else (var (x, s); false)))))
		      val _ = destroy ()
		      val _ =
			 Vector.foreach (decs, fn {var, lambda, ...} =>
					 (setFree (lambda, xs)
					  ; bind (var, s)))
		   in
		      vars (xs, s)
		   end)
	    ; varExp (result, s)
	 end
      and primExp (e, s) = 
	 case e of
	    Const _ => ()
	  | Var x => varExp (x, s)
	  | Tuple xs => varExps (xs, s)
	  | Select {tuple, ...} => varExp (tuple, s)
	  | Lambda l =>
	       let val xs = lambda l
	       in setFree (l, xs); vars (xs, s)
	       end
	  | ConApp {arg, ...} => varExpOpt (arg, s)
	  | PrimApp {prim, args, ...} => 
	       (if Prim.mayOverflow prim
		  then var (overflowVar, s)
		  else ();
		varExps (args, s))
	  | App {func, arg} => (varExp (func, s); varExp (arg, s))
	  | Raise x => varExp (x, s)
	  | Handle {try, catch, handler} =>
	       (exp (try, s); bind (#1 catch, s); exp (handler, s))
	  | Case {test, cases, default} =>
	       (varExp (test, s)
		; expOpt (default, s)
		; Cases.foreach' (cases, fn e => exp (e, s),
				  fn Pat.T {arg, ...} =>
				  Option.app (arg, fn (x, _) => bind (x, s))))
      and expOpt (e, s) =
	 case e of
	    NONE => ()
	  | SOME e => exp (e, s)
      and lambda (l: Lambda.t) : Var.t vector =
	 let val {arg, body, ...} = Lambda.dest l
	 in newScope (fn s => (bind (arg, s); exp (body, s)))
	 end
      val frees = newScope (fn s => exp (body, s))
      val _ =
	 if Vector.isEmpty frees
	    then ()
	 else Error.bug "program has free variables"
   in
      ()
   end

end
