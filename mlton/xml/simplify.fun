(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
(* This simplifier is based on the following article.
 *   Shrinking Lambda Expressions in Linear Time.
 *   Journal of Functional Programming. Vol 7, no 5, 1997.
 *)

functor Simplify (S: SIMPLIFY_STRUCTS): SIMPLIFY = 
struct

open S
open XmlTree
open Dec PrimExp

fun msg s = Out.output (Out.error, s)

val traceSimplifyExp =
   Trace.trace ("Xml.simplifyExp", Exp.layout, Exp.layout)

val traceSimplifyDecs =
   Trace.trace
   ("Xml.simplifyDecs", List.layout Dec.layout, List.layout Dec.layout)

val traceSimplifyLambda =
   Trace.trace ("Xml.simplifyLambda", Lambda.layout, Lambda.layout)
   
fun inc (r: int ref, n) =
   let val n = !r + n
   in Assert.assert ("inc", fn () => n >= 0)
      ; r := n
   end

structure VarInfo =
   struct
      datatype t =
	 Mono of monoVarInfo
       | Poly of VarExp.t
      and value =
	 ConApp of {con: Con.t,
		    targs: Type.t vector,
		    arg: t option}
	| Const of Const.t
	| Lambda of {isInlined: bool ref,
		     lam: Lambda.t}
	| Tuple of t vector
      withtype monoVarInfo = {numOccurrences: int ref,
			      value: value option ref,
			      varExp: VarExp.t}

      local
	 open Layout
      in
	 val rec layout =
	    fn Mono {numOccurrences, value, varExp} =>
	         record [("numOccurrences", Int.layout (!numOccurrences)),
			 ("value", Option.layout layoutValue (!value)),
			 ("varExp", VarExp.layout varExp)]
	     | Poly x => seq [str "Poly ", VarExp.layout x]
	 and layoutValue =
	    fn ConApp {con, arg, ...} =>
	         seq [Con.layout con,
		      case arg of
			 NONE => empty
		       | SOME i => paren (layout i)]
	     | Const c => Const.layout c
	     | Lambda {isInlined, lam} =>
		  seq [str "Lambda ", Bool.layout (!isInlined)]
	     | Tuple is => Vector.layout layout is
      end

      val inc =
	 fn (i, n) =>
	 case i of
	    Mono {numOccurrences = r, ...} => inc (r, n)
	  | _ => ()

      val inc =
	 Trace.trace2 ("VarInfo.inc", layout, Int.layout, Unit.layout) inc

      fun delete i = inc (i, ~1)

      val delete = Trace.trace ("VarInfo.delete", layout, Unit.layout) delete
	 
      fun deletes is = Vector.foreach (is, delete)

      val varExp =
	 fn Mono {varExp, ...} => varExp
	  | Poly x => x
   end

structure MonoVarInfo =
   struct
      type t = VarInfo.monoVarInfo
   end

structure Value =
   struct
      datatype t = datatype VarInfo.value

      val layout = VarInfo.layoutValue

      fun toPrimExp v =
	 case v of
	    ConApp {con, targs, arg} =>
	       PrimExp.ConApp {con = con,
			       targs = targs,
			       arg = Option.map (arg, VarInfo.varExp)}
	  | Const c => PrimExp.Const c
	  | Lambda {lam, ...} => PrimExp.Lambda lam
	  | Tuple vs => PrimExp.Tuple (Vector.map (vs, VarInfo.varExp))
   end

fun simplifyOnce (Program.T {datatypes, body, overflow}) =
   let
      (* Keep track of the number of constuctors in each datatype so that
       * we can eliminate redundant defaults.
       *)
      val {get = conNumCons: Con.t -> int , set = setConNumCons, ...} =
	 Property.getSetOnce (Con.plist, Property.initConst ~1)
      val _ =
	 Vector.foreach
	 (datatypes, fn {cons, ...} =>
	  let
	     val n = Vector.length cons
	  in
	     Vector.foreach (cons, fn {con, ...} => setConNumCons (con, n))
	  end)
      fun isExhaustive (cases: exp Cases.t): bool =
	 case cases of
	    Cases.Con v =>
	       ((0 < Vector.length v
		 andalso (Vector.length v
			  = conNumCons (Pat.con (#1 (Vector.sub (v, 0)))))))
	  | _ => false
      val {get = varInfo: Var.t -> VarInfo.t, set = setVarInfo, ...} =
	 Property.getSet (Var.plist,
			  Property.initRaise ("simplify varInfo", Var.layout))
      val varInfo =
	 Trace.trace ("Xml.Simplify.varInfo", Var.layout, VarInfo.layout) varInfo
      fun monoVarInfo x =
	 case varInfo x of
	    VarInfo.Mono i => i
	  | _ => Error.bug "monoVarInfo"
      fun varInfos xs = List.map (xs, varInfo)
      fun varExpInfo (x as VarExp.T {var, targs, ...}): VarInfo.t =
	 if Vector.isEmpty targs
	    then varInfo var
	 else VarInfo.Poly x
      val varExpInfo =
	 Trace.trace ("varExpInfo", VarExp.layout, VarInfo.layout) varExpInfo
      fun varExpInfos xs = Vector.map (xs, varExpInfo)
      fun replaceInfo (x: Var.t,
		       {numOccurrences = r, ...}: MonoVarInfo.t,
		       i: VarInfo.t): unit =
	 (VarInfo.inc (i, !r)
	  ; setVarInfo (x, i))
      val replaceInfo =
	 Trace.trace ("replaceInfo",
		      fn (x, _, i) => Layout.tuple [Var.layout x,
						    VarInfo.layout i],
		      Unit.layout)
	 replaceInfo
      fun replace (x, i) = replaceInfo (x, monoVarInfo x, i)
      val simplifyVarExp = VarInfo.varExp o varExpInfo
      fun simplifyVarExps xs = Vector.map (xs, simplifyVarExp)
      val dummyVarExp = VarExp.mono (Var.newString "dummy")
      local
	 fun handleBoundVar (x, ts, ty) =
	    setVarInfo (x,
			if Vector.isEmpty ts
			   then VarInfo.Mono {numOccurrences = ref 0,
					      value = ref NONE,
					      varExp = VarExp.mono x}
			else VarInfo.Poly dummyVarExp)
	 fun handleVarExp x = VarInfo.inc (varInfo (VarExp.var x), 1)
      in
	 fun countExp (e: Exp.t): unit =
	    Exp.foreach {exp = e,
			 handleBoundVar = handleBoundVar,
			 handleExp = fn _ => (),
			 handlePrimExp = fn _ => (),
			 handleVarExp = handleVarExp}
      end
      fun deleteVarExp (x: VarExp.t): unit =
	 VarInfo.inc (varInfo (VarExp.var x), ~1)
      fun deleteExp (e: Exp.t): unit = Exp.foreachVarExp (e, deleteVarExp)
      val deleteExp =
	 Trace.trace ("deleteExp", Exp.layout, Unit.layout) deleteExp
      fun deleteLambda l = deleteExp (Lambda.body l)
      (*---------------------------------------------------*)
      (*                    simplifyExp                    *)
      (*---------------------------------------------------*)
      fun simplifyExp arg: Exp.t =
	 traceSimplifyExp
	 (fn (e: Exp.t) =>
	  let val {decs, result} = Exp.dest e
	  in Exp.new {decs = simplifyDecs decs,
		      result = simplifyVarExp result}
	  end) arg
      and simplifyDecs (decs: Dec.t list): Dec.t list =
	 case decs of
	    [] => []
	  | dec :: decs =>
	       case dec of
		  Exception _ => dec :: simplifyDecs decs
		| PolyVal {var, tyvars, ty, exp} =>
		     Dec.PolyVal {var = var, tyvars = tyvars, ty = ty,
				  exp = simplifyExp exp}
		     :: simplifyDecs decs
		| Fun {tyvars, decs = decs'} =>
		     if Vector.isEmpty tyvars
			then
			   let
			      val decs' =
				 Vector.keepAll
				 (decs', fn {var, ty, lambda} =>
				  let
				     val {numOccurrences, value, ...} =
					monoVarInfo var
				  in if 0 = !numOccurrences
					then (deleteLambda lambda; false)
				     else (value := (SOME
						     (Value.Lambda
						      {isInlined = ref false,
						       lam = lambda}))
					   ; true)
				  end)
			      val decs = simplifyDecs decs
			      (* Need to walk over all the decs and remove
			       * their value before simplifying any of them
			       * because they are mutually recursive.
			       *)
			      val decs' =
				 Vector.keepAll
				 (decs', fn {var, lambda, ...} =>
				  let
				     val {numOccurrences, value, ...} =
					monoVarInfo var
				  in case !value of
				     SOME (Value.Lambda {isInlined, ...}) =>
					not (!isInlined)
					andalso
					if 0 = !numOccurrences
					   then (deleteLambda lambda
						 ; false)
					else (value := NONE; true)
				   | _ => Error.bug "should be a lambda"
				  end)
			   in
			      if Vector.isEmpty decs'
				 then decs
			      else
				 Dec.Fun {tyvars = tyvars,
					  decs =
					  Vector.map
					  (decs', fn {var, ty, lambda} =>
					   {var = var,
					    ty = ty,
					    lambda = simplifyLambda lambda})}
				 :: decs
			   end
		     else
			Dec.Fun {tyvars = tyvars,
				 decs =
				 Vector.map
				 (decs', fn {var, ty, lambda} =>
				  {var = var,
				   ty = ty,
				   lambda = simplifyLambda lambda})}
			:: simplifyDecs decs
		| MonoVal b =>
		     simplifyMonoVal (b, fn () => simplifyDecs decs)
      and simplifyMonoVal ({var, ty, exp},
			   rest: unit -> Dec.t list) =
	 let
	    val info as {numOccurrences, value, ...} = monoVarInfo var
	    fun finish (exp, decs) =
	       MonoVal {var = var, ty = ty, exp = exp} :: decs
	    fun nonExpansive (delete: unit -> unit,
			      set: unit -> (unit -> PrimExp.t) option) =
	       if 0 = !numOccurrences
		  then (delete (); rest ())
	       else let
		       val s = set ()
		       val decs = rest ()
		    in if 0 = !numOccurrences
			  then (delete (); decs)
		       else (case s of
				NONE => decs
			      | SOME n => finish (n (), decs))
		    end
	    fun expansive (e: PrimExp.t) = finish (e, rest ())
	    fun nonExpansiveCon (delete, v: Value.t) =
	       nonExpansive
	       (delete,
		fn () => (value := SOME v
			  ; SOME (fn () => Value.toPrimExp v)))
	    fun expression (e: Exp.t): Dec.t list =
	       let
		  val {decs = decs', result} = Exp.dest (simplifyExp e)
		  val _ = replaceInfo (var, info, varExpInfo result)
		  val decs = rest ()
	       in decs' @ decs
	       end
	 in
	    case exp of
	       Case {test, cases, default} =>
		  let
		     fun match (cases, f): Dec.t list =
			let
			   val _ = deleteVarExp test
			   fun step (i, (c, e), ()) =
			      if f c
				 then
				    (Vector.foreachR (cases, i + 1,
						      Vector.length cases,
						      deleteExp o #2)
				     ; Option.app (default, deleteExp o #1)
				     ; Vector.Done (expression e))
			      else (deleteExp e; Vector.Continue ())
			   fun done () =
			      case default of
				 SOME (e, _) => expression e
			       | NONE => Error.bug "simplifyPrimExp: Case"
			in Vector.fold' (cases, 0, (), step, done)
			end
		     fun normal test =
			let
			   (* Eliminate redundant default case. *)
			   val default =
			      if isExhaustive cases
				 then (Option.app (default, deleteExp o #1)
				       ; NONE)
			      else Option.map (default, fn (e, r) =>
					       (simplifyExp e, r))
			in
			   expansive
			   (Case {test = test,
				  cases = Cases.map (cases, simplifyExp),
				  default = default})
			end
		  in
		     case varExpInfo test of
			VarInfo.Poly test => normal test
		      | VarInfo.Mono {value, varExp, ...} => 
			   case (cases, !value) of
			      (Cases.Con cases,
			       SOME (Value.ConApp {con = c, arg, ...})) =>
			      let
				 val match =
				    fn f =>
				    match (cases,
					   fn Pat.T {con = c', arg, ...} =>
					   Con.equals (c, c')
					   andalso f arg)
			      in case arg of
				 NONE => match Option.isNone
			       | SOME v =>
				    match
				    (fn SOME (x, _) => (replace (x, v); true)
				  | _ => false)
			      end
			     | (_, SOME (Value.Const c)) =>
				  let
				     fun doit (l, z) = match (l, fn z' => z = z')
				  in case (cases, Const.node c) of
				     (Cases.Char l, Const.Node.Char c) =>
					doit (l, c)
				   | (Cases.Int l, Const.Node.Int i) =>
					doit (l, i)
				   | (Cases.Word l, Const.Node.Word w) =>
					doit (l, w)
				   | (Cases.Word8 l, Const.Node.Word w) =>
					doit (l, Word8.fromWord w)
				   | _ => Error.bug "strange case"
				  end
			     | (_, NONE) => normal varExp
			     | _ => Error.bug "simplifyMonoVal"
		  end
	     | ConApp {con, targs, arg} =>
		  if Con.equals (con, Con.overflow)
		     then
			expansive
			(ConApp
			 {con = con,
			  targs = targs,
			  arg = Option.map (arg, simplifyVarExp)})
		  else
		     let
			val arg = Option.map (arg, varExpInfo)
		     in nonExpansiveCon
			(fn () => Option.app (arg, VarInfo.delete),
			 Value.ConApp {con = con, targs = targs, arg = arg})
		     end			     
	     | Const c => nonExpansiveCon (fn () => (), Value.Const c)
	     | Handle {try, catch, handler} =>
		  expansive (Handle {try = simplifyExp try,
				     catch = catch,
				     handler = simplifyExp handler})
	     | Lambda l =>
		  let val isInlined = ref false
		  in nonExpansive
		     (fn () => if !isInlined then () else deleteLambda l,
		      fn () => (value := SOME (Value.Lambda {isInlined = isInlined,
							     lam = l})
				; SOME (fn () => Lambda (simplifyLambda l))))
		  end
	     | PrimApp {prim, args, targs} =>
		  let
		     fun make () =
			PrimApp {prim = prim, targs = targs,
				 args = simplifyVarExps args}
		  in if Prim.maySideEffect prim
			then expansive (make ())
		     else nonExpansive (fn () => (), fn () => SOME make)
		  end
	     | Profile _ => expansive exp
	     | Raise {exn, filePos} =>
		  expansive (Raise {exn = simplifyVarExp exn,
				    filePos = filePos})
	     | Select {tuple, offset} =>
		  let
		     fun normal x = Select {tuple = x, offset = offset}
		  in case varExpInfo tuple of
		     VarInfo.Poly x => finish (normal x, rest ())
		   | VarInfo.Mono {numOccurrences, value, varExp, ...} =>
			nonExpansive
			(fn () => inc (numOccurrences, ~1),
			 fn () =>
			 case !value of
			    NONE => SOME (fn () => normal varExp)
			  | SOME (Value.Tuple vs) => 
			       (inc (numOccurrences, ~1)
				; replaceInfo (var, info, Vector.sub (vs, offset))
				; NONE)
			  | _ => Error.bug "simplifyMonoVal: Select")
		  end
	     | Tuple xs =>
		  let val xs = varExpInfos xs
		  in nonExpansiveCon (fn () => VarInfo.deletes xs,
				      Value.Tuple xs)
		  end
	     | Var x => let val x = varExpInfo x
			in replaceInfo (var, info, x)
			   ; VarInfo.inc (x, ~1)
			   ; rest ()
			end
	     | App {func, arg} =>
		  let
		     val arg = varExpInfo arg
		     fun normal func =
			expansive (App {func = func,
					arg = VarInfo.varExp arg})
		  in case varExpInfo func of
		     VarInfo.Poly x => normal x
		   | VarInfo.Mono {numOccurrences, value, varExp, ...} => 
			case (!numOccurrences, !value) of
			   (1, SOME (Value.Lambda {isInlined, lam = l})) =>
			      let
				 val {arg = form, body, ...} = Lambda.dest l
			      in VarInfo.inc (arg, ~1)
				 ; replace (form, arg)
				 ; isInlined := true
				 ; numOccurrences := 0
				 ; expression body
			      end
			 | _ => normal varExp
		  end
	 end
      and simplifyLambda l: Lambda.t =
	 traceSimplifyLambda
	 (fn l => 
	  let
	     val {arg, argType, body} = Lambda.dest l
	  in
	     Lambda.new {arg = arg,
			 argType = argType,
			 body = simplifyExp body}
	  end) l
      val _ = countExp body
      val _ = Option.app (overflow, fn x => VarInfo.inc (varInfo x, 1))
      val body = simplifyExp body
      val overflow =
	 Option.map (overflow, fn x =>
		     VarExp.var (VarInfo.varExp (varInfo x)))
      val _ = Exp.clear body
      val _ = Vector.foreach (datatypes, fn {cons, ...} =>
			      Vector.foreach (cons, Con.clear o #con))
   in
      Program.T {datatypes = datatypes,
		 body = body,
		 overflow = overflow}
   end

val simplifyOnce =
   Trace.trace ("Xml.simplifyOnce", Program.layout, Program.layout) simplifyOnce

val simplify = simplifyOnce o simplifyOnce

val simplify =
   Trace.trace ("Xml.simplify", Program.layout, Program.layout) simplify

end
