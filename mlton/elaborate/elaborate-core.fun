(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor ElaborateCore (S: ELABORATE_CORE_STRUCTS): ELABORATE_CORE = 
struct

open S

local open Env
in
   structure TypeStr = TypeStr
   structure Vid = Vid
end

local open Ast
in 
   structure Aconst = Const
   structure Adec = Dec
   structure Aexp = Exp
   structure Amatch = Match
   structure Apat = Pat
   structure Atype = Type
   structure Avar = Var
   structure DatatypeRhs = DatatypeRhs
   structure DatBind = DatBind
   structure EbRhs = EbRhs
   structure Fixop = Fixop
   structure Longvid = Longvid
   structure Longtycon = Longtycon
   structure Record = Record
   structure SortedRecord = SortedRecord
   structure Strid = Strid
   structure TypBind = TypBind
end

local open CoreML
in
   structure Con = Con
   structure Cdec = Dec
   structure Cexp = Exp
   structure Cmatch = Match
   structure Cpat = Pat
   structure Cprim = Prim
   structure Ctype = Type
   structure Cvar = Var
   structure Scheme = Scheme
   structure Tycon = Tycon
   structure Type = Type
   structure Tyvar = Tyvar
end

local
   open Record
in
   structure Field = Field
end

structure Parse = PrecedenceParse (structure Ast = Ast
				   structure Env = Env)

structure Lookup =
   struct
      type t = Longtycon.t -> TypeStr.t

      fun fromFun f = f
      fun fromEnv (E: Env.t) longtycon = Env.lookupLongtycon (E, longtycon)
      fun lookup (l: t, c: Longtycon.t) = l c

      fun plusEnv (lookup: t, E: Env.t): t =
	 fn longtycon =>
	 case Env.peekLongtycon (E, longtycon) of
	    SOME typeFcn => typeFcn
	  | NONE => lookup longtycon

      fun plusTycons (f: t, v) =
	 if Vector.isEmpty v
	    then f
	 else
	    fn t => (case Longtycon.split t of
			([], t') =>
			   (case Vector.peek (v, fn (t'', _) =>
					      Ast.Tycon.equals (t', t'')) of
			       NONE => f t
			     | SOME (_, s) => s)
		      | _ => f t)
   end

(*
 * Replaces all tyvars that have the same name with a single tyvar.
 *)
fun elaborateType (ty: Atype.t, lookup: Lookup.t): Scheme.t =
   let 
      fun loop (ty: Atype.t, accum: Tyvar.t list): Type.t * Tyvar.t list =
	 case Atype.node ty of
	    Atype.Var a => (* rule 44 *)
	       let
		  fun loop tyvars =
		     case tyvars of
			[] => (* new type variable, add it to the accum *)
			   (Type.var a, a :: accum)
		      | a' :: tyvars =>
			   if Tyvar.sameName (a, a')
			      then (Type.var a', accum)
			   else loop tyvars
	       in loop accum
	       end
	  | Atype.Con (c, ts) => (* rules 46, 47 *)
	       let
		  val (ts, accum) = loops (ts, accum)
		  fun normal () = TypeStr.apply (lookup c, ts)
	       in (case (Ast.Longtycon.split c, Vector.length ts) of
		      (([], c), 2) =>
			 if Ast.Tycon.equals (c, Ast.Tycon.arrow)
			    then Type.arrow (Vector.sub (ts, 0),
					     Vector.sub (ts, 1))
			 else normal ()
		    | _ => normal (),
		   accum)
	       end
	  | Atype.Record r => (* rules 45, 49 *)
	       let
		  val (fs, ts) = SortedRecord.unzip r
		  val (ts, accum) = loops (ts, accum)
	       in
		  (Type.record (SortedRecord.zip (fs, ts)), accum)
	       end
      and loops (ts, ac) = Vector.mapAndFold (ts, ac, loop)
      val (ty, tyvars) = loop (ty, [])
   in Scheme.T {tyvars = Vector.fromList tyvars, ty = ty}
   end

fun elaborateTypeOpt (ty, lookup) =
   Option.map (ty, fn ty => Scheme.ty (elaborateType (ty, lookup)))

(* Returns a scheme, plus the type variables that occured in the ty but
 * not in the tyvars.
 *)
fun elaborateScheme (tyvars, ty: Atype.t, lookup: Lookup.t)
  : Scheme.t * Tyvar.t list =
   let val Scheme.T {tyvars = tyvars', ty} = elaborateType (ty, lookup)
      (* Replace each tyvar with the corresponding tyvar'.
       * Keep track of any tyvars' that are left over.
       *)
      val (tyvars, tyvars') =
	 Vector.foldr
	 (tyvars, ([], Vector.toList tyvars'), fn (a, (tyvars, tyvars')) =>
	  let
	     fun loop (tyvars', remaining) =
		case tyvars' of
		   [] => (a, remaining)
		 | a' :: tyvars' =>
		      if Tyvar.sameName (a, a')
			 then (a', remaining @ tyvars')
		      else loop (tyvars', a' :: remaining)
	     val (a, tyvars') = loop (tyvars', [])
	  in (a :: tyvars, tyvars')
	  end)
   in (Scheme.T {tyvars = Vector.fromList tyvars, ty = ty}, tyvars')
   end

fun elaborateClosedScheme arg: Scheme.t =
   let val (scheme, tyvars) = elaborateScheme arg
   in (case tyvars of
	  [] => ()
	| tyvar :: _ =>
	     Control.error
	     (Tyvar.region tyvar,
	      let open Layout
	      in seq [str "unbound type variables: ",
		     seq (separate (List.map (tyvars, Tyvar.layout), " "))]
	      end)) ;
      scheme
   end

fun elaborateTypBind (typBind, lookup: Lookup.t)
   : (Ast.Tycon.t * TypeStr.t) list =
   let
      val TypBind.T types = TypBind.node typBind
   in
      List.revMap
      (types, fn {tyvars, tycon, def} =>
       (tycon, TypeStr.def (elaborateClosedScheme (tyvars, def, lookup))))
   end

val info = Trace.info "elaboratePat"

fun elaboratePat (p: Apat.t, E: Env.t): Cpat.t =
   let
      fun bind (x: Ast.Var.t): CoreML.Var.t =
	 let
	    val x' = CoreML.Var.fromAst x
	    val _ = Env.extendVar (E, x, x')
	 in x'
	 end
      fun loop arg: Cpat.t =
	 Trace.traceInfo' (info, Apat.layout, Cpat.layout)
	 (fn p: Apat.t =>
	  case Apat.node p of
	    Apat.Wild => Cpat.Wild
	  | Apat.Var {name = x, ...} =>
	       (case Env.peekLongcon (E, Ast.Longvid.toLongcon x) of
		   SOME c => Cpat.Con {con = c, arg = NONE}
		 | NONE => (case Ast.Longvid.split x of
			       ([], x) => Cpat.Var (bind (Ast.Vid.toVar x))
			     | _ => Error.bug "longid in var pat"))
	  | Apat.Const c => Cpat.Const c
	  | Apat.Tuple ps => loopsContV (ps, Cpat.tuple)
	  | Apat.Record {items, flexible} =>
	       (* rules 36, 38, 39 and Appendix A, p.57 *)
	       let
		  val (fs, ps) =
		     Vector.unzip
		     (Vector.map
		      (items,
		       fn Apat.Item.Field fp => fp
			| Apat.Item.Vid (vid, tyo, po) =>
			     (Field.String (Ast.Vid.toString vid),
			      let
				 val p =
				    case po of
				       NONE => Apat.longvid (Longvid.short vid)
				     | SOME p =>
					  Apat.layered {fixop = Fixop.None,
							var = Ast.Vid.toVar vid,
							constraint = NONE,
							pat = p}
			      in case tyo of
				 NONE => p
			       | SOME ty => Apat.constraint (p, ty)
			      end)))
	       in loopsContV
		  (ps, fn ps =>		   
		   Cpat.record {record = Record.fromVector (Vector.zip (fs, ps)),
				flexible = flexible})
	       end
	  | Apat.List ps => loopsCont (ps, Cpat.list)
	  | Apat.FlatApp items => loop (Parse.parsePat (items, E))
	  | Apat.App (c, p) => Cpat.Con {con = Env.lookupLongcon (E, c),
					 arg = SOME (loop p)}
	  | Apat.Constraint (p, t) =>
	       Cpat.Constraint (loop p,
				Scheme.ty (elaborateType (t, Lookup.fromEnv E)))
	  | Apat.Layered {var = x, constraint, pat, ...} =>
	       Cpat.Layered (bind x,
			     loop (case constraint of
				      NONE => pat
				    | SOME t => Apat.constraint (pat, t)))
	       ) arg
      and loopsCont (ps: Apat.t list, cont: Cpat.t list -> Cpat.t): Cpat.t =
	 cont (elaboratePats (ps, E))
      and loopsContV (ps: Apat.t vector, cont: Cpat.t vector -> Cpat.t): Cpat.t =
	 cont (elaboratePatsV (ps, E))
   in loop p
   end

and elaboratePats (ps: Apat.t list, E): Cpat.t list =
   List.map (ps, fn p => elaboratePat (p, E))

and elaboratePatsV (ps: Apat.t vector, E): Cpat.t vector =
   Vector.map (ps, fn p => elaboratePat (p, E))

fun constrain (e, tyOpt) =
   case tyOpt of
      NONE => e
    | SOME ty => CoreML.Exp.Constraint (e, ty)
  
(*---------------------------------------------------*)
(*                   Declarations                    *)
(*---------------------------------------------------*)
  
local
   fun make name =
      Aexp.longvid (Ast.Longvid.long
		    ([Strid.fromString "Primitive",
		      Strid.fromString "Debug"],
		     Ast.Vid.fromString name))
in
   val enterDebug = make "enter"
   val leaveDebug = make "leave"
end

val info = Trace.info "elaborateDec"
val info' = Trace.info "elaborateExp"

fun elaborateDec (d, E) =
   let
      fun elabType t = elaborateType (t, Lookup.fromEnv E)
      fun elabTypeOpt t = elaborateTypeOpt (t, Lookup.fromEnv E)
      fun elabDatBind datBind =
	 (* rules 28, 29, 81, 82 *)
	 let
	    val lookup = Lookup.fromEnv E
	    val DatBind.T {datatypes, withtypes} = DatBind.node datBind
	    (* Build enough of an env so that that the withtypes
	     * and the constructor argument types can be evaluated.
	     *)
	    val (tycons, datatypes) =
	       Vector.unzip
	       (Vector.map
		(datatypes, fn {tyvars, tycon = name, cons} =>
		 let
		    val tycon = CoreML.Tycon.fromAst name
		 in
		    ((name, TypeStr.tycon tycon),
		     {name = name, tycon = tycon, tyvars = tyvars, cons = cons})
		 end))
	    val lookup = Lookup.plusTycons (lookup, tycons)
	    (* Elaborate the withtypes. *)
	    val tycons' = Vector.fromList (elaborateTypBind (withtypes, lookup))
	    val lookup =
	       Lookup.plusTycons (lookup, Vector.concat [tycons', tycons])
	    (* Elaborate the datatypes, this time including the constructors. *)
	    val (cons, tycons, datatypes) =
	       Vector.unzip3
	       (Vector.map
		(datatypes, fn {name, tyvars, tycon, cons} =>
		 let
		    val resultType =
		       Atype.con (name, Vector.map (tyvars, Atype.var))
		    val (cons, datatypeCons) =
		       Vector.unzip
		       (Vector.map
			(cons, fn (name, arg) =>
			 let
			    val con = Con.fromAst name
			 in ({name = name, con = con},
			     {con = con,
			      arg = Option.map (arg, fn t =>
						Scheme.ty
						(elaborateType (t, lookup)))})
			 end))
		 in (cons,
		     (name, TypeStr.data (tycon, cons)),
		     {tyvars = tyvars,
		      tycon = tycon,
		      cons = datatypeCons})
		 end))
	 in {cons = Vector.concatV cons,
	     tycons = Vector.concat [tycons, tycons'],
	     decs = Decs.single (Cdec.Datatype datatypes)}
	 end
      fun elabDec d =
	 Trace.traceInfo (info, Ast.Dec.layout, Layout.ignore, Trace.assertTrue)
	 (fn d =>
	  case Adec.node d of
	     Adec.Abstype {datBind, body} => (* rule 19 and p.57 *)
		let
		   val {cons, decs, tycons} = elabDatBind datBind
		   val (_, decs') =
		      Env.localCore
		      (E,
		       fn () => (Vector.foreach (cons, fn {name, con} =>
						 Env.extendCon (E, name, con))
				 ; Vector.foreach (tycons, fn (t, s) =>
						   Env.extendTycon (E, t, s))),
		       fn () => elabDec body)
                   val _ = Vector.foreach (tycons, fn (t, s) =>
					   Env.extendTycon (E, t, TypeStr.abs s))
		in Decs.append (decs, decs')
		end
	   | Adec.Datatype rhs =>
		let
		   val {cons, decs, tycons} =
		      case DatatypeRhs.node rhs of
			 DatatypeRhs.DatBind datBind => (* rule 17 *)
			    elabDatBind datBind
		       | DatatypeRhs.Repl {lhs, rhs} => (* rule 18 *)
			    let
			       val tyStr = Env.lookupLongtycon (E, rhs)
			    in
			       {cons = TypeStr.cons tyStr,
				decs = Decs.empty,
				tycons = Vector.new1 (lhs, tyStr)}
			    end
		   val _ = Vector.foreach (cons, fn {name, con} =>
					   Env.extendCon (E, name, con))
		   val _ = Vector.foreach (tycons, fn (t, s) =>
					   Env.extendTycon (E, t, s))
		in decs
		end
	   | Adec.Exception ebs =>
		Vector.fold
		(ebs, Decs.empty, fn ((exn, rhs), decs) =>
		 let
		    val (decs, exn') =
		       case EbRhs.node rhs of
			  EbRhs.Def c => (decs, Env.lookupLongcon (E, c))
			| EbRhs.Gen to =>
			     let val exn' = CoreML.Con.fromAst exn
			     in (Decs.add
				 (decs,
				  Cdec.Exception {con = exn',
						  arg = elabTypeOpt to}),
				 exn')
			     end
		    val _ = Env.extendExn (E, exn, exn')
		 in decs
		 end)
	   | Adec.Fix {ops, fixity} =>
		(Vector.foreach (ops, fn op' => Env.extendFix (E, op', fixity))
		 ; Decs.empty)
	   | Adec.Fun (tyvars, fbs) =>
		let
		   val clausess =
		      Vector.map
		      (fbs, fn {clauses, filePos} =>
		       {filePos = filePos,
			clauses = 
			Vector.map
			(clauses, fn {pats, resultType, body} =>
			 let
			    val {func, args} = Parse.parseClause (pats, E)
			 in {func = func,
			     args = args,
			     resultType = resultType,
			     body =
			     (if !Control.printAtFunEntry
				 then
				    let
				       val x = Avar.fromString "z"
				       val c =
					  Aexp.const
					  (Aconst.String (Avar.toString func))
				       fun make f = Aexp.app (f, c)
				    in
				       Aexp.seq
				       (Vector.new2
					(make enterDebug,
					 Aexp.lett (Vector.new1
						    (Adec.vall (Vector.new0 (),
								x, body)),
						    Aexp.seq
						    (Vector.new2
						     (make leaveDebug,
						      Aexp.var x)))))
				    end
			      else body)}
			 end)})
		   val funcs =
		      Vector.map (clausess, fn {clauses, ...} =>
				  if Vector.isEmpty clauses
				     then Error.bug "no clauses in fundec"
				  else #func (Vector.sub (clauses, 0)))
		   val newFuncs = Vector.map (funcs, Cvar.fromAst)
		   val _ =
		      Vector.foreach2 (funcs, newFuncs, fn (name, var) =>
				       Env.extendVar (E, name, var))
		   val decs =
		      Vector.map2
		      (clausess, newFuncs, fn ({clauses, filePos}, newFunc) =>
		       if Vector.isEmpty clauses
			  then Error.bug "empty clauses in fundec"
		       else
			  let
			     val {args, ...} = Vector.sub (clauses, 0)
			     val numVars = Vector.length args
			  in {var = newFunc, ty = NONE,
			      match =
			      let
				 val rs =
				    Vector.map
				    (clauses,
				     fn {args, resultType, body, ...} =>
				     let
					val (pats, body) =
					   Env.scope
					   (E, fn () =>
					    (elaboratePatsV (args, E),
					     elabExp body))
				     in (Cpat.tuple pats,
					 constrain (body,
						    elabTypeOpt resultType))
				     end)
				 fun make (i, xs) =
				    if i = 0
				       then
					  Cexp.casee
					  (Cexp.tuple
					   (Vector.fromList
					    (List.fold (xs, [], fn (x, xs) =>
							(Cexp.Var x) :: xs))),
					   Cmatch.T {filePos = filePos,
						     rules = rs})
				    else 
				       let val x = Cvar.newNoname ()
				       in Cexp.lambda (x, make (i - 1, x :: xs))
				       end
			      in if numVars = 1
				    then Cmatch.T {filePos = filePos,
						   rules = rs}
				 else (case make (numVars, []) of
					  Cexp.Fn m => m
					| _ => Error.bug "elabFbs")
			      end}
			  end)
		in Decs.single (Cdec.Fun {tyvars = tyvars,
					  decs = decs})
		end
	   | Adec.Local (d, d') =>
		Decs.append (Env.localCore (E,
					    fn () => elabDec d,
					    fn () => elabDec d'))
	   | Adec.Open paths =>
		let
		   (* The following code is careful to first lookup all of the
		    * paths in the current environment, and then extend the
		    * environment with all of the results.
		    * See rule 22 of the Definition.
		    *)
		   val _ =
		      Vector.foreach
		      (Vector.map (paths, fn p => Env.lookupLongstrid (E, p)),
		       fn s => Env.openStructure (E, s))
		in
		   Decs.empty
		end
	   | Adec.Overload (x, t, xs) =>
		let
		   val x' = Cvar.fromAst x
		   val scheme = elabType t
		   val _ = Env.extendVar (E, x, x')
		in
		   Decs.single
		   (Cdec.Overload
		    {var = x',
		     scheme = scheme,
		     ovlds = Vector.map (xs, fn x => Env.lookupLongvar (E, x))})
		end
	   | Adec.SeqDec ds =>
		Vector.fold (ds, Decs.empty, fn (d, decs) =>
			     Decs.append (decs, elabDec d))
	   | Adec.Type typBind =>
		(List.foreach
		 (elaborateTypBind (typBind, Lookup.fromEnv E),
		  fn (tyc, str) => Env.extendTycon (E, tyc, str))
		 ; Decs.empty)
	   | Adec.Val {tyvars, vbs, rvbs} =>
		let
		   (* Must do all the es and rvbs pefore the ps because of
		    * scoping rules.
		    *)
		   val es = Vector.map (vbs, elabExp o #exp)
		   val vars = Vector.map (rvbs, #var)
		   val newVars = Vector.map (vars, Cvar.fromAst)
		   val _ = Vector.foreach2 (vars, newVars, fn (name, var) =>
					    Env.extendVar (E, name, var))
		   val rvbs =
		      Vector.map2
		      (rvbs, newVars,
		       fn ({var, fixity, match, ty}, newvar) =>
		       {var = newvar,
			ty = elabTypeOpt ty,
			match = elabMatch match})
		   val ps = Vector.map (vbs, fn {pat, filePos, ...} =>
					{pat = elaboratePat (pat, E),
					 filePos = filePos})
		   val vbs = Vector.map2 (ps, es, fn ({pat, filePos}, e) =>
					  Cdec.Val {pat = pat,
						    filePos = filePos,
						    tyvars = tyvars,
						    exp = e})
		in Decs.append
		   (Decs.fromVector vbs,
		    Decs.single (Cdec.Fun {tyvars = tyvars,
					   decs = rvbs}))
		end
	     ) d
      and elabExps (es: Ast.Exp.t list): CoreML.Exp.t list =
	 List.map (es, elabExp)
      and elabExp arg: CoreML.Exp.t =
	 Trace.traceInfo (info', Ast.Exp.layout, CoreML.Exp.layout,
			  Trace.assertTrue)
	 (fn (e: Aexp.t) =>
	  let
	  in case Aexp.node e of
	     Aexp.Andalso (e, e') =>
		Cexp.andAlso (elabExp e, elabExp e')
	   | Aexp.App (e1, e2) =>
		Cexp.App (elabExp e1, elabExp e2)
	   | Aexp.Case (e, m) => Cexp.casee (elabExp e, elabMatch m)
	   | Aexp.Const c => Cexp.Const c
	   | Aexp.Constraint (e, t) =>
		Cexp.Constraint (elabExp e, Scheme.ty (elabType t))
	   | Aexp.FFI {name, ty} =>
		Cexp.Prim (Cprim.ffi (name, elabType ty))
	   | Aexp.FlatApp items => elabExp (Parse.parseExp (items, E))
	   | Aexp.Fn m => Cexp.Fn (elabMatch m)
	   | Aexp.Handle (try, match) =>
		Cexp.Handle (elabExp try, elabMatch match)
	   | Aexp.If (a, b, c) =>
		Cexp.iff (elabExp a, elabExp b, elabExp c)
	   | Aexp.Let (d, e) =>
		Env.scope
		(E, fn () => Cexp.Let (Decs.toVector (elabDec d), elabExp e))
	   | Aexp.List es => Cexp.list (elabExps es)
	   | Aexp.Orelse (e, e') => Cexp.orElse (elabExp e, elabExp e')
	   | Aexp.Prim {name, ty} => Cexp.Prim (Cprim.new (name, elabType ty))
	   | Aexp.Raise {exn, filePos} => Cexp.Raise {exn = elabExp exn,
						      filePos = filePos}
	   | Aexp.Record r => Cexp.Record (Record.map (r, elabExp))
	   | Aexp.Selector f => Cexp.selector f
	   | Aexp.Seq es => Cexp.seq (Vector.map (es, elabExp))
	   | Aexp.Var {name = id, ...} =>
		(case Env.lookupLongvid (E, id) of
		    Vid.Var x => Cexp.Var x
		  | Vid.ConAsVar c => Cexp.Con c
		  | Vid.Con c => Cexp.Con c
		  | Vid.Exn c => Cexp.Con c
		  | Vid.Prim p => Cexp.Prim p)
	   | Aexp.While {test, expr} =>
		Cexp.whilee {test = elabExp test, expr = elabExp expr}
	  end) arg
      and elabMatch (Amatch.T {filePos, rules}) =
	 Cmatch.T {filePos = filePos,
		   rules = 
		   Vector.map (rules, fn (pat, exp) =>
			       Env.scope (E, fn () => (elaboratePat (pat, E),
						       elabExp exp)))}
   in elabDec d
   end

end
