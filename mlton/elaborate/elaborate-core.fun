(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
   structure Ffi = Ffi
   structure Cmatch = Match
   structure Cpat = Pat
   structure Cprim = Prim
   structure Cvar = Var
   structure Scheme = Scheme
   structure SourceInfo = SourceInfo
   structure Tycon = Tycon
   structure Type = Type
   structure Ctype = Type
   structure Tyvar = Tyvar
end

local
   open Record
in
   structure Field = Field
end

structure Parse = PrecedenceParse (structure Ast = Ast
				   structure Env = Env)

structure Apat =
   struct
      open Apat

      fun getName (p: t): string option =
	 case node p of
	    Var {name, ...} => SOME (Longvid.toString name)
	  | Constraint (p, _) => getName p
	  | FlatApp v =>
	       if 1 = Vector.length v
		  then getName (Vector.sub (v, 0))
	       else NONE
	  | Layered {var, ...} => SOME (Avar.toString var)
	  | _ => NONE

      val getName =
	 Trace.trace ("Apat.getName", layout, Option.layout String.layout)
	 getName
   end

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
   let
      val (scheme, tyvars) = elaborateScheme arg
      val _ =
	 case tyvars of
	    [] => ()
	  | tyvar :: _ =>
	       Control.error
	       (Tyvar.region tyvar,
		let open Layout
		in seq [str "unbound type variables: ",
			seq (separate (List.map (tyvars, Tyvar.layout), " "))]
		end,
		Layout.empty)
   in
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
      fun bind (x: Ast.Var.t): Cvar.t =
	 let
	    val x' = Cvar.fromAst x
	    val _ = Env.extendVar (E, x, x')
	 in x'
	 end
      fun loop arg: Cpat.t =
	 Trace.traceInfo' (info, Apat.layout, Cpat.layout)
	 (fn p: Apat.t =>
	  let
	     val region = Apat.region p
	     fun doit n = Cpat.makeRegion (n, region)
	  in
	     case Apat.node p of
		Apat.Wild => doit Cpat.Wild
	      | Apat.Var {name = x, ...} =>
		   (case Env.peekLongcon (E, Ast.Longvid.toLongcon x) of
		       SOME c => doit (Cpat.Con {con = c, arg = NONE})
		     | NONE =>
			  (case Ast.Longvid.split x of
			      ([], x) =>
				 doit (Cpat.Var (bind (Ast.Vid.toVar x)))
			    | _ => Error.bug "longid in var pat"))
	      | Apat.Const c => doit (Cpat.Const c)
	      | Apat.Tuple ps =>
		   loopsContV (ps, fn ps => Cpat.tuple (ps, region))
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
					   NONE =>
					      Apat.longvid (Longvid.short vid)
					 | SOME p =>
					      Apat.layered
					      {fixop = Fixop.None,
					       var = Ast.Vid.toVar vid,
					       constraint = NONE,
					       pat = p}
				  in
				     case tyo of
					NONE => p
				      | SOME ty => Apat.constraint (p, ty)
				  end)))
		   in
		      loopsContV
		      (ps, fn ps =>		   
		       Cpat.record
		       {flexible = flexible,
			record = Record.fromVector (Vector.zip (fs, ps)),
			region = region})
		   end
	      | Apat.List ps => loopsCont (ps, fn ps => Cpat.list (ps, region))
	      | Apat.FlatApp items => loop (Parse.parsePat (items, E))
	      | Apat.App (c, p) =>
		   doit (Cpat.Con {con = Env.lookupLongcon (E, c),
				   arg = SOME (loop p)})
	      | Apat.Constraint (p, t) =>
		   doit (Cpat.Constraint
			 (loop p,
			  Scheme.ty (elaborateType (t, Lookup.fromEnv E))))
	      | Apat.Layered {var = x, constraint, pat, ...} =>
		   doit (Cpat.Layered
			 (bind x,
			  loop (case constraint of
				   NONE => pat
				 | SOME t => Apat.constraint (pat, t))))
	  end) arg
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

fun constrain (e, tyOpt, r) =
   case tyOpt of
      NONE => e
    | SOME ty => Cexp.makeRegion (Cexp.Constraint (e, ty), r)
  
(*---------------------------------------------------*)
(*                   Declarations                    *)
(*---------------------------------------------------*)

structure Nest =
   struct
      type t = string list

      val layout = List.layout String.layout
   end

val info = Trace.info "elaborateDec"
val elabExpInfo = Trace.info "elaborateExp"

structure Ffi =
   struct
      open Ffi
	 
      structure Type =
	 struct
	    open Type
	       
	    val bogus = Bool
	       
	    val nullary =
	       [(Bool, Ctype.bool),
		(Char, Ctype.con (Tycon.char, Vector.new0 ()))]
	       @ List.map (IntSize.all, fn s => (Int s, Ctype.int s))
	       @ List.map (RealSize.all, fn s => (Real s, Ctype.real s))
	       @ List.map (WordSize.all, fn s => (Word s, Ctype.word s))

	    fun peekNullary t =
	       List.peek (nullary, fn (_, t') => Ctype.equals (t, t'))

	    val unary = [Tycon.array, Tycon.reff, Tycon.vector]

	    fun fromCtype (t: Ctype.t): t option =
	       case peekNullary t of
		  NONE =>
		     (case Ctype.deconOpt t of
			 NONE => NONE
		       | SOME (tycon, ts) =>
			    if List.exists (unary, fn tycon' =>
					    Tycon.equals (tycon, tycon'))
			       andalso 1 = Vector.length ts
			       andalso isSome (peekNullary
					       (Vector.sub (ts, 0)))
			       then SOME Pointer
			    else NONE)
		| SOME (t, _) => SOME t
	 end
	 
      fun parseCtype (ty: Ctype.t): (Type.t vector * Type.t option) option =
	 case Ctype.dearrowOpt ty of
	    NONE => NONE
	  | SOME (t1, t2) =>
	       let
		  fun finish (ts: Type.t vector) =
		     case Type.fromCtype t2 of
			NONE =>
			   if Ctype.equals (t2, Ctype.unit)
			      then SOME (ts, NONE)
			   else NONE
		      | SOME t => SOME (ts, SOME t)
	       in
		  case Ctype.detupleOpt t1 of 
		     NONE =>
			(case Type.fromCtype t1 of
			    NONE => NONE
			  | SOME u => finish (Vector.new1 u))
		   | SOME ts =>
			let
			   val us = Vector.map (ts, Type.fromCtype)
			in
			   if Vector.forall (us, isSome)
			      then finish (Vector.map (us, valOf))
			   else NONE
			end
	       end
   end

fun export (name: string, ty: Type.t, region: Region.t): Aexp.t =
   let
      val (args, exportId, res) =
	 case Ffi.parseCtype ty of
	    NONE =>
	       (Control.error
		(region,
		 let
		    open Layout
		 in
		    seq [str "invalid type for exported function: ",
			 Type.layout ty]
		 end,
		 Layout.empty)
		; (Vector.new0 (), 0, NONE))
	  | SOME (us, t) =>
	       let
		  val id = Ffi.addExport {args = us,
					  name = name,
					  res = t}
	       in
		  (us, id, t)
	       end
      open Ast
      val filePos = "<export>"
      fun strid name = Strid.fromString (name, region)
      fun id name =
	 Aexp.longvid
	 (Longvid.long ([strid "MLton", strid "FFI"], 
			Vid.fromString (name, region)))
      fun int (i: int): Aexp.t =
	 Aexp.const (Aconst.makeRegion (Aconst.Int (IntInf.fromInt i), region))
      val f = Var.fromString ("f", region)
   in
      Exp.fnn
      (Match.T
       {filePos = filePos,
	rules =
	Vector.new1
	(Pat.var f,
	 Exp.app
	 (id "register",
	  Exp.tuple
	  (Vector.new2
	   (int exportId,
	    Exp.fnn
	    (Match.T
	     {filePos = filePos,
	      rules =
	      Vector.new1
	      (Pat.tuple (Vector.new0 ()),
	       let
		  val map = Ffi.Type.memo (fn _ => Counter.new 0)
		  val varCounter = Counter.new 0
		  val (args, decs) =
		     Vector.unzip
		     (Vector.map
		      (args, fn u =>
		       let
			  val x =
			     Var.fromString
			     (concat ["x",
				      Int.toString (Counter.next varCounter)],
			      region)
			  val dec =
			     Dec.vall (Vector.new0 (),
				       x,
				       Exp.app
				       (id (concat
					    ["get", Ffi.Type.toString u]),
					int (Counter.next (map u))))
		       in
			  (x, dec)
		       end))
		  val resVar = Var.fromString ("res", region)
		  fun newVar () = Var.fromString ("none", region)
	       in
		  Exp.lett
		  (Vector.concat
		   [decs,
		    Vector.map 
		    (Vector.new4
		     ((newVar (), Exp.app (id "atomicEnd", Exp.unit)),
		      (resVar, Exp.app (Exp.var f,
					Exp.tuple (Vector.map (args, Exp.var)))),
		      (newVar (), Exp.app (id "atomicBegin", Exp.unit)),
		      (newVar (),
		       (case res of
			   NONE => Exp.unit
			 | SOME t => 
			      Exp.app (id (concat ["set", Ffi.Type.toString t]),
				       Exp.var resVar)))),
		     fn (x, e) => Dec.vall (Vector.new0 (), x, e))],
		   Exp.tuple (Vector.new0 ()))
	       end)})))))})
   end
   
fun elaborateDec (d, nest, E) =
   let
      fun elabType t = elaborateType (t, Lookup.fromEnv E)
      fun elabTypeOpt t = elaborateTypeOpt (t, Lookup.fromEnv E)
      fun elabDatBind datBind =
	 (* rules 28, 29, 81, 82 *)
	 let
	    val region = DatBind.region datBind
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
		    val tycon = Tycon.fromAst name
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
	     decs = Decs.single (Cdec.makeRegion (Cdec.Datatype datatypes,
						  region))}
	 end
      fun elabDec arg =
	 Trace.traceInfo (info,
			  Layout.tuple2 (Ast.Dec.layout, Nest.layout),
			  Layout.ignore, Trace.assertTrue)
	 (fn (d, nest) =>
	  let
	     val region = Adec.region d
	     fun doit n = Cexp.makeRegion (n, region)
	     val elabDec' = elabDec
	     fun elabDec (d: Adec.t) = elabDec' (d, nest)
	  in
	     case Adec.node d of
		Adec.Abstype {datBind, body} => (* rule 19 and p.57 *)
		   let
		      val {cons, decs, tycons} = elabDatBind datBind
		      val (_, decs') =
			 Env.localCore
			 (E,
			  fn () =>
			  (Vector.foreach (cons, fn {name, con} =>
					   Env.extendCon (E, name, con))
			   ; Vector.foreach (tycons, fn (t, s) =>
					     Env.extendTycon (E, t, s))),
			  fn () => elabDec body)
		      val _ =
			 Vector.foreach (tycons, fn (t, s) =>
					 Env.extendTycon (E, t, TypeStr.abs s))
		   in
		      Decs.append (decs, decs')
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
		   in
		      decs
		   end
	      | Adec.Exception ebs =>
		   Vector.fold
		   (ebs, Decs.empty, fn ((exn, rhs), decs) =>
		    let
		       val (decs, exn') =
			  case EbRhs.node rhs of
			     EbRhs.Def c => (decs, Env.lookupLongcon (E, c))
			   | EbRhs.Gen to =>
				let val exn' = Con.fromAst exn
				in (Decs.add
				    (decs,
				     Cdec.makeRegion
				     (Cdec.Exception {con = exn',
						      arg = elabTypeOpt to},
				      EbRhs.region rhs)),
				    exn')
				end
		       val _ = Env.extendExn (E, exn, exn')
		    in decs
		    end)
	      | Adec.Fix {ops, fixity} =>
		   (Vector.foreach (ops, fn op' =>
				    Env.extendFix (E, op', fixity))
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
			    in
			       {func = func,
				args = args,
				resultType = resultType,
				body = body}
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
				val {func, args, ...} = Vector.sub (clauses, 0)
				val nest = Avar.toString func :: nest
				val profile =
				   SourceInfo.function
				   {name = nest,
				    region = Avar.region func}
				val numVars = Vector.length args
				val match =
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
						  elabExp' (body, nest)))
					  in (Cpat.tuple (pats, region),
					      constrain (body,
							 elabTypeOpt resultType,
							 region))
					  end)
				      fun make (i, xs) =
					 if i = 0
					    then
					       Cexp.casee
					       (Cexp.tuple
						(Vector.rev
						 (Vector.fromListMap
						  (xs, fn x =>
						   doit (Cexp.Var x))),
						 region),
						Cmatch.new {filePos = filePos,
							    rules = rs},
						region)
					 else 
					    let
					       val x = Cvar.newNoname ()
					    in
					       Cexp.lambda
					       (x,
						make (i - 1, x :: xs),
						if i = 1
						   then SOME profile
						else NONE,
						region)
					    end
				   in if numVars = 1
					 then Cmatch.new {filePos = filePos,
							  rules = rs}
				      else (case Cexp.node (make (numVars, [])) of
					       Cexp.Fn {match = m, ...} => m
					     | _ => Error.bug "elabFbs")
				   end
			     in
				{match = match,
				 profile = if numVars = 1
					      then SOME profile
					   else NONE,
				 types = Vector.new0 (),
				 var = newFunc}
			     end)
		   in
		      Decs.single (Cdec.makeRegion (Cdec.Fun {tyvars = tyvars,
							      decs = decs},
						    region))
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
		      (* Elaborate the overloads before extending the var in
		       * case x appears in the xs.
		       *)
		      val ovlds =
			 Vector.map (xs, fn x => Env.lookupLongvar (E, x))
		      val _ = Env.extendVar (E, x, x')
		   in
		      Decs.single (Cdec.makeRegion
				   (Cdec.Overload {var = x',
						   scheme = scheme,
						   ovlds = ovlds},
				    region))
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
		      val hasCon: string option ref = ref NONE
		      fun checkName (name: Ast.Longvid.t): unit =
			 case !hasCon of
			    SOME _ => ()
			  | NONE =>
			       if isSome (Env.peekLongcon
					  (E, Ast.Longvid.toLongcon name))
				  then hasCon := SOME (Region.toString
						       (Ast.Longvid.region name))
			       else ()
		      (* Must do all the es and rvbs pefore the ps because of
		       * scoping rules.
		       *)
		      val es =
			 Vector.map (vbs, fn {pat, exp, ...} =>
				     elabExp'
				     (exp,
				      case Apat.getName pat of
					 NONE => "<anon>" :: nest
				       | SOME s => s :: nest))
		      fun varsAndTypes (p: Apat.t, vars, types)
			 : Avar.t list * Atype.t list =
			 let
			    fun error () =
			       Error.bug
			       (concat ["strange rec pattern: ",
					Layout.toString (Apat.layout p)])
			    datatype z = datatype Apat.node
			 in
			    case Apat.node p of
			       Wild => (vars, types)
			     | Var {name, ...} =>
				  (checkName name
				   ; (case Ast.Longvid.split name of
					 ([], x) =>
					    (Ast.Vid.toVar x :: vars, types)
				       | _ => Error.bug "longid in val rec pattern"))
			     | Constraint (p, t) =>
				  varsAndTypes (p, vars, t :: types)
			     | FlatApp ps =>
				  if 1 = Vector.length ps
				     then varsAndTypes (Vector.sub (ps, 0),
							vars, types)
				  else error ()
			     | Apat.Layered {var, constraint, pat, ...} =>
				  varsAndTypes (pat, var :: vars,
						case constraint of
						   NONE => types
						 | SOME t => t :: types)
			     | _ => error ()
			 end
		      val varsAndTypes =
			 Trace.trace ("varsAndTypes",
				      Apat.layout o #1,
				      Layout.tuple2 (List.layout Avar.layout,
						     List.layout Atype.layout))
			 varsAndTypes
		      val vts =
			 Vector.map
			 (rvbs, fn {pat, ...} =>
			  let
			     val (vars, types) = varsAndTypes (pat, [], [])
			     val (nest, var) =
				case vars of
				   [] => ("<anon>" :: nest, Cvar.newNoname ())
				 | x :: _ =>
				      let
					 val x' = Cvar.fromAst x
					 val _ =
					    List.foreach
					    (vars, fn y =>
					     Env.extendVar (E, y, x'))
				      in
					 (Avar.toString x :: nest, x')
				      end
			  in
			     {nest = nest,
			      types = (Vector.fromListMap
				       (types, Scheme.ty o elabType)),
			      var = var}
			  end)
		      val rvbs =
			 Vector.map2
			 (rvbs, vts,
			  fn ({pat, match, ...}, {nest, types, var}) =>
			  {match = elabMatch (match, nest),
			   profile = SOME (SourceInfo.function
					   {name = nest,
					    region = Apat.region pat}),
			   types = types,
			   var = var})
		      val ps = Vector.map (vbs, fn {pat, filePos, ...} =>
					   {pat = elaboratePat (pat, E),
					    filePos = filePos,
					    region = Apat.region pat})
		      val vbs =
			 Vector.map2 (ps, es, fn ({pat, filePos, region}, e) =>
				      Cdec.makeRegion
				      (Cdec.Val {pat = pat,
						 filePos = filePos,
						 tyvars = tyvars,
						 exp = e},
				       region))
		   in Decs.appends
		      [Decs.fromVector vbs,
		       Decs.single (Cdec.makeRegion
				    (Cdec.Fun {tyvars = tyvars,
					       decs = rvbs},
				     region)),
		       (* Hack to implement rule 126, which requires Bind to be
			* raised if any of the rvbs contains a constructor in a
			* pattern.  This, despite the fact that rule 26 allows
			* identifier status to be overridden for the purposes of
			* type checking.
			*)
		       case !hasCon of
			  NONE => Decs.empty
			| SOME filePos => 
			     Decs.single
			     (Cdec.makeRegion
			      (Cdec.Val
			       {exp = doit (Cexp.Raise
					    {exn = doit (Cexp.Con Con.bind),
					     filePos = filePos}),
				filePos = "",
				pat = Cpat.makeRegion (Cpat.Wild, region),
				tyvars = Vector.new0 ()},
			       region))]
		   end
	  end) arg
      and elabExp' (arg: Aexp.t * Nest.t): Cexp.t =
	 Trace.traceInfo (elabExpInfo,
			  Layout.tuple2 (Aexp.layout, Nest.layout),
			  Cexp.layout,
			  Trace.assertTrue)
	 (fn (e: Aexp.t, nest) =>
	  let
	     val region = Aexp.region e
	     fun doit n = Cexp.makeRegion (n, region)
	     fun elabExp e = elabExp' (e, nest)
	  in
	     case Aexp.node e of
		Aexp.Andalso (e, e') =>
		   Cexp.andAlso (elabExp e, elabExp e', region)
	      | Aexp.App (e1, e2) =>
		   doit (Cexp.App (elabExp e1, elabExp e2))
	      | Aexp.Case (e, m) =>
		   Cexp.casee (elabExp e, elabMatch (m, nest), region)
	      | Aexp.Const c => doit (Cexp.Const c)
	      | Aexp.Constraint (e, t) =>
		   doit (Cexp.Constraint (elabExp e,
					  Scheme.ty (elabType t)))
	      | Aexp.FlatApp items => elabExp (Parse.parseExp (items, E))
	      | Aexp.Fn m =>
		   doit
		   (Cexp.Fn
		    {match = elabMatch (m, nest),
		     profile = SOME (SourceInfo.function {name = nest,
							  region = region})})
	      | Aexp.Handle (try, match) =>
		   doit (Cexp.Handle (elabExp try, elabMatch (match, nest)))
	      | Aexp.If (a, b, c) =>
		   Cexp.iff (elabExp a, elabExp b, elabExp c, region)
	      | Aexp.Let (d, e) =>
		   Env.scope
		   (E, fn () =>
		    doit (Cexp.Let (Decs.toVector (elabDec (d, nest)),
				    elabExp e)))
	      | Aexp.List es => Cexp.list (List.map (es, elabExp), region)
	      | Aexp.Orelse (e, e') =>
		   Cexp.orElse (elabExp e, elabExp e', region)
	      | Aexp.Prim {kind, name, ty} =>
		   let
		      val ty = elabType ty
		      datatype z = datatype Ast.PrimKind.t
		      val simple = doit o Cexp.Prim
		   in
		      case kind of
			 BuildConst => simple (Cprim.buildConstant (name, ty))
		       | Const => simple (Cprim.constant (name, ty))
		       | Export =>
			    let
			       val ty = Scheme.ty ty
			    in
			       doit
			       (Cexp.Constraint
				(elabExp' (export (name, ty, region), nest),
				 Type.arrow (ty, Type.unit)))
			    end
		       | FFI => simple (Cprim.ffi (name, ty))
		       | Prim => simple (Cprim.new (name, ty))
		   end
	      | Aexp.Raise {exn, filePos} =>
		   doit (Cexp.Raise {exn = elabExp exn, filePos = filePos})
	      | Aexp.Record r =>
		   doit (Cexp.Record (Record.map (r, elabExp)))
	      | Aexp.Selector f =>
		   Cexp.selector (f, region)
	      | Aexp.Seq es =>
		   Cexp.seq (Vector.map (es, elabExp), region)
	      | Aexp.Var {name = id, ...} =>
		   doit (case Env.lookupLongvid (E, id) of
			    Vid.Var x => Cexp.Var x
			  | Vid.ConAsVar c => Cexp.Con c
			  | Vid.Con c => Cexp.Con c
			  | Vid.Exn c => Cexp.Con c
			  | Vid.Prim p => Cexp.Prim p)
	      | Aexp.While {test, expr} =>
		   Cexp.whilee {test = elabExp test,
				expr = elabExp expr,
				region = region}
	  end) arg
      and elabMatch (Amatch.T {filePos, rules}, nest: Nest.t) =
	 Cmatch.new {filePos = filePos,
		     rules = 
		     Vector.map (rules, fn (pat, exp) =>
				 Env.scope (E, fn () => (elaboratePat (pat, E),
							 elabExp' (exp, nest))))}
   in
      elabDec (d, nest)
   end

end
