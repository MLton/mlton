(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor ElaborateSigexp (S: ELABORATE_SIGEXP_STRUCTS): ELABORATE_SIGEXP = 
struct

open S

local
   open Ast
in
   structure Atype = Type
   structure DatBind = DatBind
   structure DatatypeRhs = DatatypeRhs
   structure Equation = Equation
   structure Longstrid = Longstrid
   structure Longtycon = Longtycon
   structure Sigexp = Sigexp
   structure SortedRecord = SortedRecord
   structure Spec = Spec
   structure Strid = Strid
   structure TypBind = TypBind
   structure Tyvar = Tyvar
end

local
   open Env
in
   structure Interface = Interface
end

structure Con = Env.CoreML.Con

local
   open Interface
in
   structure Status = Status
   structure Tycon = Tycon
   structure TypeStr = TypeStr
end

local
   open TypeStr
in
   structure AdmitsEquality = AdmitsEquality
   structure Cons = Cons
   structure Kind = Kind
   structure Scheme = Scheme
   structure Type = Type
end

fun lookupLongtycon (E: Env.t,
		     I: Interface.t,
		     c: Ast.Longtycon.t): TypeStr.t =
   let
      fun env () = TypeStr.fromEnv (Env.lookupLongtycon (E, c))
      val (strids, t) = Ast.Longtycon.split c
   in
      case strids of
	 [] =>
	    (case Interface.peekLongtycon (I, c) of
		NONE => env ()
	      | SOME s => s)
       | s :: _ =>
	    (case Interface.peekStrid (I, s) of
		NONE => env ()
	      | SOME s =>
		   let
		      val r = ref NONE
		      val _ =
			 Interface.lookupLongtycon (I, c, fn s => r := SOME s)
		   in
		      case !r of
			 NONE => TypeStr.bogus Kind.Nary
		       | SOME s => s
		   end)
   end

fun elaborateType (ty: Atype.t, E: Env.t, I: Interface.t)
   : Tyvar.t vector * Type.t =
   let
      val tyvars = ref []
      fun loop (ty: Atype.t): Type.t =
	 case Atype.node ty of
	    Atype.Var a => (* rule 44 *)
	       Type.var
	       (case List.peek (!tyvars, fn a' => Tyvar.sameName (a, a')) of
		   NONE => (List.push (tyvars, a); a)
		 | SOME a => a)
	  | Atype.Con (c, ts) => (* rules 46, 47 *)
	       let
		  val ts = Vector.map (ts, loop)
		  fun normal () =
		     let
			val s = lookupLongtycon (E, I, c)
			val kind = TypeStr.kind s
			val numArgs = Vector.length ts
		     in
			if (case kind of
			       Kind.Arity n => n = numArgs
			     | Kind.Nary => true)
			   then TypeStr.apply (s, ts)
			else
			   let
			      open Layout
			      val _ = 
				 Control.error
				 (Atype.region ty,
				  seq [str "type constructor ",
				       Ast.Longtycon.layout c,
				       str " given ",
				       Int.layout numArgs,
				       str (if numArgs = 1
					       then " argument"
					       else " arguments"),
				       str " but wants ",
				       Kind.layout kind],
				  empty)
			   in
			      Type.bogus
			   end
		     end
	       in
		  case (Ast.Longtycon.split c, Vector.length ts) of
		     (([], c), 2) =>
			if Ast.Tycon.equals (c, Ast.Tycon.arrow)
			   then Type.arrow (Vector.sub (ts, 0),
					    Vector.sub (ts, 1))
			else normal ()
		   | _ => normal ()
	       end
	  | Atype.Record r => (* rules 45, 49 *)
	       Type.record (SortedRecord.map (r, loop))
      val ty = loop ty
   in
      (Vector.fromList (!tyvars), ty)
   end

val elaborateType =
   Trace.trace ("elaborateType", Atype.layout o #1, Type.layout o #2)
   elaborateType

fun elaborateScheme (tyvars: Tyvar.t vector, ty: Atype.t, E, I): Scheme.t =
   let
      val (tyvars', ty) = elaborateType (ty, E, I)
      val unbound =
	 Vector.keepAll
	 (tyvars', fn a =>
	  not (Vector.exists (tyvars, fn a' => Tyvar.sameName (a, a'))))
      val _ =
	 if 0 = Vector.length unbound
	    then ()
	 else
	    let
	       open Layout
	    in
	       Control.error (Tyvar.region (Vector.sub (tyvars', 0)),
			      seq [str (concat ["undefined type variable",
						if Vector.length unbound > 1
						   then "s"
						else "",
						": "]),
				   seq (separate
					(Vector.toListMap (unbound,
							   Tyvar.layout),
					 ", "))],
			      empty)
	    end
      (* Need to get the representatives that were chosen when elaborating the
       * type.
       *)
      val tyvars =
	 Vector.map
	 (tyvars, fn a =>
	  case Vector.peek (tyvars', fn a' => Tyvar.sameName (a, a')) of
	     NONE => a
	   | SOME a' => a')
   in
      Scheme.make (tyvars, ty)
   end

fun elaborateTypedescs (typedescs: {tycon: Ast.Tycon.t,
				    tyvars: Tyvar.t vector} list,
			{equality: bool}): Interface.t =
   Interface.types
   (Vector.fromListMap
    (typedescs, fn {tycon = name, tyvars} =>
     let
	val tycon = Tycon.make {hasCons = false}
	val _ =
	   Tycon.admitsEquality tycon
	   := (if equality
		  then AdmitsEquality.Sometimes
	       else AdmitsEquality.Never)
     in
	{name = name,
	 typeStr = TypeStr.tycon (tycon, Kind.Arity (Vector.length tyvars))}
     end))

val elaborateTypedescs =
   Trace.trace ("elaborateTypedescs", Layout.ignore, Interface.layout)
   elaborateTypedescs


fun elaborateDatBind (datBind: DatBind.t, E, I): Interface.t =
   let
      val region = DatBind.region datBind
      val DatBind.T {datatypes, withtypes} = DatBind.node datBind
      val change = ref false
      (* Build enough of an interface so that that the withtypes and the
       * constructor argument types can be elaborated.
       *)
      val (tycons, strs) =
	 Vector.unzip
	 (Vector.map
	  (datatypes, fn {cons, tycon = name, tyvars} =>
	   let
	      val tycon = Tycon.make {hasCons = true}
	   in
	      (tycon,
	       {name = name,
		typeStr = TypeStr.data (tycon,
					Kind.Arity (Vector.length tyvars),
					Cons.empty)})
	   end))
      val I' = Interface.types strs
      fun elabAll (I1: Interface.t): Interface.t =
	 let
	    val I2 = Interface.+ (I1, I)
	    val Is =
	       Vector.map2
	       (tycons, datatypes,
		fn (tycon, {cons, tycon = astTycon, tyvars, ...}) =>
		let
		   val resultType: Atype.t =
		      Atype.con (astTycon, Vector.map (tyvars, Atype.var))
		   val (cons, conArgs) =
		      Vector.unzip
		      (Vector.map
		       (cons, fn (name, arg) =>
			let
			   val con = Con.newNoname ()
			   val (makeArg, ty) =
			      case arg of
				 NONE => (fn _ => NONE, resultType)
			       | SOME t =>
				    (fn s =>
				     SOME (#1 (Type.deArrow (Scheme.ty s))),
				     Atype.arrow (t, resultType))
			   val scheme = elaborateScheme (tyvars, ty, E, I2)
			in
			   ({con = con: TypeStr.Con.t,
			     name = name,
			     scheme = scheme},
			    makeArg scheme)
			end))
		   val cons = Cons.T cons
		   val _ =
		      let
			 val r = Tycon.admitsEquality tycon
			 datatype z = datatype AdmitsEquality.t
		      in
			 case !r of
			    Always => Error.bug "datatype Always"
			  | Never => ()
			  | Sometimes =>
			       if Vector.forall
				  (conArgs, fn arg =>
				   case arg of
				      NONE => true
				    | SOME ty =>
					 Scheme.admitsEquality
					 (Scheme.make (tyvars, ty)))
				  then ()
			       else (r := Never; change := true)
		      end
		in
		   Interface.+
		   (Interface.cons cons,
		    Interface.types
		    (Vector.new1
		     {name = astTycon,
		      typeStr = TypeStr.data (tycon,
					      Kind.Arity (Vector.length tyvars),
					      cons)}))
		end)
	 in
	    Vector.fold (Is, Interface.empty, Interface.+)
	 end
      (* Maximize equality. *)
      fun loop (I: Interface.t): Interface.t =
	 let
	    val I = elabAll I
	 in
	    if !change
	       then (change := false; loop I)
	    else I
	 end
   in
      loop I'
   end

val info = Trace.info "elaborateSigexp"
val info' = Trace.info "elaborateSpec"

(* rule 65 *)
fun elaborateSigexp (sigexp: Sigexp.t, E: Env.t): Interface.t =
   let
      val _ = Interface.renameTycons := (fn () => Env.setTyconNames E)
      fun elaborateSigexp arg : Interface.t =
	 Trace.traceInfo' (info,
			   Layout.tuple2 (Sigexp.layout,
					  Interface.layout),
			   Interface.layout)
	 (fn (sigexp: Sigexp.t, I: Interface.t) =>
	  case Sigexp.node sigexp of
	     Sigexp.Spec spec => (* rule 62 *)
		let
		   val I = elaborateSpec (spec, I)
		   val _ = Interface.reportDuplicates (I, Sigexp.region sigexp)
		in
		   I
		end
	   | Sigexp.Var x => (* rule 63 *)
		Interface.copy (Env.lookupSigid (E, x))
	   | Sigexp.Where (sigexp, wheres) => (* rule 64 *)
		let
		   val time = Interface.Time.tick ()
		   val I' = elaborateSigexp (sigexp, I)
		   val _ =
		      Interface.wheres
		      (I',
		       Vector.fromListMap
		       (wheres, fn {tyvars, longtycon, ty} =>
			(longtycon,
			 TypeStr.def
			 (elaborateScheme (tyvars, ty, E, I),
			  Kind.Arity (Vector.length tyvars)))),
		       time)
		in
		   I'
		end) arg
      and elaborateSpec arg : Interface.t =
	 Trace.traceInfo' (info',
			   Layout.tuple2 (Ast.Spec.layout, Layout.ignore),
			   Layout.ignore)
	 (fn (spec: Ast.Spec.t, I: Interface.t) =>
	  case Spec.node spec of
	     Spec.Datatype rhs => (* rules 71, 72 *)
		(case DatatypeRhs.node rhs of
		    DatatypeRhs.DatBind b => elaborateDatBind (b, E, I)
		  | DatatypeRhs.Repl {lhs, rhs} =>
		       let
			  val s = lookupLongtycon (E, I, rhs)
		       in
			  Interface.+
			  (Interface.types (Vector.new1 {name = lhs,
							 typeStr = s}),
			   Interface.cons (TypeStr.cons s))
		       end)
	   | Spec.Empty => (* rule 76 *)
		Interface.empty
	   | Spec.Eqtype typedescs => (* rule 70 *)
		elaborateTypedescs (typedescs, {equality = true})
	   | Spec.Exception cons => (* rule 73 *)
		Interface.excons
		(Cons.T
		 (Vector.fromListMap
		  (cons, fn (name: TypeStr.Name.t,
			     arg: Ast.Type.t option) =>
		   let
		      val con = Con.newNoname ()
		      val (arg, ty) =
			 case arg of
			    NONE => (NONE, Type.exn)
			  | SOME t =>
			       let
				  val t =
				     Scheme.ty
				     (elaborateScheme (Vector.new0 (),
						       t, E, I))
			       in
				  (SOME t, Type.arrow (t, Type.exn))
			       end
		   in
		      {con = con: TypeStr.Con.t,
		       name= name: TypeStr.Name.t,
		       scheme = Scheme.make (Vector.new0 (), ty)}
		   end)))
	   | Spec.IncludeSigexp sigexp => (* rule 75 *)
		elaborateSigexp (sigexp, I)
	   | Spec.IncludeSigids sigids => (* Appendix A, p.59 *)
		List.fold
		(sigids, Interface.empty, fn (sigid, I) =>
		 Interface.+
		 (I, Interface.copy (Env.lookupSigid (E, sigid))))
	   | Spec.Seq (s, s') => (* rule 77 *)
		let
		   val I' = elaborateSpec (s, I)
		   val I'' = elaborateSpec (s', Interface.+ (I', I))
		in
		   Interface.+ (I', I'')
		end
	   | Spec.Sharing {equations, spec} =>
		(* rule 78 and section G.3.3 *)
		let
		   val time = Interface.Time.tick ()
		   val I' = elaborateSpec (spec, I)
		   fun share eqn =
		      case Equation.node eqn of
			 Equation.Structure ss =>
			    let
			       fun loop ss =
				  case ss of
				     [] => ()
				   | s :: ss =>
					(List.foreach
					 (ss, fn s' =>
					  Interface.share (I', s, s', time))
					 ; loop ss)
			    in
			       loop ss
			    end
		       | Equation.Type cs =>
			    case cs of
			       [] => ()
			     | c :: cs =>
				  List.foreach
				  (cs, fn c' =>
				   Interface.shareType (I', c, c', time))
		   val _ = List.foreach (equations, share)
		in
		   I'
		end
	   | Spec.Structure ss => (* rules 74, 84 *)
		Interface.strs
		(Vector.fromListMap
		 (ss, fn (strid, sigexp) =>
		  {interface = elaborateSigexp (sigexp, I),
		   name = strid}))
	   | Spec.Type typedescs => (* rule 69 *)
		elaborateTypedescs (typedescs, {equality = false})
	   | Spec.TypeDefs typBind =>
		(* Abbreviation on page 59,
		 * combined with rules 77 and 80.
		 *)
		let
		   val TypBind.T ds = TypBind.node typBind
		in
		   #2
		   (List.fold
		    (ds, (I, Interface.empty),
		     fn ({def, tycon, tyvars}, (I, I')) =>
		     let
			val I'' = 
			   Interface.types
			   (Vector.new1
			    {name = tycon,
			     typeStr = (TypeStr.def
					(elaborateScheme (tyvars, def, E, I),
					 Kind.Arity (Vector.length tyvars)))})
		     in
			(Interface.+ (I, I''), Interface.+ (I', I''))
		     end))
		end
	   | Spec.Val xts => (* rules 68, 79 *)
		Interface.vals
		(Vector.fromListMap
		 (xts, fn (x, t) =>
		  {name = Ast.Vid.fromVar x,
		   scheme = Scheme.make (elaborateType (t, E, I)),
		   status = Status.Var}))
		) arg
   in
      elaborateSigexp (sigexp, Interface.empty)
   end

val elaborateSigexp = 
   Trace.trace2 ("elaborateSigexp",
		 Sigexp.layout,
		 Layout.ignore,
		 Layout.ignore)
   elaborateSigexp

end
