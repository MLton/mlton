(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Ast (S: AST_STRUCTS): AST = 
struct

open S

structure AstCore = AstCore (AstAtoms (S))

open AstCore Layout
   
val layouts = List.map
structure Wrap = Region.Wrap
val node = Wrap.node
   
structure Equation =
   struct
      open Wrap
      datatype node =
	 Type of Longtycon.t list
       | Structure of Longstrid.t list
      type t = node Wrap.t
      type node' = node
      type obj = t

      fun layout eq =
	 case node eq of
	    Type longtycons =>
	       seq (str "sharing type "
		    :: separate (List.map (longtycons, Longtycon.layout), " = "))
	  | Structure longstrids =>
	       seq (str "sharing "
		    :: separate (List.map (longstrids, Longstrid.layout), " = "))
   end

type typedescs = {tyvars: Tyvar.t vector,
		  tycon: Tycon.t} list

datatype sigexpNode =
   Var of Sigid.t
 | Where of sigexp * {tyvars: Tyvar.t vector,
		      longtycon: Longtycon.t,
		      ty: Type.t} list
 | Spec of spec
and sigConst =
   None
  | Transparent of sigexp
  | Opaque of sigexp
and specNode =
   Datatype of DatatypeRhs.t
  | Empty
  | Eqtype of typedescs
  | Exception of (Con.t * Type.t option) list
  | IncludeSigexp of sigexp
  | IncludeSigids of Sigid.t list
  | Seq of spec * spec
  | Sharing of {spec: spec, equations: Equation.t list}
  | Structure of (Strid.t * sigexp) list
  | Type of typedescs
  | TypeDefs of TypBind.t
  | Val of (Var.t * Type.t) list
withtype spec = specNode Wrap.t
and sigexp = sigexpNode Wrap.t

fun layoutTypedescs (prefix, typedescs) =
   layoutAnds (prefix, typedescs, fn (prefix, {tyvars, tycon}) =>
	       seq [prefix,
		    Type.layoutApp (Tycon.layout tycon, tyvars, Tyvar.layout)])

fun layoutTypedefs (prefix, typBind) =
   let
      val TypBind.T ds = TypBind.node typBind
   in
      layoutAnds (prefix, Vector.toList ds, fn (prefix, {def, tycon, tyvars}) =>
		  seq [prefix,
		       Type.layoutApp (Tycon.layout tycon, tyvars, Tyvar.layout),
		       str " = ", Type.layout def])
   end

fun layoutSigexp (e: sigexp): Layout.t =
   case node e of
      Var s => Sigid.layout s
    | Where (e, ws) =>
	 let val e = layoutSigexp e
	 in case ws of
	    [] => e
	  | _ => 
	       seq [e, 
		    layoutAndsBind
		    (" where", "=", ws, fn {tyvars, longtycon, ty} =>
		     (OneLine,
		      seq [str "type ",
			   Type.layoutApp
			   (Longtycon.layout longtycon, tyvars,
			    Tyvar.layout)],
		      Type.layout ty))]
	 end
    | Spec s => align [str "sig",
		       indent (layoutSpec s, 3),
		       str "end"]

and layoutSigConst sigConst =
   case sigConst of
      None => empty
    | Transparent s => seq [str ": ", layoutSigexp s]
    | Opaque s => seq [str " :> ", layoutSigexp s]

and layoutSpec (s: spec): t =
   case node s of
      Empty => empty
    | Seq (s, s') => align [layoutSpec s, layoutSpec s']
    | Structure l =>
	 layoutAndsBind ("structure", ":", l, fn (strid, sigexp) =>
			 (case node sigexp of
			     Var _ => OneLine
			   | _ => Split 3,
				Strid.layout strid,
				layoutSigexp sigexp))
    | Type typedescs => layoutTypedescs ("type", typedescs)
    | TypeDefs typedefs => layoutTypedefs ("type", typedefs)
    | Eqtype typedescs => layoutTypedescs ("eqtype", typedescs)
    | Val sts =>
	 layoutAndsBind
	 ("val", ":", sts, fn (x, t) => (OneLine, Var.layout x, Type.layout t))
    | Datatype rhs => DatatypeRhs.layout rhs
    | Exception sts =>
	 layoutAnds
	 ("exception", sts, fn (prefix, (c, to)) => seq [prefix,
							 Con.layout c,
							 Type.layoutOption to])
    | IncludeSigexp s => seq [str "include ", layoutSigexp s]
    | IncludeSigids sigids =>
	 seq (str "include "
	      :: separate (List.map (sigids, Sigid.layout), " "))
    | Sharing {spec, equations} =>
	 align [layoutSpec spec,
		align (List.map (equations, Equation.layout))]

structure Sigexp =
   struct
      open Wrap
      type spec = spec
      type t = sigexp
      datatype node = datatype sigexpNode
      type node' = node
      type obj = t
	 
      fun wheree (sigexp: t, wherespecs, region): t =
	 case wherespecs of
	    [] => sigexp
	  | _ => makeRegion (Where (sigexp, wherespecs),
			     region)

      fun make n = makeRegion (n, Region.bogus)
	 
      val spec = make o Spec
      val var = make o Var
	 
      val layout = layoutSigexp
   end

structure SigConst =
   struct
      datatype t = datatype sigConst
      val layout = layoutSigConst
   end

structure Spec =
   struct
      open Wrap
      datatype node = datatype specNode
      type t = spec
      type node' = node
      type obj = t
	 
      val layout = layoutSpec
   end

(*---------------------------------------------------*)
(*                Strdecs and Strexps                *)
(*---------------------------------------------------*)

datatype strdecNode =
   Core of Dec.t
  | Local of strdec * strdec
  | Seq of strdec list
  | Structure of {constraint: SigConst.t,
		  def: strexp,
		  name: Strid.t} vector

and strexpNode =
   App of Fctid.t * strexp
  | Constrained of strexp * SigConst.t
  | Let of strdec * strexp
  | Struct of strdec
  | Var of Longstrid.t
withtype strexp = strexpNode Wrap.t
and strdec = strdecNode Wrap.t

fun layoutStrdec d =
   case node d of
      Core d => Dec.layout d
    | Local (d, d') => Pretty.locall (layoutStrdec d, layoutStrdec d')
    | Seq ds => align (layoutStrdecs ds)
    | Structure strbs =>
	 layoutAndsBind ("structure", "=", Vector.toList strbs,
			 fn {name, def, constraint} =>
			 (case node def of
			     Var _ => OneLine
			   | _ => Split 3,
				seq [Strid.layout name, SigConst.layout constraint],
				layoutStrexp def))

and layoutStrdecs ds = layouts (ds, layoutStrdec)
   
and layoutStrexp exp =
   case node exp of
      App (f, e) => seq [Fctid.layout f, str " ", paren (layoutStrexp e)]
    | Constrained (e, c) => mayAlign [layoutStrexp e, SigConst.layout c]
    | Let (dec, strexp) => Pretty.lett (layoutStrdec dec, layoutStrexp strexp)
    | Struct d => align [str "struct",
			 indent (layoutStrdec d, 3),
			 str "end"]
    | Var s => Longstrid.layout s
	 
structure Strexp =
   struct
      open Wrap
      type strdec = strdec
      type t = strexp
      datatype node = datatype strexpNode
      type node' = node
      type obj = t

      fun make n = makeRegion (n, Region.bogus)
      val var = make o Var
      val structt = make o Struct
      val constrained = make o Constrained
      val app = make o App
      val lett = make o Let
      val layout = layoutStrexp
   end

structure Strdec =
   struct
      open Wrap
      type t = strdec
      datatype node = datatype strdecNode
      type node' = node
      type obj = t

      fun make n = makeRegion (n, Region.bogus)
      val structuree = make o Structure

      val locall = make o Local
      val core = make o Core
      val seq = make o Seq

      val openn = core o Dec.openn

      val layout = layoutStrdec

      val fromExp = core o Dec.fromExp

      val trace = Trace.trace ("coalesce", layout, layout)
      fun coalesce (d: t): t =
	 trace
	 (fn d =>
	 case node d of
	    Core _ => d
	  | Local (d1, d2) =>
	       let
		  val d1 = coalesce d1
		  val d2 = coalesce d2
		  val node = 
		     case (node d1, node d2) of
			(Core d1', Core d2') =>
			   Core (Dec.makeRegion
				 (Dec.Local (d1', d2'),
				  Region.append (region d1, region d2)))
		      | _ => Local (d1, d2)
	       in
		  makeRegion (node, region d)
	       end
	  | Seq ds =>
	       let
		  fun finish (ds: Dec.t list, ac: t list): t list =
		     case ds of
			[] => ac
		      | _ =>
			   let
			      val d =
				 makeRegion (Core (Dec.makeRegion
						   (Dec.SeqDec (Vector.fromListRev ds),
						    Region.bogus)),
					     Region.bogus)
			   in
			      d :: ac
			   end
		  fun loop (ds, cores, ac) =
		     case ds of
			[] => finish (cores, ac)
		      | d :: ds =>
			   let
			      val d = coalesce d
			   in
			      case node d of
				 Core d => loop (ds, d :: cores, ac)
			       | Seq ds' => loop (ds' @ ds, cores, ac)
			       | _ => loop (ds, [], d :: finish (cores, ac))
			   end
		  val r = region d
	       in
		  case loop (ds, [], []) of
		     [] => makeRegion (Core (Dec.makeRegion
					     (Dec.SeqDec (Vector.new0 ()), r)),
				       r)
		   | [d] => d
		   | ds => makeRegion (Seq (rev ds), r)
	       end
	  | Structure _ => d) d
   end

structure FctArg =
   struct
      open Wrap
      datatype node =
	 Structure of Strid.t * Sigexp.t
       | Spec of Spec.t
      type t = node Wrap.t
      type node' = node
      type obj = t

      fun layout a =
	 case node a of
	    Structure (strid, sigexp) =>
	       seq [Strid.layout strid, str ": ", Sigexp.layout sigexp]
	  | Spec spec => Spec.layout spec
   end

structure Topdec =
   struct
      open Wrap
      datatype node =
	 BasisDone of {ffi: Longstrid.t}
       | Functor of {arg: FctArg.t,
		     body: Strexp.t,
		     name: Fctid.t,
		     result: SigConst.t} vector
       | Signature of (Sigid.t * Sigexp.t) vector
       | Strdec of Strdec.t
      type t = node Wrap.t
      type node' = node
      type obj = t
	 
      fun layout d =
	 case node d of
	    BasisDone {ffi} => seq [str "_basis_done ", Longstrid.layout ffi]
	  | Functor fctbs =>
	       layoutAndsBind ("functor", "=", Vector.toList fctbs,
			       fn {name, arg, result, body} =>
			       (Split 0,
				seq [Fctid.layout name, str " ",
				     paren (FctArg.layout arg),
				     layoutSigConst result],
				layoutStrexp body))
	  | Signature sigbs =>
	       layoutAndsBind ("signature", "=", Vector.toList sigbs,
			       fn (name, def) =>
			       (case Sigexp.node def of
				   Sigexp.Var _ => OneLine
				 | _ => Split 3,
				      Sigid.layout name,
				      Sigexp.layout def))
	  | Strdec d => Strdec.layout d


      fun make n = makeRegion (n, Region.bogus)
      val fromExp = make o Strdec o Strdec.fromExp
      val functorr = make o Functor
      val signaturee = make o Signature
      val strdec = make o Strdec
   end			

structure Program =
   struct
      datatype t = T of Topdec.t list list

      val empty = T []

      fun append (T ds1, T ds2) = T (ds1 @ ds2)

      fun layout (T dss) =
	 Layout.align (List.map (dss, fn ds =>
				 Layout.paren 
				 (Layout.align (List.map (ds, Topdec.layout)))))

      fun coalesce (T dss): t =
	 let
	    fun finish (sds, ac) =
	       case sds of
		  [] => ac
		| _ =>
		     let
			val t =
			   Topdec.makeRegion
			   (Topdec.Strdec (Strdec.makeRegion
					   (Strdec.Seq (rev sds), Region.bogus)),
			    Region.bogus)
		     in
			t :: ac
		     end
	    fun loop (ds, sds, ac) =
	       case ds of
		  [] => finish (sds, ac)
		| d :: ds =>
		     case Topdec.node d of
			Topdec.Strdec d => loop (ds, d :: sds, ac)
		      | _ => loop (ds, [], d :: finish (sds, ac))
	 in
	    T (List.map (dss, fn ds => rev (loop (ds, [], []))))
	 end

      val coalesce =
	 Trace.trace ("Ast.Program.coalesce", layout, layout) coalesce

      fun size (T dss): int =
	 let
	    val n = ref 0
	    fun inc () = n := 1 + !n
	    fun dec (d: Dec.t): unit =
	       let
		  datatype z = datatype Dec.node
	       in
		  case Dec.node d of
		     Abstype {body, ...} => dec body
		   | Exception cs => Vector.foreach (cs, fn _ => inc ())
		   | Fun (_, ds) =>
			Vector.foreach (ds, fn clauses =>
					Vector.foreach (clauses, exp o #body))
		   | Local (d, d') => (dec d; dec d')
		   | SeqDec ds => Vector.foreach (ds, dec)
		   | Val {vbs, rvbs, ...} =>
			(Vector.foreach (vbs, exp o #exp)
			 ; Vector.foreach (rvbs, match o #match))
		   | _ => ()
	       end
	    and exp (e: Exp.t): unit =
	       let
		  val _ = inc ()
		  datatype z = datatype Exp.node
	       in
		  case Exp.node e of
		     Andalso (e1, e2) => (exp e1; exp e2)
		   | App (e, e') => (exp e; exp e')
		   | Case (e, m) => (exp e; match m)
		   | Constraint (e, _) => exp e
		   | FlatApp es => exps es
		   | Fn m => match m
		   | Handle (e, m) => (exp e; match m)
		   | If (e1, e2, e3) => (exp e1; exp e2; exp e3)
		   | Let (d, e) => (dec d; exp e)
		   | List es => Vector.foreach (es, exp)
		   | Orelse (e1, e2) => (exp e1; exp e2)
		   | Raise exn => exp exn
		   | Record r => Record.foreach (r, exp)
		   | Seq es => exps es
		   | While {test, expr} => (exp test; exp expr)
		   | _ => ()
	       end
	    and exps es = Vector.foreach (es, exp)
	    and match m =
	       let
		  val Match.T rules = Match.node m
	       in
		  Vector.foreach (rules, exp o #2)
	       end
	    fun strdec d =
	       case Strdec.node d of
		  Core d => dec d
		| Local (d, d') => (strdec d; strdec d')
		| Seq ds => List.foreach (ds, strdec)
		| Structure ds =>
		     Vector.foreach (ds, fn {def, ...} => strexp def)
	    and strexp e =
	       case Strexp.node e of
		  Struct d => strdec d
		| Constrained (e, _) => strexp e
		| App (_, e) => strexp e
		| Let (d, e) => (strdec d; strexp e)
		| _ => ()

	    fun topdec d =
	       let
		  datatype z = datatype Topdec.node
	       in
		  case Topdec.node d of
		     Functor ds =>
			Vector.foreach (ds, fn {body, ...} => strexp body)
		   | Strdec d => strdec d
		   | _ => ()
	       end
	    val _ = List.foreach (dss, fn ds => List.foreach (ds, topdec))
	 in
	    !n
	 end
   end

end
