(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Ast (S: AST_STRUCTS): AST = 
struct

open S

structure Const = AstConst ()
structure Field = Field ()
structure Record = Record (val isSorted = false
			   structure Field = Field)
structure SortedRecord = Record (val isSorted = true
				 structure Field = Field)
structure Tyvar = Tyvar ()
   
structure AstAtoms = AstAtoms (structure Const = Const
			       structure Record = Record
			       structure SortedRecord = SortedRecord
			       structure Tyvar = Tyvar)

structure AstCore = AstCore (open AstAtoms)

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

type typedefs = {tyvars: Tyvar.t vector,
		 tycon: Tycon.t,
		 ty: Type.t} list

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
   Empty
  | Seq of spec * spec
  | Structure of (Strid.t * sigexp) list
  | Type of typedescs
  | TypeDefs of typedefs
  | Eqtype of typedescs
  | Val of (Var.t * Type.t) list
  | Datatype of DatatypeRhs.t
  | Exception of (Con.t * Type.t option) list
  | IncludeSigexp of sigexp
  | IncludeSigids of Sigid.t list
  | Sharing of {spec: spec, equations: Equation.t list}
withtype spec = specNode Wrap.t
and sigexp = sigexpNode Wrap.t

fun layoutTypedescs (prefix, typedescs) =
   layoutAnds (prefix, typedescs, fn (prefix, {tyvars, tycon}) =>
	       seq [prefix,
		    Type.layoutApp (Tycon.layout tycon, tyvars, Tyvar.layout)])

fun layoutTypedefs (prefix, typedescs) =
   layoutAnds (prefix, typedescs, fn (prefix, {tyvars, tycon, ty}) =>
	       seq [prefix,
		    Type.layoutApp (Tycon.layout tycon, tyvars, Tyvar.layout),
		    str " = ", Type.layout ty])

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
	 
      fun wheree (sigexp: t, wherespecs): t =
	 case wherespecs of
	    [] => sigexp
	  | _ => makeRegion (Where (sigexp, wherespecs),
			     region sigexp)

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
	 
      val empty = makeRegion (Empty, Region.bogus)
	 
      val layout = layoutSpec
   end

(*---------------------------------------------------*)
(*                Strdecs and Strexps                *)
(*---------------------------------------------------*)

datatype strdecNode =
   Structure of {name: Strid.t,
		 def: strexp,
		 constraint: SigConst.t} list
  | Local of strdec * strdec
  | Seq of strdec list
  | Core of Dec.t
and strexpNode =
   Var of Longstrid.t
  | Struct of strdec
  | Constrained of strexp * SigConst.t
  | App of Fctid.t * strexp
  | Let of strdec * strexp
withtype strexp = strexpNode Wrap.t
and strdec = strdecNode Wrap.t

fun layoutStrdec d =
   case node d of
      Structure strbs =>
	 layoutAndsBind ("structure", "=", strbs, fn {name, def, constraint} =>
			 (case node def of
			     Var _ => OneLine
			   | _ => Split 3,
				seq [Strid.layout name, SigConst.layout constraint],
				layoutStrexp def))
    | Local (d, d') => layoutLocal (layoutStrdec d, layoutStrdec d')
    | Seq ds => align (layoutStrdecs ds)
    | Core d => Dec.layout d

and layoutStrdecs ds = layouts (ds, layoutStrdec)
   
and layoutStrexp exp =
   case node exp of
      Var s => Longstrid.layout s
    | Struct d => align [str "struct",
			 indent (layoutStrdec d, 3),
			 str "end"]
    | Constrained (e, c) => mayAlign [layoutStrexp e, SigConst.layout c]
    | App (f, e) =>
	 seq [Fctid.layout f, str "(", layoutStrexp e, str ")"]
    | Let (dec, strexp) => layoutLet (layoutStrdec dec, layoutStrexp strexp)
	 
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
	 Strdec of Strdec.t
       | Signature of (Sigid.t * Sigexp.t) list
       | Functor of {name: Fctid.t,
		     arg: FctArg.t,
		     result: SigConst.t,
		     body: Strexp.t} list
      type t = node Wrap.t
      type node' = node
      type obj = t
	 
      fun layout d =
	 case node d of
	    Strdec d => Strdec.layout d
	  | Signature sigbs =>
	       layoutAndsBind ("signature", "=", sigbs, fn (name, def) =>
			       (case Sigexp.node def of
				   Sigexp.Var _ => OneLine
				 | _ => Split 3,
				      Sigid.layout name,
				      Sigexp.layout def))
	  | Functor fctbs =>
	       layoutAndsBind ("functor", "=", fctbs,
			       fn {name, arg, result, body} =>
			       (Split 0,
				seq [Fctid.layout name,
				     paren (FctArg.layout arg),
				     layoutSigConst result],
				layoutStrexp body))

      fun make n = makeRegion (n, Region.bogus)
      val fromExp = make o Strdec o Strdec.fromExp
      val functorr = make o Functor
      val signaturee = make o Signature
      val strdec = make o Strdec
   end			

structure Program =
   struct
      datatype t = T of Topdec.t list

      fun layout (T ds) = Layout.align (List.map (ds, Topdec.layout))

      fun size (T ds): int =
	 let
	    open Dec Exp Strexp Strdec Topdec
	    val n = ref 0
	    fun inc () = n := 1 + !n

	    fun dec (d: Dec.t): unit =
	       case Dec.node d of
		  Val {vbs, rvbs, ...} =>
		     (Vector.foreach (vbs, exp o #exp)
		      ; Vector.foreach (rvbs, match o #match))
		| Fun (_, ds) =>
		     Vector.foreach (ds, fn {clauses, ...} =>
				     Vector.foreach (clauses, exp o #body))
		| Abstype {body, ...} => dec body
		| Exception cs => Vector.foreach (cs, fn _ => inc ())
		| SeqDec ds => Vector.foreach (ds, dec)
		| Dec.Local (d, d') => (dec d; dec d')
		| _ => ()

	    and exp (e: Exp.t): unit =
	       (inc ();
		case Exp.node e of
		   Fn m => match m
		 | FlatApp es => exps es
		 | Exp.App (e, e') => (exp e; exp e')
		 | Case (e, m) => (exp e; match m)
		 | Exp.Let (d, e) => (dec d; exp e)
		 | Exp.Seq es => exps es
		 | Record r => Record.foreach (r, exp)
		 | List es => List.foreach (es, exp)
		 | Constraint (e, _) => exp e
		 | Handle (e, m) => (exp e; match m)
		 | Raise {exn, ...} => exp exn
		 | If (e1, e2, e3) => (exp e1; exp e2; exp e3)
		 | Andalso (e1, e2) => (exp e1; exp e2)
		 | Orelse (e1, e2) => (exp e1; exp e2)
		 | While {test, expr} => (exp test; exp expr)
		 | _ => ())

	    and exps es = Vector.foreach (es, exp)
	       
	    and match (Match.T {rules, ...}) = Vector.foreach (rules, exp o #2)
		     
	    fun strdec d =
	       case Strdec.node d of
		  Structure ds => List.foreach (ds, fn {def, ...} => strexp def)
		| Seq ds => List.foreach (ds, strdec)
		| Local (d, d') => (strdec d; strdec d')
		| Core d => dec d
	    and strexp e =
	       case Strexp.node e of
		  Struct d => strdec d
		| Constrained (e, _) => strexp e
		| App (_, e) => strexp e
		| Let (d, e) => (strdec d; strexp e)
		| _ => ()

	    fun topdec d =
	       case Topdec.node d of
		  Strdec d => strdec d
		| Functor ds => List.foreach (ds, fn {body, ...} => strexp body)
		| _ => ()
	 in List.foreach (ds, topdec);
	    !n
	 end
   end

end
