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

local open Ast
in structure Atype = Type
   structure DatBind = DatBind
   structure DatatypeRhs = DatatypeRhs
   structure Equation = Equation
   structure Longstrid = Longstrid
   structure Longtycon = Longtycon
   structure Sigexp = Sigexp
   structure Spec = Spec
   structure Strid = Strid
end

local
   open Env
in
   structure Interface = Interface
   structure Maker = InterfaceMaker
end

structure Set = DisjointSet

val info = Trace.info "elaborateSigexp"
val info' = Trace.info "elaborateSpec"

(* rule 65 *)
fun elaborateSigexp (sigexp: Sigexp.t, E: Env.t): Interface.t =
   case Sigexp.node sigexp of
      Sigexp.Var s => Env.lookupSigid (E, s)
    | _ =>
	 let
	    val m = Env.makeInterfaceMaker E
	    fun elaborateSigexp arg : Interface.t =
	       Trace.traceInfo' (info, Sigexp.layout, Layout.ignore)
	       (fn (sigexp: Sigexp.t) =>
		case Sigexp.node sigexp of
		   Sigexp.Spec spec => (* rule 62 *)
		      #2 (Maker.makeInterface (m, fn () => elaborateSpec spec))
		 | Sigexp.Var x => (* rule 63 *)
		      Env.lookupSigid (E, x)
		 | Sigexp.Where (sigexp, _) => (* rule 64 *)
		      elaborateSigexp sigexp) arg
	    and elaborateSpec arg : unit =
	       Trace.traceInfo' (info', Ast.Spec.layout, Layout.ignore)
	       (fn (spec: Ast.Spec.t) =>
		case Spec.node spec of
		   Spec.Datatype rhs => (* rules 71, 72 *)
		      (case DatatypeRhs.node rhs of
			  DatatypeRhs.DatBind b =>
			     let
				val DatBind.T {datatypes, withtypes} =
				   DatBind.node b
				val _ =
				   Vector.foreach
				   (datatypes, fn {tycon, cons, ...} =>
				    Maker.addTycon (m, tycon,
						    Vector.map (cons, #1)))
				val Ast.TypBind.T l =
				   Ast.TypBind.node withtypes
				val _ =
				   List.foreach
				   (l, fn {tycon, ...} =>
				    Maker.addTycon (m, tycon, Vector.new0 ()))
			     in ()
			     end
			| DatatypeRhs.Repl {lhs, rhs} =>
			     Maker.addTycon (m, lhs,
					     Maker.lookupLongtycon (m, rhs)))
		 | Spec.Empty => (* rule 76 *)
		      ()
		 | Spec.Eqtype typedescs => (* rule 70 *)
		      List.foreach (typedescs, fn {tycon, ...} =>
				    Maker.addTycon (m, tycon, Vector.new0 ()))
		 | Spec.Exception cons => (* rule 73 *)
		      List.foreach
		      (cons, fn (con, _) => Maker.addExcon (m, con))
		 | Spec.IncludeSigexp sigexp => (* rule 75 *)
		      Maker.includeInterface (m, elaborateSigexp sigexp)
		 | Spec.IncludeSigids sigids => (* Appendix A, p.59 *)
		      List.foreach
		      (sigids, fn sigid =>
		       Maker.includeInterface (m, Env.lookupSigid (E, sigid)))
		 | Spec.Seq (s, s') => (* rule 77 *)
		      (elaborateSpec s; elaborateSpec s')
		 | Spec.Sharing {spec, ...} =>
		      (* rule 78 and section G.3.3 *)
		      elaborateSpec spec
		 | Spec.Structure ss => (* rules 74, 84 *)
		      List.foreach (ss, fn (strid, sigexp) =>
				    Maker.addStrid
				    (m, strid, elaborateSigexp sigexp))
		 | Spec.Type typedescs => (* rule 69 *)
		      List.foreach (typedescs, fn {tycon, ...} =>
				    Maker.addTycon (m, tycon, Vector.new0 ()))
		 | Spec.TypeDefs typedefs => (* rule 69 *)
		      List.foreach (typedefs, fn {tycon, ...} =>
				    Maker.addTycon (m, tycon, Vector.new0 ()))
		 | Spec.Val xts => (* rules 68, 79 *)
		      List.foreach (xts, fn (x, _) => Maker.addVar (m, x))
		   ) arg
	 in elaborateSigexp sigexp
	 end

val elaborateSigexp = 
   Trace.trace2 ("elaborateSigexp",
		 Sigexp.layout,
		 Layout.ignore,
		 Layout.ignore)
   elaborateSigexp

end
