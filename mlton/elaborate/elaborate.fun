(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Elaborate (S: ELABORATE_STRUCTS): ELABORATE= 
struct

open S

local open Ast
in structure FctArg = FctArg
   structure Longstrid = Longstrid
   structure Topdec = Topdec
   structure SigConst = SigConst
   structure Sigexp = Sigexp
   structure Strdec = Strdec
   structure Strid = Strid
   structure Strexp = Strexp
end

local open CoreML
in structure Con = Con
   structure Prim = Prim
   structure Scheme = Scheme
   structure Tycon = Tycon
   structure Type = Type
end

structure Decs = Decs (structure Ast = Ast
		       structure CoreML = CoreML)
structure Env = ElaborateEnv (structure Ast = Ast
			      structure CoreML = CoreML
			      structure Decs = Decs)

local
   open Env
in
   structure Decs = Decs
   structure FunctorClosure = FunctorClosure
   structure Interface = Interface
   structure Structure = Structure
   structure TypeStr = TypeStr
end

structure ElaborateSigexp = ElaborateSigexp (structure Ast = Ast
					     structure Env = Env
					     structure Interface = Interface)

structure ElaborateCore = ElaborateCore (structure Ast = Ast
					structure CoreML = CoreML
					structure Decs = Decs
					structure Env = Env)

val info = Trace.info "elaborateStrdec"
val info' = Trace.info "elaborateTopdec"
	  
fun elaborateProgram (Ast.Program.T decs, E: Env.t) =
   let
      fun elabSigexp s = ElaborateSigexp.elaborateSigexp (s, E)
      fun elabSigexpConstraint (cons: SigConst.t, S: Structure.t): Structure.t =
	 let
	    fun s (sigexp, opaque) =
	       let val interface = elabSigexp sigexp
	       in Structure.cut {str = S,
				 interface = interface,
				 opaque = opaque,
				 region = Sigexp.region sigexp}
	       end
	 in case cons of
	    SigConst.None => S
	  | SigConst.Transparent sigexp => s (sigexp, false)
	  | SigConst.Opaque sigexp => s (sigexp, true)
	 end	 

      fun elabStrdec arg: Decs.t =
	 Trace.traceInfo' (info, Strdec.layout, Layout.ignore)
	 (fn d: Strdec.t =>
	  case Strdec.node d of
	     Strdec.Core d => (* rule 56 *)
		ElaborateCore.elaborateDec (d, E)
	   | Strdec.Local (d, d') => (* rule 58 *)
		Decs.append (Env.localModule (E,
					      fn () => elabStrdec d,
					      fn () => elabStrdec d'))
	   | Strdec.Seq ds => (* rule 60 *)
		List.fold
		(ds, Decs.empty, fn (d, decs) =>
		 Decs.append (decs, elabStrdec d))
	   | Strdec.Structure strbinds => (* rules 57, 61 *)
		List.fold
		(strbinds, Decs.empty, fn ({name, def, constraint}, decs) =>
		 let val (decs', S) = elabStrexp def
		    val _ = 
		       Env.extendStrid
		       (E, name, elabSigexpConstraint (constraint, S))
		 in Decs.append (decs, decs')
		 end)
		) arg

      and elabStrexp (e: Strexp.t): Decs.t * Structure.t =
	 case Strexp.node e of
	    Strexp.App (fctid, strexp) => (* rules 54, 154 *)
	       let
		  val (decs, S) = elabStrexp strexp
		  val (decs', S) =
		     FunctorClosure.apply (Env.lookupFctid (E, fctid),
					   S, Strexp.region strexp)
	       in (Decs.append (decs, decs'), S)
	       end
	  | Strexp.Constrained (e, c) => (* rules 52, 53 *)
	       let val (decs, S) = elabStrexp e
	       in (decs, elabSigexpConstraint (c, S))
	       end
	  | Strexp.Let (d, e) => (* rule 55 *)
	       Env.scope
	       (E, fn () =>
		let val decs = elabStrdec d
		   val (decs', S) = elabStrexp e
		in (Decs.append (decs, decs'), S)
		end)
	  | Strexp.Struct d => (* rule 50 *)
	       Env.makeStructure (E, fn () => elabStrdec d)
	  | Strexp.Var p => (* rule 51 *)
	       (Decs.empty, Env.lookupLongstrid (E, p))

      fun elabTopdec arg: Decs.t =
	 Trace.traceInfo' (info', Topdec.layout, Decs.layout)
	 (fn (d: Topdec.t) =>
	  case Topdec.node d of
	     Topdec.Strdec d => elabStrdec d
	   | Topdec.Signature sigbinds =>
		(List.foreach
		 (sigbinds, fn (sigid, sigexp) =>
		  Env.extendSigid (E, sigid, elabSigexp sigexp))
		 ; Decs.empty)
	   | Topdec.Functor funbinds =>
		(* Appendix A, p.58 *)
		(List.foreach
		 (funbinds, fn ({name, arg, result, body}) =>
		  let
		     val body = Strexp.constrained (body, result)
		     val (arg, argSig, body) =
			case FctArg.node arg of
			   FctArg.Structure (arg, argSig) => (arg, argSig, body)
			 | FctArg.Spec spec =>
			      let val strid = Strid.fromString "ZZZNewStridZZZ"
			      in (strid,
				  Sigexp.spec spec,
				  Strexp.lett
				  (Strdec.openn (Vector.new1
						 (Longstrid.short strid)),
				   body))
			      end
		     val argInt = elabSigexp argSig
		     val closure =
			Env.functorClosure
			(E, argInt,
			 fn formal => (Env.extendStrid (E, arg, formal)
				       ; elabStrexp body))
		  in Env.extendFctid (E, name, closure)
		  end)
		 ; Decs.empty)
		) arg
   in List.fold (decs, Decs.empty, fn (d, decs) =>
		 Decs.append (decs, elabTopdec d))
   end

end
