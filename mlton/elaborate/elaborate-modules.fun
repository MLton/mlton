(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor ElaborateModules (S: ELABORATE_MODULES_STRUCTS): ELABORATE_MODULES = 
struct

open S

local
   open Control.Elaborate
in
   val allowPrim = fn () => current allowPrim
end

local
   open Ast
in
   structure FctArg = FctArg
   structure Fctid = Fctid
   structure Longstrid = Longstrid
   structure SigConst = SigConst
   structure Sigexp = Sigexp
   structure Strdec = Strdec
   structure Strexp = Strexp
   structure Strid = Strid
   structure Symbol = Symbol
   structure Topdec = Topdec
end

local
   open Env
in
   structure Decs = Decs
   structure FunctorClosure = FunctorClosure
   structure Structure = Structure
end

structure ElaborateSigexp = ElaborateSigexp (structure Ast = Ast
					     structure Env = Env)

structure ElaborateCore = ElaborateCore (structure Ast = Ast
					 structure ConstType = ConstType
					 structure CoreML = CoreML
					 structure Decs = Decs
					 structure Env = Env)
val lookupConstant = ElaborateCore.lookupConstant

val elabStrdecInfo = Trace.info "elabStrdec"
val elabTopdecInfo = Trace.info "elabTopdec"

fun elaborateTopdec (topdec, {env = E: Env.t}) =
   let
      fun elabSigexp s = ElaborateSigexp.elaborateSigexp (s, {env = E})
      fun elabSigexpConstraint (cons: SigConst.t,
				S: Structure.t option,
				nest: string list)
	 : Decs.t * Structure.t option =
	 let
	    fun s (sigexp, opaque) =
	       let
		  val prefix =
		     case nest of
			[] => ""
		      | _ => concat (List.fold (nest, [], fn (s, ac) =>
						s :: "." :: ac))
	       in
		  case S of
		     NONE => (Decs.empty, NONE)
		   | SOME S => 
			let
			   val (S, decs) =
			      case elabSigexp sigexp of
				 NONE => (S, Decs.empty)
			       | SOME I => 
				    Env.cut (E, S, I,
					     {isFunctor = false,
					      opaque = opaque,
					      prefix = prefix},
					     Sigexp.region sigexp)
			in
			   (decs, SOME S)
			end
	       end
	 in
	    case cons of
	       SigConst.None => (Decs.empty, S)
	     | SigConst.Opaque sigexp => s (sigexp, true)
	     | SigConst.Transparent sigexp => s (sigexp, false)
	 end	 
      fun elabStrdec (arg: Strdec.t * string list): Decs.t =
	 Trace.traceInfo' (elabStrdecInfo,
			   Layout.tuple2 (Strdec.layout,
					  List.layout String.layout),
			   Layout.ignore)
	 (fn (d: Strdec.t, nest: string list) =>
	  let
	     val d = Strdec.coalesce d
	     val elabStrdec = fn d => elabStrdec (d, nest)
	  in
	     case Strdec.node d of
		Strdec.Core d => (* rule 56 *)
		   ElaborateCore.elaborateDec
		   (d, {env = E, nest = nest})
	      | Strdec.Local (d, d') => (* rule 58 *)
		   Env.localModule (E,
				    fn () => elabStrdec d,
				    fn d => Decs.append (d, elabStrdec d'))
	      | Strdec.Seq ds => (* rule 60 *)
		   List.fold
		   (ds, Decs.empty, fn (d, decs) =>
		    Decs.append (decs, elabStrdec d))
	      | Strdec.Structure strbinds => (* rules 57, 61 *)
		   let
		      val strbinds =
			 Vector.map
			 (strbinds, fn {name, def, constraint} =>
			  let
			     val nest = Strid.toString name :: nest
			     val (decs', S) = elabStrexp (def, nest)
			     val (decs'', S) =
				elabSigexpConstraint (constraint, S, nest)
			  in
			     {decs = Decs.append (decs', decs''),
			      name = name,
			      S = S}
			  end)
		      val () =
			 Vector.foreach
			 (strbinds, fn {name, S, ...} =>
			  Option.app (S, fn S => Env.extendStrid (E, name, S)))
		    in
		       Decs.appendsV (Vector.map (strbinds, #decs))
		    end
	  end) arg
      and elabStrexp (e: Strexp.t, nest: string list)
	 : Decs.t * Structure.t option =
	 let
	    val elabStrexp = fn e => elabStrexp (e, nest)
	 in
	    case Strexp.node e of
	       Strexp.App (fctid, strexp) => (* rules 54, 154 *)
		  let
		     val (decs, S) = elabStrexp strexp
		  in
		     case S of
			NONE => (decs, NONE)
		      | SOME S =>
			   case Env.lookupFctid (E, fctid) of
			      NONE => (decs, NONE)
			    | SOME fct  =>
				 let
				    val (S, decs') =
				       Env.cut
				       (E, S,
					FunctorClosure.argInterface fct,
					{isFunctor = true,
					 opaque = false,
					 prefix = ""},
					Strexp.region strexp)
				    val (decs'', S) =
				       FunctorClosure.apply
				       (fct, S, [Fctid.toString fctid])
			   in
			      (Decs.appends [decs, decs', decs''], S)
			   end
		  end
	     | Strexp.Constrained (e, c) => (* rules 52, 53 *)
		  let
		     val (decs, S) = elabStrexp e
		     val (decs', S) = elabSigexpConstraint (c, S, nest)
		  in
		     (Decs.append (decs, decs'), S)
		  end
	     | Strexp.Let (d, e) => (* rule 55 *)
		  Env.scope
		  (E, fn () =>
		   let
		      val decs = elabStrdec (d, nest)
		      val (decs', S) = elabStrexp e
		   in
		      (Decs.append (decs, decs'), S)
		   end)
	     | Strexp.Struct d => (* rule 50 *)
		  let
		     val (decs, S) =
			Env.makeStructure (E, fn () => elabStrdec (d, nest))
		  in
		     (decs, SOME S)
		  end
	     | Strexp.Var p => (* rule 51 *)
		  (Decs.empty, Env.lookupLongstrid (E, p))
	 end
      fun elabFunctor {arg, result, body}: FunctorClosure.t option =
	 let
	    val body = Strexp.constrained (body, result)
	    val (arg, argSig, body, prefix) =
	       case FctArg.node arg of
		  FctArg.Structure (arg, argSig) =>
		     (arg, argSig, body, concat [Strid.toString arg, "."])
		| FctArg.Spec spec =>
		     let
			val strid =
			   Strid.fromSymbol (Symbol.fromString "ZZZNewStridZZZ",
					     Region.bogus)
		     in
			(strid,
			 Sigexp.spec spec,
			 Strexp.lett (Strdec.openn (Vector.new1
						    (Longstrid.short strid)),
				      body),
			 "")
		     end
	 in
	    Option.map (elabSigexp argSig, fn argInt =>
			Env.functorClosure
			(E, prefix, argInt,
			 fn (formal, nest) =>
			 Env.scope (E, fn () =>
				    (Env.extendStrid (E, arg, formal)
				     ; elabStrexp (body, nest)))))
	 end
      fun elabTopdec arg: Decs.t =
	 Trace.traceInfo' (elabTopdecInfo, 
			   Topdec.layout, 
			   Decs.layout)
	 (fn (d: Topdec.t) =>
	  case Topdec.node d of
	     Topdec.BasisDone {ffi} =>
		(if not (allowPrim ())
		    then let open Layout
			 in Control.error (Topdec.region d, str "_basis_done disallowed", empty)
			 end
		    else ()
		 ; let
		      val _ =
			 Option.app
			 (Env.lookupLongstrid (E, ffi), fn S =>
			  (Env.Structure.ffi := SOME S
			   ; Env.Structure.forceUsed S))
		   in
		      Decs.empty
		   end)
	   | Topdec.Signature sigbinds =>
		let
		   val sigbinds =
		      Vector.map
		      (sigbinds, fn (sigid, sigexp) =>
		       (sigid, elabSigexp sigexp))
		   val () =
		      Vector.foreach
		      (sigbinds, fn (sigid, I) =>
		       Option.app (I, fn I => Env.extendSigid (E, sigid, I)))
		in
		   Decs.empty
		end
	   | Topdec.Strdec d => elabStrdec (d, [])
	   | Topdec.Functor funbinds =>
		(* Rules 85, 86. Appendix A, p.58 *)
		let
		   val funbinds =
		      Vector.map
		      (funbinds, fn {arg, body, name, result} =>
		       {closure = elabFunctor {arg = arg,
					       body = body,
					       result = result},
			name = name})
		   val () =
		      Vector.foreach (funbinds, fn {closure, name} =>
				      Option.app
				      (closure, fn closure =>
				       Env.extendFctid (E, name, closure)))
		   (* Check for errors here so that we don't report duplicate
		    * errors when re-elaborating the functor body.
		    *)
		   val () = Control.checkForErrors "elaborate"
		in
		   Decs.empty
		end
		) arg
      val elabTopdec =
	 fn d =>
	 let
	    val res = elabTopdec d
	    val _ = ElaborateCore.reportUndeterminedTypes ()
	 in
	    res
	 end
   in
      elabTopdec topdec
   end
end
