(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Elaborate (S: ELABORATE_STRUCTS): ELABORATE= 
struct

open S

local
   open Ast
in
   structure FctArg = FctArg
   structure Longstrid = Longstrid
   structure Topdec = Topdec

   structure SigConst = SigConst
   structure Sigexp = Sigexp
   structure Strdec = Strdec
   structure Strid = Strid
   structure Strexp = Strexp
end

local
   open CoreML
in
   structure Con = Con
   structure Prim = Prim
   structure Tycon = Tycon
   structure Type = Type
end

structure Env = ElaborateEnv (structure Ast = Ast
			      structure CoreML = CoreML
			      structure TypeEnv = TypeEnv)

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

structure ConstType =
   struct
      datatype t = Bool | Int | Real | String | Word

      val toString =
	 fn Bool => "Bool"
	  | Int => "Int"
	  | Real => "Real"
	  | String => "String"
	  | Word => "Word"
   end

structure ElaborateCore = ElaborateCore (structure Ast = Ast
					 structure ConstType = ConstType
					 structure CoreML = CoreML
					 structure Decs = Decs
					 structure Env = Env)

val allowRebindEquals = ElaborateCore.allowRebindEquals

val info = Trace.info "elaborateStrdec"
val info' = Trace.info "elaborateTopdec"
	  
fun elaborateProgram (program,
		      E: Env.t,
		      lookupConstant) =
   let
      val Ast.Program.T decs = Ast.Program.coalesce program 
      fun elabSigexp s = ElaborateSigexp.elaborateSigexp (s, E)
      fun elabSigexpConstraint (cons: SigConst.t, S: Structure.t,
				nest: string list)
	 : Decs.t * Structure.t =
	 let
	    fun s (sigexp, opaque) =
	       let
		  val prefix =
		     case nest of
			[] => ""
		      | _ => concat (List.fold (nest, [], fn (s, ac) =>
						s :: "." :: ac))
		  val (S, decs) =
		     Env.cut (E, S, elabSigexp sigexp,
			      {isFunctor = false,
			       opaque = opaque,
			       prefix = prefix},
			      Sigexp.region sigexp)
	       in
		  (decs, S)
	       end
	 in
	    case cons of
	       SigConst.None => (Decs.empty, S)
	     | SigConst.Opaque sigexp => s (sigexp, true)
	     | SigConst.Transparent sigexp => s (sigexp, false)
	 end	 
      fun elabStrdec (arg: Strdec.t * string list): Decs.t =
	 Trace.traceInfo' (info,
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
		   (d, {env = E,
			lookupConstant = lookupConstant,
			nest = nest})
	      | Strdec.Local (d, d') => (* rule 58 *)
		   Env.localModule (E,
				    fn () => elabStrdec d,
				    fn d => Decs.append (d, elabStrdec d'))
	      | Strdec.Seq ds => (* rule 60 *)
		   List.fold
		   (ds, Decs.empty, fn (d, decs) =>
		    Decs.append (decs, elabStrdec d))
	      | Strdec.Structure strbinds => (* rules 57, 61 *)
		   List.fold
		   (strbinds, Decs.empty, fn ({name, def, constraint}, decs) =>
		    let
		       val nest = Strid.toString name :: nest
		       val (decs', S) = elabStrexp (def, nest)
		       val (decs'', S) =
			  elabSigexpConstraint (constraint, S, nest)
		       val _ = Env.extendStrid (E, name, S)
		    in
		       Decs.appends [decs, decs', decs'']
		    end)
	  end) arg
      and elabStrexp (e: Strexp.t, nest: string list): Decs.t * Structure.t =
	 let
	    val elabStrexp = fn e => elabStrexp (e, nest)
	 in
	    case Strexp.node e of
	       Strexp.App (fctid, strexp) => (* rules 54, 154 *)
		  let
		     val (decs, S) = elabStrexp strexp
		     val (decs', S) =
			FunctorClosure.apply (Env.lookupFctid (E, fctid),
					      S, nest, Strexp.region strexp)
		  in
		     (Decs.append (decs, decs'), S)
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
		  Env.makeStructure (E, fn () => elabStrdec (d, nest))
	     | Strexp.Var p => (* rule 51 *)
		  (Decs.empty, Env.lookupLongstrid (E, p))
	 end
      fun elabTopdec arg: Decs.t =
	 Trace.traceInfo' (info', Topdec.layout, Decs.layout)
	 (fn (d: Topdec.t) =>
	  case Topdec.node d of
	     Topdec.Strdec d => elabStrdec (d, [])
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
		     val (arg, argSig, body, prefix) =
			case FctArg.node arg of
			   FctArg.Structure (arg, argSig) =>
			      (arg, argSig, body,
			       concat [Strid.toString arg, "."])
			 | FctArg.Spec spec =>
			      let
				 val strid =
				    Strid.fromString ("ZZZNewStridZZZ",
						      Region.bogus)
			      in
				 (strid,
				  Sigexp.spec spec,
				  Strexp.lett
				  (Strdec.openn (Vector.new1
						 (Longstrid.short strid)),
				   body),
				  "")
			      end
		     val argInt = elabSigexp argSig
		     val closure =
			Env.functorClosure
			(E, prefix, argInt,
			 fn (formal, nest) =>
			 Env.scope (E, fn () =>
				    (Env.extendStrid (E, arg, formal)
				     ; elabStrexp (body, nest))))
		  in Env.extendFctid (E, name, closure)
		  end)
		 ; Decs.empty)
		) arg
      val elabTopdec =
	 fn d =>
	 let
	    val res = elabTopdec d
	    val _ = Control.checkForErrors "elaborate"
	 in
	    res
	 end
   in
      List.fold (decs, Decs.empty, fn (d, decs) =>
		 Decs.append (decs, elabTopdec d))
   end

end
