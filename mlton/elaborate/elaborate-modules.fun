(* Copyright (C) 2012,2017 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ElaborateModules (S: ELABORATE_MODULES_STRUCTS): ELABORATE_MODULES = 
struct

open S

local
   open Control.Elaborate
in
   val resolveScope = fn () => current resolveScope
end

local
   open Ast
in
   structure FctArg = FctArg
   structure Fctid = Fctid
   structure Longstrid = Longstrid
   structure SigConst = SigConst
   structure Sigid = Sigid
   structure Sigexp = Sigexp
   structure Strdec = Strdec
   structure Strexp = Strexp
   structure Strid = Strid
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
                                         structure CoreML = CoreML
                                         structure Decs = Decs
                                         structure Env = Env)

val elabStrdecInfo = Trace.info "ElaborateModules.elabStrdec"
val elabStrexpInfo = Trace.info "ElaborateModules.elabStrexp"
val elabTopdecInfo = Trace.info "ElaborateModules.elabTopdec"

fun elaborateTopdec (topdec, {env = E: Env.t}) =
   let
      fun elabSigexp (s, no) =
         ElaborateSigexp.elaborateSigexp
         (s, {env = E, nest = case no of NONE => [] | SOME n => [n]})
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
                              case elabSigexp (sigexp, SOME (Strid.toString Strid.uSig)) of
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
                           Decs.layout)
         (fn (d: Strdec.t, nest: string list) =>
          let
             val d = Strdec.coalesce d
             val elabStrdec = fn d => elabStrdec (d, nest)
             val decs =
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
                 | Strdec.ShowBasis file =>
                      let
                         open Layout
                         val () =
                            File.withOut
                            (file, fn out =>
                             Env.output
                             (E, out,
                              {compact = !Control.showBasisCompact,
                               def = !Control.showBasisDef,
                               flat = !Control.showBasisFlat,
                               onlyCurrent = false,
                               prefixUnset = true}))
                            handle exn =>
                            Control.warning
                            (Strdec.region d,
                             str "Exception raised processing #showBasis",
                             align [seq [str "file: ", File.layout file],
                                    seq [str "exn:  ", Exn.layout exn]])
                      in
                         Decs.empty
                      end
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
             val () =
                case resolveScope () of
                   Control.Elaborate.ResolveScope.Strdec =>
                      (ElaborateCore.reportUnresolvedFlexRecords ()
                       ; ElaborateCore.resolveOverloads ())
                 | _ => ()
          in
             decs
          end) arg
      and elabStrexp (arg: Strexp.t * string list): Decs.t * Structure.t option =
         Trace.traceInfo' (elabStrexpInfo,
                           Layout.tuple2 (Strexp.layout,
                                          List.layout String.layout),
                           Layout.tuple2 (Decs.layout,
                                          Option.layout Structure.layout))
         (fn (e: Strexp.t, nest: string list) =>
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
                                         Region.append
                                         (Fctid.region fctid,
                                          Strexp.region strexp))
                                     val resId = Strid.uRes (Fctid.toString fctid)
                                     val (decs'', S) =
                                        FunctorClosure.apply
                                        (fct, S, [Strid.toString resId])
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
          end) arg
      fun elabFunctor {arg, body, name, result}: FunctorClosure.t option =
         let
            val body = Strexp.constrained (body, result)
            val argId = Strid.uArg (Fctid.toString name)
            val (argSig, argDec) =
               case FctArg.node arg of
                  FctArg.Structure (arg, argSig) =>
                     (argSig,
                      Strdec.structuree
                      {name = arg,
                       def = Strexp.var (Longstrid.short argId),
                       constraint = SigConst.None})
                | FctArg.Spec spec =>
                     (Sigexp.spec spec,
                      Strdec.openn (Vector.new1 (Longstrid.short argId)))
            val body = Strexp.lett (argDec, body)
         in
            Option.map (elabSigexp (argSig, SOME (Strid.toString argId)), fn argInt =>
                        Env.functorClosure
                        (E, name, argInt,
                         fn (formal, nest) =>
                         Env.scope
                         (E, fn () =>
                          (Env.extendStrid (E, argId, formal)
                           ; elabStrexp (body, nest)))))
         end
      fun elabTopdec arg: Decs.t =
         Trace.traceInfo' (elabTopdecInfo, 
                           Topdec.layout, 
                           Decs.layout)
         (fn (d: Topdec.t) =>
          let
             val decs =
                case Topdec.node d of
                   Topdec.Signature sigbinds =>
                      let
                         val sigbinds =
                            Vector.map
                            (sigbinds, fn (sigid, sigexp) =>
                             (sigid, elabSigexp (sigexp, SOME (Sigid.toString sigid))))
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
                                                     name = name,
                                                     result = result},
                              name = name})
                         val () =
                            Vector.foreach (funbinds, fn {closure, name} =>
                                            Option.app
                                            (closure, fn closure =>
                                             Env.extendFctid (E, name, closure)))
                      in
                         Decs.empty
                      end
            val () =
               case resolveScope () of
                  Control.Elaborate.ResolveScope.Topdec =>
                     (ElaborateCore.reportUnresolvedFlexRecords ()
                      ; ElaborateCore.resolveOverloads ())
                | _ => ()
            val _ = ElaborateCore.reportUndeterminedTypes ()
            val _ = ElaborateCore.reportSequenceNonUnit ()
          in
             decs
          end) arg
   in
      elabTopdec topdec
   end

val reportSequenceNonUnit = ElaborateCore.reportSequenceNonUnit
val reportUndeterminedTypes = ElaborateCore.reportUndeterminedTypes
val reportUnresolvedFlexRecords = ElaborateCore.reportUnresolvedFlexRecords
val resolveOverloads = ElaborateCore.resolveOverloads

end
