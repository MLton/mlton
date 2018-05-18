(* Copyright (C) 2010,2016 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ElaborateMLBs (S: ELABORATE_MLBS_STRUCTS): ELABORATE_MLBS = 
struct

open S

local
   open Control.Elaborate
in
   val deadCode = fn () => current deadCode
end

structure ElabControl = Control.Elaborate

fun check (c: (bool,bool) ElabControl.t, keyword: string, region) =
   if ElabControl.current c
      then ()
   else
      let
         open Layout
      in
         Control.error 
         (region,
          str (concat (if ElabControl.expert c
                          then [keyword, " disallowed"]
                          else [keyword, " disallowed, compile with -default-ann '", 
                                ElabControl.name c, " true'"])),
          empty)
      end

local
   open Ast
in
   structure Basexp = Basexp
   structure Basdec = Basdec
   structure Longstrid = Longstrid
   structure ModIdBind = ModIdBind
end

local
   open Env
in
   structure Decs = Decs
end

structure ElaboratePrograms = ElaboratePrograms (structure Ast = Ast
                                                 structure CoreML = CoreML
                                                 structure Decs = Decs
                                                 structure Env = Env)

local
   open ElaboratePrograms
in
   structure Decs = Decs
   structure Env = Env
end

fun elaborateMLB (mlb : Basdec.t, {addPrim}) =
   let
      val decs = Buffer.new {dummy = ([], false)}

      val E = Env.empty ()
      fun withDef f =
         ElabControl.withDef 
         (fn () =>
          if ElabControl.default ElabControl.forceUsed
             then Env.forceUsedLocal (E, f)
             else f ())

      val emptySnapshot : (unit -> Env.Basis.t) -> Env.Basis.t = 
         Env.snapshot E
      val emptySnapshot = fn (f: unit -> Env.Basis.t) =>
         emptySnapshot (fn () => withDef f)

      val primBasis =
         emptySnapshot
         (fn () =>
          (#2 o Env.makeBasis)
          (E, fn () =>
           let val primDecs = addPrim E
           in Buffer.add (decs, (primDecs, false))
           end))

      fun elabProg p = ElaboratePrograms.elaborateProgram (p, {env = E})

      val psi : (File.t * Env.Basis.t Promise.t) HashSet.t =
         HashSet.new {hash = String.hash o #1}

      val elabBasexpInfo = Trace.info "ElaborateMLBs.elabBasexp"
      val elabBasdecInfo = Trace.info "ElaborateMLBs.elabBasdec"

      fun elabBasexp (basexp: Basexp.t) : Env.Basis.t option =
         Trace.traceInfo' (elabBasexpInfo,
                           Basexp.layout,
                           Layout.ignore)
         (fn (basexp: Basexp.t) =>
          case Basexp.node basexp of
             Basexp.Bas basdec => 
                let
                   val ((), B) =
                      Env.makeBasis (E, fn () => elabBasdec basdec)
                in
                   SOME B
                end
           | Basexp.Var basid => Env.lookupBasid (E, basid)
           | Basexp.Let (basdec, basexp) => 
                Env.scopeAll
                (E, fn () =>
                 (elabBasdec basdec
                  ; elabBasexp basexp))) basexp
      and elabBasdec (basdec: Basdec.t) : unit =
         Trace.traceInfo' (elabBasdecInfo,
                           Basdec.layout,
                           Layout.ignore)
         (fn (basdec: Basdec.t) =>
          case Basdec.node basdec of
             Basdec.Defs def =>
                let
                   fun doit (lookup, extend, bnds) =
                      Vector.foreach
                      (Vector.map (bnds, fn {lhs, rhs} =>
                                   {lhs = lhs, rhs = lookup (E, rhs)}), 
                       fn {lhs, rhs} =>
                       Option.app (rhs, fn z => extend (E, lhs, z)))
                in
                   case ModIdBind.node def of
                      ModIdBind.Fct bnds => 
                         doit (Env.lookupFctid, Env.extendFctid, bnds)
                    | ModIdBind.Sig bnds => 
                         doit (Env.lookupSigid, Env.extendSigid, bnds)
                    | ModIdBind.Str bnds => 
                         doit (Env.lookupStrid, Env.extendStrid, bnds)
                end
           | Basdec.Basis basbinds => 
                let
                   val basbinds =
                      Vector.map
                      (basbinds, fn {name, def} =>
                       let val B = elabBasexp def
                       in {B = B, name = name}
                       end)
                in
                   Vector.foreach
                   (basbinds, fn {name, B, ...} =>
                    Option.app (B, fn B => Env.extendBasid (E, name, B)))
                end
           | Basdec.Local (basdec1, basdec2) =>
                Env.localAll (E, fn () => 
                              elabBasdec basdec1, fn () => 
                              elabBasdec basdec2)
           | Basdec.Seq basdecs =>
                List.foreach(basdecs, elabBasdec)
           | Basdec.Open basids => 
                Vector.foreach
                (Vector.map (basids, fn basid => 
                             Env.lookupBasid (E, basid)), fn bo => 
                 Option.app (bo, fn b => Env.openBasis (E, b)))
           | Basdec.Prog (_, prog) =>
                let
                   val prog = Promise.force prog
                in
                   Buffer.add (decs, (Decs.toList (elabProg prog), deadCode ()))
                end
           | Basdec.MLB ({fileAbs, ...}, basdec) =>
                let
                   val (_, B) =
                      HashSet.lookupOrInsert
                      (psi, String.hash fileAbs, fn (fileAbs', _) => 
                       String.equals (fileAbs, fileAbs'), fn () =>
                       let
                          val basdec = Promise.force basdec
                          val B =
                             Promise.delay
                             (fn () =>
                              emptySnapshot
                              (fn () =>
                               (#2 o Env.makeBasis) 
                               (E, fn () => elabBasdec basdec)))
                       in
                          (fileAbs, B)
                       end)
                   val B = Promise.force B
                           handle Promise.Force =>
                           (* Basis forms a cycle; 
                            * force the AST to generate error message.
                            *)
                           (ignore (Promise.force basdec)
                            ; #2 (Env.makeBasis (E, fn () => ())))
                in
                   Env.openBasis (E, B)
                end
           | Basdec.Prim => 
                (check (ElabControl.allowPrim, "_prim", Basdec.region basdec)
                 ; Env.openBasis (E, primBasis))
           | Basdec.Ann (ann, reg, basdec) =>
                let
                   open ElabControl
                   fun warn () =
                      if !Control.warnAnn
                         then let open Layout
                              in
                                 Control.warning
                                 (reg, seq [str "unrecognized annotation: ", str ann],
                                  empty)
                              end
                         else ()
                in
                   case parseIdAndArgs ann of
                      ElabControl.Bad =>
                         (warn ()
                          ; elabBasdec basdec)
                    | ElabControl.Good (id, args) =>
                         let
                            val restore = Args.processAnn args
                         in
                            Exn.finally
                            (fn () =>
                             if equalsId (forceUsed, id) andalso enabled forceUsed
                                then Env.forceUsedLocal (E, fn () => elabBasdec basdec)
                             else if equalsId (ffiStr, id)
                                then let
                                        val ffi = valOf (current ffiStr)
                                        val ffi = 
                                           Longstrid.fromSymbols 
                                           (List.map (String.split (ffi, #"."), 
                                                      Longstrid.Symbol.fromString), 
                                            reg)
                                     in
                                        elabBasdec basdec
                                        before
                                        Option.app
                                        (Env.lookupLongstrid (E, ffi), 
                                         fn S => (Env.Structure.ffi := SOME S
                                                  ; Env.Structure.forceUsed S))
                                     end
                             else elabBasdec basdec, 
                             restore)
                         end
                    | ElabControl.Other => elabBasdec basdec
                    | ElabControl.Proxy (alts, {deprecated}) =>
                         let
                            val (ids, args) = List.unzip alts
                            val () =
                               if !Control.warnDeprecated andalso deprecated
                                  then
                                     let open Layout
                                     in
                                        Control.warning
                                        (reg, seq [str "deprecated annotation: ", str ann, str ", use ",
                                                   List.layout (str o ElabControl.Id.name) ids],
                                         empty)
                                     end
                               else ()
                            val restores =
                               List.map (args, Args.processAnn)
                         in
                            Exn.finally
                            (fn () => elabBasdec basdec,
                             fn () => List.foreach (List.rev restores, fn restore => restore ()))
                         end

                end) basdec
      val _ = withDef (fn () => elabBasdec mlb)
   in
      (E, Buffer.toVector decs)
   end

end
