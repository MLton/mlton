(* Copyright (C) 2010,2012,2015,2017 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
   structure SharingEquation = SharingEquation
   structure Longstrid = Longstrid
   structure Longtycon = Longtycon
   structure Record = Record
   structure Sigexp = Sigexp
   structure Sigid = Sigid
   structure SortedRecord = SortedRecord
   structure Spec = Spec
   structure TypBind = TypBind
   structure Tyvar = Tyvar
   structure WhereEquation = WhereEquation
end

structure StructureEnv = Env
structure StructureTycon =
   struct
      open StructureEnv.Tycon
      open StructureEnv.TypeEnv.TyconExt
   end
structure Interface = StructureEnv.Interface
structure Env = StructureEnv.InterfaceEnv

local
   open Interface
in
   structure AdmitsEquality = AdmitsEquality
   structure Cons = Cons
   structure Kind = Kind
   structure Scheme = Scheme
   structure Status = Status
   structure Tycon = Tycon
   structure Type = Type
   structure TypeStr = TypeStr
end

local
   open Control.Elaborate
in
   fun check (c: (bool,bool) t, keyword: string, region) =
      if current c
         then ()
      else
         let
            open Layout
         in
            Control.error
            (region,
             str (concat (if expert c
                             then [keyword, " disallowed"]
                             else [keyword, " disallowed, compile with -default-ann '",
                                   name c, " true'"])),
             empty)
         end
end

fun elaborateType (ty: Atype.t, E: Env.t): Tyvar.t vector * Type.t =
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
                     case Env.lookupLongtycon (E, c) of
                        NONE =>
                           let
                              val kind = Kind.Arity (Vector.length ts)
                              val c =
                                 StructureTycon.make
                                 (concat ["<", Longtycon.toString c ,">"],
                                  AdmitsEquality.Sometimes,
                                  kind,
                                  Longtycon.region c)
                           in
                              Type.con (Tycon.Rigid c, ts)
                           end
                      | SOME s =>
                           let
                              val kind = TypeStr.kind s
                              val numArgs = Vector.length ts
                              val ts =
                                 case kind of
                                    Kind.Arity n =>
                                       let
                                          fun error () =
                                             let
                                                open Layout
                                             in
                                                Control.error
                                                (Atype.region ty,
                                                 seq [str "type constructor applied to incorrect number of type arguments: ",
                                                      Ast.Longtycon.layout c],
                                                 align [seq [str "expects: ", Int.layout n],
                                                        seq [str "but got: ", Int.layout numArgs],
                                                        seq [str "in: ", Atype.layout ty]])
                                             end
                                       in
                                          case Int.compare (n, numArgs) of
                                             LESS =>
                                                (error (); Vector.prefix (ts, n))
                                           | EQUAL => ts
                                           | GREATER =>
                                                (error ()
                                                 ; Vector.concat
                                                   [ts,
                                                    Vector.tabulate
                                                    (n - numArgs, fn _ =>
                                                     let
                                                        val kind = Kind.Arity 0
                                                        val c =
                                                           StructureTycon.make
                                                           ("<t>",
                                                            AdmitsEquality.Sometimes,
                                                            kind,
                                                            Region.bogus)
                                                     in
                                                        Type.con (Tycon.Rigid c,
                                                                  Vector.new0 ())
                                                     end)])
                                       end
                                  | Kind.Nary => ts
                           in
                              TypeStr.apply (s, ts)
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
          | Atype.Paren t => loop t
          | Atype.Record r => (* rules 45, 49 *)
               Type.record
               (SortedRecord.fromVector
                (Vector.map
                 (Record.toVector r,
                  fn (f, (_, t)) => (f, loop t))))
      val ty = loop ty
   in
      (Vector.fromList (!tyvars), ty)
   end

val elaborateType =
   Trace.trace ("ElaborateSigexp.elaborateType", Atype.layout o #1, Type.layout o #2)
   elaborateType

fun elaborateScheme (tyvars: Tyvar.t vector, ty: Atype.t, E): Scheme.t =
   let
      val (tyvars', ty) = elaborateType (ty, E)
      val unbound =
         Vector.keepAll
         (tyvars', fn a =>
          not (Vector.exists (tyvars, fn a' => Tyvar.sameName (a, a'))))
      val ty =
         if Vector.isEmpty unbound then
            ty
         else
            let
               open Layout
               val () =
                  Control.error (Tyvar.region (Vector.first tyvars'),
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
               fun var a =
                  if Vector.exists (unbound, fn a' => Tyvar.equals (a, a')) then
                     let
                        val kind = Kind.Arity 0
                        val c =
                           StructureTycon.make
                           ("<t>",
                            AdmitsEquality.Sometimes,
                            kind,
                            Region.bogus)
                     in
                        Type.con (Tycon.Rigid c, Vector.new0 ())
                     end
                  else
                     Type.var a
            in
               Type.hom (ty, {con = Type.con,
                              record = Type.record,
                              var = var})
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
                                    tyvars: Tyvar.t vector} vector,
                        {equality: bool},
                        E): unit =
   Vector.foreach
   (typedescs, fn {tycon = name, tyvars} =>
    let
       val kind = Kind.Arity (Vector.length tyvars)
       val tycon = Tycon.make {hasCons = false, kind = kind}
       val _ =
          Tycon.admitsEquality tycon
          := (if equality
                 then AdmitsEquality.Sometimes
              else AdmitsEquality.Never)
    in
       Env.extendTycon (E, name, TypeStr.tycon tycon)
    end)

fun elabTypBind (typBind: TypBind.t, E, {sequential}) =
   let
      fun mkDef {def, tycon = _, tyvars} =
         let
            val realization =
               TypeStr.def (elaborateScheme (tyvars, def, E))
            val _ =
               TypeStr.pushSpec (realization, Ast.Type.region def)
         in
            realization
         end
      val TypBind.T bs = TypBind.node typBind
   in
      if sequential
         then Vector.foreach
              (bs, fn b as {tycon, ...} =>
               Env.extendTycon (E, tycon, mkDef b))
         else Vector.foreach2
              (bs, Vector.map (bs, mkDef), fn ({tycon, ...}, str) =>
               Env.extendTycon (E, tycon, str))
   end

fun elaborateDatBind (datBind: DatBind.t, E): unit =
   let
      val DatBind.T {datatypes, withtypes} = DatBind.node datBind
      (* Build enough of an interface so that that the constructor argument
       * types can be elaborated.
       *)
      val datatypes =
         Vector.map
         (datatypes, fn {cons, tycon = name, tyvars} =>
          let
             val kind = Kind.Arity (Vector.length tyvars)
             val tycon = Tycon.make {hasCons = true, kind = kind}
             val _ =
                Env.extendTycon (E, name, TypeStr.tycon tycon)
          in
             {cons = cons,
              name = name,
              tycon = tycon,
              tyvars = tyvars}
          end)
      val _ = if TypBind.isEmpty withtypes
                 then ()
                 else check (Control.Elaborate.allowSigWithtype,
                             "withtype in signatures",
                             TypBind.region withtypes)
      (* To match semantics of withtype in Core,
       * type binds are elaborated simultaneously.
       *)
      val _ = elabTypBind (withtypes, E, {sequential = false})
      val datatypes =
         Vector.map
         (datatypes, fn {cons, name, tycon, tyvars, ...} =>
          let
             val resultType: Atype.t =
                Atype.con (name, Vector.map (tyvars, Atype.var))
             val (consSchemes, consArgs) =
                Vector.unzip
                (Vector.map
                 (cons, fn (name, arg) =>
                  let
                     val (makeArg, ty) =
                        case arg of
                           NONE => (fn _ => NONE, resultType)
                         | SOME t =>
                           (fn s => SOME (#1 (Type.deArrow (Scheme.ty s))),
                            Atype.arrow (t, resultType))
                     val scheme = elaborateScheme (tyvars, ty, E)
                  in
                     ({name = name,
                       scheme = scheme},
                      {con = name,
                       arg = makeArg scheme})
                  end))
          in
             {consArgs = consArgs,
              consSchemes = consSchemes,
              name = name,
              tycon = tycon,
              tyvars = tyvars}
          end)
      val _ = Env.allowDuplicates := true
      val _ =
         Vector.foreach
         (datatypes, fn {consSchemes, name, tycon, ...} =>
          let
             val _ =
                Vector.foreach
                (consSchemes, fn {name, scheme} =>
                 Env.extendCon (E, name, scheme))
             val _ =
                Env.extendTycon
                (E, name, TypeStr.data (tycon, Cons.fromVector consSchemes, false))
          in
             ()
          end)
      val _ = Env.allowDuplicates := false
      (* Maximize equality *)
      val change = ref false
      fun loop () =
         let
            val _ =
               Vector.foreach
               (datatypes, fn {consArgs, tycon, tyvars, ...} =>
                let
                   val r = Tycon.admitsEquality tycon
                   datatype z = datatype AdmitsEquality.t
                in
                   case !r of
                      Always => Error.bug "ElaborateSigexp.elaborateDatBind: Always"
                    | Never => ()
                    | Sometimes =>
                         if Vector.forall
                            (consArgs, fn {arg, ...} =>
                             case arg of
                                NONE => true
                              | SOME ty =>
                                   Scheme.admitsEquality
                                   (Scheme.make (tyvars, ty)))
                            then ()
                         else (r := Never; change := true)
                end)
         in
            if !change
               then (change := false; loop ())
            else ()
         end
      val _ = loop ()
   in
      ()
   end

val traceElaborateSigexp =
   Trace.trace2 ("ElaborateSigexp.elaborateSigexp",
                 Sigexp.layout,
                 fn {isTop} => Layout.record [("isTop", Bool.layout isTop)],
                 Option.layout Interface.layout)

val info' = Trace.info "ElaborateSigexp.elaborateSpec"

(* rule 65 *)
fun elaborateSigexp (sigexp: Sigexp.t, {env = E: StructureEnv.t}): Interface.t option =
   let
      val E = StructureEnv.makeInterfaceEnv E
      fun elaborateSigexp arg : Interface.t option =
         traceElaborateSigexp
         (fn (sigexp: Sigexp.t, {isTop}) =>
          case Sigexp.node sigexp of
             Sigexp.Spec spec =>
                (* rule 62 *)
                SOME (#1 (Env.makeInterface (E, {isTop = isTop},
                                             fn () => elaborateSpec spec)))
           | Sigexp.Var x =>
                (* rule 63 *)
                Option.map (Env.lookupSigid (E, x), Interface.copy)
           | Sigexp.Where {sigexp, equations} =>
                (* rule 64 *)
                let
                   val time = Interface.Time.tick ()
                in
                   Option.map
                   (elaborateSigexp (sigexp, {isTop = false}), fn I =>
                    let
                       val _ = 
                          Vector.foreach
                          (equations, fn eqn =>
                           case WhereEquation.node eqn of
                              WhereEquation.Type {longtycon, ty, tyvars} =>
                                 Option.app
                                 (Interface.lookupLongtycon
                                  (I, longtycon, Longtycon.region longtycon,
                                   {prefix = []}),
                                  fn (name, s) =>
                                  let
                                     val realization =
                                        TypeStr.def (elaborateScheme (tyvars, ty, E))
                                  in
                                     TypeStr.wheree
                                     {realization = realization,
                                      region = WhereEquation.region eqn,
                                      time = time,
                                      ty = {lay = fn () => Longtycon.layout longtycon,
                                            region = Longtycon.region longtycon,
                                            spec = Ast.Tycon.region name,
                                            tyStr = s}}
                                  end))
                    in
                       I
                    end)
                end) arg
      and elaborateSpec arg : unit =
         Trace.traceInfo' (info', Spec.layout, Layout.ignore)
         (fn spec: Spec.t =>
          case Spec.node spec of
             Spec.Datatype rhs =>
                (* rules 71, 72 *)
                (case DatatypeRhs.node rhs of
                    DatatypeRhs.DatBind b => elaborateDatBind (b, E)
                  | DatatypeRhs.Repl {lhs, rhs} =>
                       Option.app
                       (Env.lookupLongtycon (E, rhs), fn s =>
                        let
                           val _ = TypeStr.pushSpec (s, Longtycon.region rhs)
                           val _ = Env.extendTycon (E, lhs, TypeStr.repl s)
                           val _ =
                              Vector.foreach
                              (Cons.dest (TypeStr.cons s), fn {name, scheme} =>
                               Env.extendCon (E, name, scheme))
                        in
                           ()
                        end))
           | Spec.Empty =>
                (* rule 76 *)
                ()
           | Spec.Eqtype typedescs =>
                (* rule 70 *)
                elaborateTypedescs (typedescs, {equality = true}, E)
           | Spec.Exception cons =>
                (* rule 73 *)
                Vector.foreach
                (cons, fn (name: Ast.Con.t, arg: Ast.Type.t option) =>
                 let
                    val ty =
                       case arg of
                          NONE => Type.exn
                        | SOME t =>
                             let
                                val t = Scheme.ty (elaborateScheme
                                                   (Vector.new0 (), t, E))
                             in
                                Type.arrow (t, Type.exn)
                             end
                    val scheme = Scheme.make (Vector.new0 (), ty)
                    val _ = Env.extendExn (E, name, scheme)
                 in
                    ()
                 end)
           | Spec.IncludeSigexp sigexp =>
                (* rule 75 *)
                Option.app (elaborateSigexp (sigexp, {isTop = false}), fn I =>
                            Env.openInterface (E, I, Sigexp.region sigexp))
           | Spec.IncludeSigids sigids =>
                (* Appendix A, p.59 *)
                Vector.foreach (sigids, fn x =>
                                Option.app
                                (Env.lookupSigid (E, x), fn I =>
                                 Env.openInterface
                                 (E, Interface.copy I, Sigid.region x)))
           | Spec.Seq (s, s') =>
                (* rule 77 *)
                (elaborateSpec s; elaborateSpec s')
           | Spec.Sharing {equation, spec} =>
                (* rule 78 and section G.3.3 *)
                let
                   val time = Interface.Time.tick ()
                   (* Reifying the interface of spec is expensive,
                    * so collect all `sharing` equations that
                    * constrain the same spec.
                    *)
                   val (spec, equations) =
                      let
                         fun loop (spec, equations) =
                            case Spec.node spec of
                               Spec.Sharing {equation, spec} =>
                                  loop (spec, equation::equations)
                             | _ => (spec, List.rev equations)
                      in
                         loop (spec, [equation])
                      end
                   val (I, _) =
                      Env.makeInterface
                      (E, {isTop = false},
                       fn () => elaborateSpec spec)
                   val () = Env.openInterface (E, I, Spec.region spec)
                   val () =
                      List.foreach
                      (equations, fn eqn =>
                       case SharingEquation.node eqn of
                          SharingEquation.Structure ss =>
                             let
                                (* The following implements the "all pairs"
                                 * sharing as specified in G.3.3.
                                 *)
                                fun loop Is =
                                   case Is of
                                      [] => ()
                                    | (s, I) :: Is =>
                                         List.foreach 
                                         (Is, fn (s', I') =>
                                          Interface.share
                                          (I, s, I', s',
                                           time,
                                           SharingEquation.region eqn))
                             in
                                loop (List.fold
                                      (ss, [], fn (s, ac) =>
                                       case Interface.lookupLongstrid (I, s, Longstrid.region s, {prefix = []}) of
                                          NONE => ac
                                        | SOME I => (s, I) :: ac))
                             end
                        | SharingEquation.Type cs =>
                             ignore
                             (List.fold
                              (cs, NONE, fn (c', so) =>
                               case (so, Interface.lookupLongtycon (I, c', Longtycon.region c', {prefix = []})) of
                                  (NONE, NONE) => NONE
                                | (SOME _, NONE) => so
                                | (NONE, SOME (n', s')) => SOME (c', n', s')
                                | (SOME (c, n, s), SOME (n', s')) =>
                                     let
                                        fun mkTy (c, n, s) =
                                           {lay = fn () => Longtycon.layout c,
                                            region = Longtycon.region c,
                                            spec = Ast.Tycon.region n,
                                            tyStr = s}
                                        val _ =
                                           TypeStr.share
                                           {region = SharingEquation.region eqn,
                                            time = time,
                                            ty1 = mkTy (c, n, s),
                                            ty2 = mkTy (c', n', s')}
                                     in
                                        SOME (c', n', s')
                                     end)))
                in
                   ()
                end
           | Spec.Structure ss =>
                (* rules 74, 84 *)
                let
                   val ss =
                      Vector.map
                      (ss, fn (strid, sigexp) =>
                       (strid,
                        case elaborateSigexp (sigexp, {isTop = false}) of
                           NONE => Interface.empty
                         | SOME I => I))
                in
                   Vector.foreach
                   (ss, fn (strid, I) =>
                    Env.extendStrid
                    (E, strid, I))
                end
           | Spec.Type typedescs =>
                (* rule 69 *)
                elaborateTypedescs (typedescs, {equality = false}, E)
           | Spec.TypeDefs typBind =>
                (* Abbreviation on page 59 combined with rules 77 and 80. *)
                elabTypBind (typBind, E, {sequential = true})
           | Spec.Val xts =>
                (* rules 68, 79 *)
                Vector.foreach
                (xts, fn (x, t) =>
                 Env.extendVid
                 (E, Ast.Vid.fromVar x, Status.Var,
                  Scheme.make (elaborateType (t, E))))
                ) arg
   in
      elaborateSigexp (sigexp, {isTop = true})
   end

val elaborateSigexp =
   fn (sigexp, {env = E}) =>
   case Sigexp.node sigexp of
      Sigexp.Var x => StructureEnv.lookupSigid (E, x)
    | _ => elaborateSigexp (sigexp, {env = E})

val elaborateSigexp = 
   Trace.trace2 ("ElaborateSigexp.elaborateSigexp",
                 Sigexp.layout,
                 Layout.ignore,
                 Layout.ignore)
   elaborateSigexp

structure Env = StructureEnv

end
