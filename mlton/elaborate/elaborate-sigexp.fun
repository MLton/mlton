(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
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
   structure Equation = Equation
   structure Longtycon = Longtycon
   structure Sigexp = Sigexp
   structure Sigid = Sigid
   structure SortedRecord = SortedRecord
   structure Spec = Spec
   structure TypBind = TypBind
   structure Tyvar = Tyvar
end

local
   open Env
in
   structure Interface = Interface
end

structure StructureEnv = Env
structure Env = Env.InterfaceEnv

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
                        NONE => Type.bogus
                      | SOME s => 
                           let
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
                                    val () = 
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
         if 0 = Vector.length unbound then
            ty
         else
            let
               open Layout
               val () =
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
               fun var a =
                  if Vector.exists (unbound, fn a' => Tyvar.equals (a, a')) then
                     Type.bogus
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
       Env.extendTycon (E, name, TypeStr.tycon (tycon, kind))
    end)

fun elaborateDatBind (datBind: DatBind.t, E): unit =
   let
      val DatBind.T {datatypes, ...} = DatBind.node datBind
      val change = ref false
      (* Build enough of an interface so that that the constructor argument
       * types can be elaborated.
       *)
      val tycons =
         Vector.map
         (datatypes, fn {tycon = name, tyvars, ...} =>
          let
             val kind = Kind.Arity (Vector.length tyvars)
             val tycon = Tycon.make {hasCons = true, kind = kind}
             val _ =
                Env.extendTycon (E, name, TypeStr.data (tycon, kind, Cons.empty))
          in
             tycon
          end)
      fun elabAll (): unit =
         Vector.foreach2
         (tycons, datatypes, fn (tycon, {cons, tycon = astTycon, tyvars, ...}) =>
          let
             val resultType: Atype.t =
                Atype.con (astTycon, Vector.map (tyvars, Atype.var))
             val (cons, conArgs) =
                Vector.unzip
                (Vector.map
                 (cons, fn (name, arg) =>
                  let
                     val (makeArg, ty) =
                        case arg of
                           NONE => (fn _ => NONE, resultType)
                         | SOME t =>
                              (fn s =>
                               SOME (#1 (Type.deArrow (Scheme.ty s))),
                               Atype.arrow (t, resultType))
                     val scheme = elaborateScheme (tyvars, ty, E)
                  in
                     ({name = name,
                       scheme = scheme},
                      makeArg scheme)
                  end))
             val _ =
                let
                   val r = Tycon.admitsEquality tycon
                   datatype z = datatype AdmitsEquality.t
                in
                   case !r of
                      Always => Error.bug "ElaborateSigexp.elaborateDatBind: Always"
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
             val _ = Vector.foreach (cons, fn {name, scheme} =>
                                     Env.extendCon (E, name, scheme))
             val _ = Env.allowDuplicates := true
             val _ =
                Env.extendTycon
                (E, astTycon,
                 TypeStr.data (tycon, Kind.Arity (Vector.length tyvars),
                               Cons.T cons))
          in
             ()
          end) 
      (* We don't want to re-elaborate the datatypes if there has been a type
       * error, because that will cause duplicate error messages.
       *)
     val numErrors = !Control.numErrors
      (* Maximize equality. *)
      fun loop (): unit =
         let
            val _ = elabAll ()
         in
            if !change andalso numErrors = !Control.numErrors
               then (change := false; loop ())
            else ()
         end
      val _ = loop ()
      val _ = Env.allowDuplicates := false
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
      val _ = Interface.renameTycons := (fn () => StructureEnv.setTyconNames E)
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
           | Sigexp.Where (sigexp, wheres) =>
                (* rule 64 *)
                let
                   val time = Interface.Time.tick ()
                in
                   Option.map
                   (elaborateSigexp (sigexp, {isTop = false}), fn I =>
                    let
                       val _ = 
                          Vector.foreach
                          (wheres, fn {longtycon, ty, tyvars} =>
                           Option.app
                           (Interface.lookupLongtycon
                            (I, longtycon, Longtycon.region longtycon,
                             {prefix = []}),
                            fn s =>
                            TypeStr.wheree
                            (s, Longtycon.region longtycon,
                             fn () => Longtycon.layout longtycon,
                             time,
                             TypeStr.def (elaborateScheme (tyvars, ty, E),
                                          Kind.Arity (Vector.length tyvars)))))
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
                           val _ = Env.extendTycon (E, lhs, s)
                           val Cons.T v = TypeStr.cons s
                           val _ =
                              Vector.foreach
                              (v, fn {name, scheme} =>
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
           | Spec.Sharing {equations, spec} =>
                (* rule 78 and section G.3.3 *)
                let
                   val time = Interface.Time.tick ()
                   val () = elaborateSpec spec
                   val () =
                      Vector.foreach
                      (equations, fn eqn =>
                       case Equation.node eqn of
                          Equation.Structure ss =>
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
                                          Interface.share (I, s, I', s', time))
                             in
                                loop (List.fold
                                      (ss, [], fn (s, ac) =>
                                       case Env.lookupLongstrid (E, s) of
                                          NONE => ac
                                        | SOME I => (s, I) :: ac))
                             end
                        | Equation.Type cs =>
                             ignore
                             (List.fold
                              (cs, NONE, fn (c', so) =>
                               case (so, Env.lookupLongtycon (E, c')) of
                                  (NONE, NONE) => NONE
                                | (SOME _, NONE) => so
                                | (NONE, SOME s') => SOME (c', s')
                                | (SOME (c, s), SOME s') =>
                                     let
                                        fun doit (c, s) =
                                           (s, Longtycon.region c,
                                            fn () => Longtycon.layout c)
                                        val _ =
                                           TypeStr.share (doit (c, s),
                                                          doit (c', s'),
                                                          time)
                                     in
                                        SOME (c', s')
                                     end)))
                in
                   ()
                end
           | Spec.Structure ss =>
                (* rules 74, 84 *)
                Vector.foreach
                (ss, fn (strid, sigexp) =>
                 Env.extendStrid
                 (E, strid,
                  case elaborateSigexp (sigexp, {isTop = false}) of
                     NONE => Interface.empty
                   | SOME I => I))
           | Spec.Type typedescs =>
                (* rule 69 *)
                elaborateTypedescs (typedescs, {equality = false}, E)
           | Spec.TypeDefs typBind =>
                (* Abbreviation on page 59 combined with rules 77 and 80. *)
                let
                   val TypBind.T ds = TypBind.node typBind
                in
                   Vector.foreach
                   (ds, fn {def, tycon, tyvars} =>
                    Env.extendTycon
                    (E, tycon,
                     TypeStr.def (elaborateScheme (tyvars, def, E),
                                  Kind.Arity (Vector.length tyvars))))
                end
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
