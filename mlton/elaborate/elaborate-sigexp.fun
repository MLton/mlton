(* Copyright (C) 2010,2012,2015,2017 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
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
   structure Atyvar = Tyvar
   structure WhereEquation = WhereEquation
end

local
   open Env
in
   structure Interface = Interface
   structure StructureTycon =
      struct
         open Tycon
         open TypeEnv.TyconExt
      end
   structure TyvarEnv = TyvarEnv
end
structure StructureEnv = Env
structure Env = StructureEnv.InterfaceEnv

local
   open Interface
in
   structure AdmitsEquality = AdmitsEquality
   structure Cons = Cons
   structure Kind = Kind
   structure FlexibleTycon = FlexibleTycon
   structure Scheme = Scheme
   structure Status = Status
   structure Tycon = Tycon
   structure Type = Type
   structure TypeStr = TypeStr
   structure Tyvar = Tyvar
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

fun elaborateType (ty: Atype.t, E: Env.t): Type.t =
   let
      fun makeBogus (mc, ts) =
         let
            val arity = Vector.length ts
            val (name, region) =
               Option.fold
               (mc, ("t", NONE), fn (c, _) =>
                (Longtycon.toString c,
                 SOME (Longtycon.region c)))
            val c =
               StructureTycon.makeBogus
               {name = name,
                kind = Kind.Arity arity,
                region = region}
         in
            Type.con (Tycon.Rigid c, ts)
         end
      fun loop (ty: Atype.t): Type.t =
         case Atype.node ty of
            Atype.Var a => (* rule 44 *)
               (case TyvarEnv.lookupTyvar a of
                   NONE => makeBogus (NONE, Vector.new0 ())
                 | SOME a => Type.var a)
          | Atype.Con (c, ts) => (* rules 46, 47 *)
               let
                  val ts = Vector.map (ts, loop)
                  fun normal () =
                     case Env.lookupLongtycon (E, c) of
                        NONE => makeBogus (SOME c, ts)
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
                                                fun doit n =
                                                   seq [str "[",
                                                        case n of
                                                           0 => empty
                                                         | 1 => str "_"
                                                         | _ => seq [str "(",
                                                                     (seq o separate)
                                                                     (List.tabulate (n, fn _ => str "_"),
                                                                      ", "),
                                                                     str ")"],
                                                        str "] ",
                                                        Ast.Longtycon.layout c]
                                             in
                                                Control.error
                                                (Atype.region ty,
                                                 seq [str "type constructor applied to incorrect number of type arguments: ",
                                                      Ast.Longtycon.layout c],
                                                 align [seq [str "expects: ", doit n],
                                                        seq [str "but got: ", doit numArgs],
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
                                                     makeBogus
                                                     (NONE,
                                                      Vector.new0 ()))])
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
      ty
   end

val elaborateType =
   Trace.trace ("ElaborateSigexp.elaborateType", Atype.layout o #1, Type.layout)
   elaborateType

fun elaborateScheme (tyvars: Tyvar.t vector, ty: Atype.t, E): Scheme.t =
   let
      val ty = elaborateType (ty, E)
   in
      Scheme.make (tyvars, ty)
   end

fun elaborateTypedescs (typedescs: {tycon: Ast.Tycon.t,
                                    tyvars: Ast.Tyvar.t vector} vector,
                        {equality: bool},
                        E,
                        nest): unit =
   Vector.foreach
   (typedescs, fn {tycon = name, tyvars} =>
    let
       val admitsEquality =
          if equality
             then AdmitsEquality.Sometimes
             else AdmitsEquality.Never
       val kind = Kind.Arity (Vector.length tyvars)
       val prettyDefault =
          concat (List.separate (rev (Ast.Tycon.toString name :: nest), "."))
       val flex = FlexibleTycon.new {admitsEquality = admitsEquality,
                                     hasCons = false,
                                     kind = kind,
                                     prettyDefault = prettyDefault,
                                     region = Ast.Tycon.region name}
       val tycon = Tycon.Flexible flex
    in
       Env.extendTycon (E, name, TypeStr.tycon (tycon, equality))
    end)

fun elabTypBind (typBind: TypBind.t, E, {sequential}) =
   let
      fun mkDef {def, tycon = _, tyvars} =
         TyvarEnv.scope
         (tyvars, fn tyvars =>
          let
             val realization =
                TypeStr.def (elaborateScheme (tyvars, def, E))
             val _ =
                TypeStr.pushSpec (realization, Ast.Type.region def)
          in
             realization
          end)
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

fun elaborateDatBind (datBind: DatBind.t, E, nest): unit =
   let
      val DatBind.T {datatypes, withtypes} = DatBind.node datBind
      (* Build enough of an interface so that that the constructor argument
       * types can be elaborated.
       *)
      val datatypes =
         Vector.map
         (datatypes, fn {cons, tycon = name, tyvars} =>
          let
             val arity = Vector.length tyvars
             val kind = Kind.Arity arity
             val prettyDefault =
                concat (List.separate (rev (Ast.Tycon.toString name :: nest), "."))
             val flex = FlexibleTycon.new {admitsEquality = AdmitsEquality.Sometimes,
                                           hasCons = true,
                                           kind = kind,
                                           prettyDefault = prettyDefault,
                                           region = Ast.Tycon.region name}
             val tycon = Tycon.Flexible flex
             val _ = Env.extendTycon (E, name, TypeStr.tycon (tycon, false))
          in
             {cons = cons,
              flex = flex,
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
         (datatypes, fn {cons, flex, name, tycon, tyvars} =>
          let
             val cons =
                Vector.map
                (cons, fn (name, arg) =>
                 TyvarEnv.scope
                 (tyvars, fn tyvars =>
                  {arg = Option.map (arg, fn t => elaborateType (t, E)),
                   name = name,
                   tyvars = tyvars}))
          in
             {cons = cons,
              flex = flex,
              name = name,
              tycon = tycon}
          end)
      (* Maximize equality *)
      val change = ref false
      fun loop () =
          let
             val _ =
                Vector.foreach
                (datatypes, fn {cons, flex, ...} =>
                 let
                    val isEquality = ref true
                    val () =
                       Vector.foreach
                       (cons, fn {arg, tyvars, ...} =>
                        Option.foreach
                        (arg, fn arg =>
                         let
                            val argScheme =
                               Scheme.make (tyvars, arg)
                         in
                            if Scheme.admitsEquality argScheme
                               then ()
                               else isEquality := false
                         end))
                    datatype z = datatype AdmitsEquality.t
                 in
                    case FlexibleTycon.admitsEquality flex of
                       Always => Error.bug "ElaborateSigexp.elaborateDatBind: Always"
                     | Never => ()
                     | Sometimes =>
                          if !isEquality
                             then ()
                             else (FlexibleTycon.setAdmitsEquality (flex, Never)
                                   ; change := true)
                 end)
          in
             if !change
                then (change := false; loop ())
                else ()
          end
      val () = loop ()
      val () =
         Vector.foreach
         (datatypes, fn {cons, name, tycon, ...} =>
          let
             val cons =
                Vector.map
                (cons, fn {arg, name, tyvars} =>
                 let
                    val res =
                       Type.con (tycon, Vector.map (tyvars, Type.var))
                    val ty =
                       case arg of
                          NONE => res
                        | SOME arg => Type.arrow (arg, res)
                    val scheme =
                       Scheme.make (tyvars, ty)
                    val () =
                       Env.extendCon (E, name, scheme)
                 in
                    {name = name,
                     scheme = scheme}
                 end)
             val () =
                Env.rebindTycon
                (E, name, TypeStr.data (tycon, Cons.fromVector cons, false))
          in
             ()
          end)
   in
      ()
   end

val traceElaborateSigexp =
   Trace.trace2 ("ElaborateSigexp.elaborateSigexp",
                 Sigexp.layout,
                 fn {isTop, nest} => Layout.record [("isTop", Bool.layout isTop),
                                                    ("nest", List.layout Layout.str nest)],
                 Option.layout Interface.layout)
val traceElaborateSpec =
   Trace.trace2 ("ElaborateSigexp.elaborateSpec",
                 Spec.layout,
                 fn {nest} => Layout.record [("nest", List.layout Layout.str nest)],
                 Unit.layout)

(* rule 65 *)
fun elaborateSigexp (sigexp: Sigexp.t, {env = E: StructureEnv.t, nest: string list}): Interface.t option =
   let
      val strE = E
      val E = StructureEnv.makeInterfaceEnv E
      fun elaborateSigexp arg : Interface.t option =
         traceElaborateSigexp
         (fn (sigexp: Sigexp.t, {isTop, nest}) =>
          case Sigexp.node sigexp of
             Sigexp.Spec spec =>
                (* rule 62 *)
                SOME (#1 (Env.makeInterface (E, {isTop = isTop},
                                             fn () => elaborateSpec (spec, {nest = nest}))))
           | Sigexp.Var x =>
                (* rule 63 *)
                Option.map (Env.lookupSigid (E, x), Interface.copy)
           | Sigexp.Where {sigexp, equations} =>
                (* rule 64 *)
                let
                   val time = Interface.Time.tick ()
                in
                   Option.map
                   (elaborateSigexp (sigexp, {isTop = false, nest = nest}), fn I =>
                    let
                       val {layoutPrettyTycon = layoutPrettyEnvTycon,
                            layoutPrettyFlexTycon, ...} =
                          StructureEnv.makeLayoutPrettyTyconAndFlexTycon
                          (strE, E, SOME I, {prefixUnset = true})
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
                                        TyvarEnv.scope
                                        (tyvars, fn tyvars =>
                                         TypeStr.def (elaborateScheme (tyvars, ty, E)))
                                  in
                                     TypeStr.wheree
                                     {layoutPrettyEnvTycon = layoutPrettyEnvTycon,
                                      layoutPrettyFlexTycon = layoutPrettyFlexTycon,
                                      realization = realization,
                                      region = WhereEquation.region eqn,
                                      time = time,
                                      ty = {name = fn () => Longtycon.layout longtycon,
                                            region = Longtycon.region longtycon,
                                            spec = Ast.Tycon.region name,
                                            tyStr = s}}
                                  end))
                    in
                       I
                    end)
                end) arg
      and elaborateSpec arg : unit =
         traceElaborateSpec
         (fn (spec: Spec.t, {nest}) =>
          case Spec.node spec of
             Spec.Datatype rhs =>
                (* rules 71, 72 *)
                (case DatatypeRhs.node rhs of
                    DatatypeRhs.DatBind b => elaborateDatBind (b, E, nest)
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
                elaborateTypedescs (typedescs, {equality = true}, E, nest)
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
                                val t = elaborateType (t, E)
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
                Option.app (elaborateSigexp (sigexp, {isTop = false, nest = nest}), fn I =>
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
                (elaborateSpec (s, {nest = nest})
                 ; elaborateSpec (s', {nest = nest}))
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
                             | _ => (spec, equations)
                      in
                         loop (spec, [equation])
                      end
                   val (I, _) =
                      Env.makeInterface
                      (E, {isTop = false},
                       fn () => elaborateSpec (spec, {nest = nest}))
                   val () = Env.openInterface (E, I, Spec.region spec)
                   val {layoutPrettyTycon = layoutPrettyEnvTycon,
                        layoutPrettyFlexTycon, ...} =
                      StructureEnv.makeLayoutPrettyTyconAndFlexTycon
                      (strE, E, NONE, {prefixUnset = true})
                   val () =
                      List.foreach
                      (equations, fn eqn =>
                       case SharingEquation.node eqn of
                          SharingEquation.Structure ss =>
                             let
                                (* The following implements the "all
                                 * pairs" sharing as specified in
                                 * Appendix A (and described in
                                 * Appendix G.3.3).
                                 *)
                                fun loop Is =
                                   case Is of
                                      [] => ()
                                    | (long1, I1) :: Is =>
                                         (List.foreach
                                          (Is, fn (long2, I2) =>
                                           Interface.share
                                           {layoutPrettyEnvTycon = layoutPrettyEnvTycon,
                                            layoutPrettyFlexTycon = layoutPrettyFlexTycon,
                                            I1 = I1, long1 = long1,
                                            I2 = I2, long2 = long2,
                                            region = SharingEquation.region eqn,
                                            time = time})
                                          ; loop Is)
                                val Is =
                                   List.keepAllMap
                                   (ss, fn s =>
                                    Option.map
                                    (Interface.lookupLongstrid
                                     (I, s, Longstrid.region s, {prefix = []}),
                                     fn I => (s, I)))
                             in
                                loop Is
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
                                           {name = fn () => Longtycon.layout c,
                                            region = Longtycon.region c,
                                            spec = Ast.Tycon.region n,
                                            tyStr = s}
                                        val _ =
                                           TypeStr.share
                                           {layoutPrettyEnvTycon = layoutPrettyEnvTycon,
                                            layoutPrettyFlexTycon = layoutPrettyFlexTycon,
                                            region = SharingEquation.region eqn,
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
                        case elaborateSigexp
                             (sigexp,
                              {isTop = false,
                               nest = (Ast.Strid.toString strid)::nest}) of
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
                elaborateTypedescs (typedescs, {equality = false}, E, nest)
           | Spec.TypeDefs typBind =>
                (* Abbreviation on page 59 combined with rules 77 and 80. *)
                elabTypBind (typBind, E, {sequential = true})
           | Spec.Val xts =>
                (* rules 68, 79 *)
                Vector.foreach
                (xts, fn (x, t) =>
                 Env.extendVid
                 (E, Ast.Vid.fromVar x, Status.Var,
                  let
                     val tyvars =
                        let
                           val tyvars = ref []
                           fun loop t =
                              case Ast.Type.node t of
                                 Atype.Var a =>
                                    if List.contains (!tyvars, a, Atyvar.equals)
                                       then ()
                                       else List.push (tyvars, a)
                               | Atype.Con (_, ts) =>
                                    Vector.foreach (ts, loop)
                               | Atype.Paren t => loop t
                               | Atype.Record r => Record.foreach (r, loop o #2)
                           val () = loop t
                        in
                           Vector.fromListRev (!tyvars)
                        end
                  in
                     TyvarEnv.scope
                     (tyvars, fn tyvars =>
                      elaborateScheme (tyvars, t, E))
                  end))) arg
   in
      elaborateSigexp (sigexp, {isTop = true, nest = nest})
   end

val elaborateSigexp =
   fn (sigexp, {env = E, nest}) =>
   case Sigexp.node sigexp of
      Sigexp.Var x => StructureEnv.lookupSigid (E, x)
    | _ => elaborateSigexp (sigexp, {env = E, nest = nest})

val elaborateSigexp = 
   Trace.trace2 ("ElaborateSigexp.elaborateSigexp",
                 Sigexp.layout,
                 Layout.ignore,
                 Layout.ignore)
   elaborateSigexp

structure Env = StructureEnv

end
