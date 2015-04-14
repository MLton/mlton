(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Scope (S: SCOPE_STRUCTS): SCOPE =
struct

open S
open Ast

structure Tyvars = UnorderedSet (UseName (Tyvar))
structure Env =
   struct
      structure Env = MonoEnv (structure Domain = UseName (Tyvar)
                               structure Range = Tyvar)
      open Env

      (* rename (env, tyvars) extends env by mapping each tyvar to
       * a new tyvar (with the same equality property).  It returns
       * the extended environment and the list of new tyvars.
       *)
      fun rename (env: t, tyvars: Tyvar.t vector): t * Tyvar.t vector =
         let
            val (tyvars, env) =
               Vector.mapAndFold
               (tyvars, env, fn (a, env) =>
                let
                   val a' = Tyvar.newLike a
                in
                   (a', extend (env, a, a'))
                end)
         in
            (env, tyvars)
         end
   end

fun ('down, 'up)
   processDec (d: Dec.t,
               {(* bindType is used at datatype and type declarations. *)
                bindType: ('down * Tyvar.t vector
                           -> 'down * Tyvar.t vector * ('up -> 'up)),
                (* bindFunVal is used at fun, overload, and val declarations. *)
                bindFunVal: ('down * Tyvar.t vector
                             -> ('down * ('up -> Tyvar.t vector * 'up))),
                combineUp: 'up * 'up -> 'up,
                initDown: 'down,
                initUp: 'up,
                tyvar: Tyvar.t * 'down -> Tyvar.t * 'up
                }): Dec.t * 'up =
   let
      fun loops (xs: 'a vector, loopX: 'a -> 'a * 'up): 'a vector * 'up =
         Vector.mapAndFold (xs, initUp, fn (x, u) =>
                            let
                               val (x, u') = loopX x
                            in
                               (x, combineUp (u, u'))
                            end)
      fun loopTy (t: Type.t, d: 'down): Type.t * 'up =
         let
            fun loop (t: Type.t): Type.t * 'up =
               let
                  datatype z = datatype Type.node
                  val (n, u) =
                     case Type.node t of
                        Con (c, ts) =>
                           let
                              val (ts, u) = loops (ts, loop)
                           in
                              (Con (c, ts), u)
                           end
                      | Record r =>
                           let
                              val (r, u) = SortedRecord.change (r, fn ts =>
                                                                loops (ts, loop))
                           in
                              (Record r, u)
                           end
                      | Var a =>
                           let
                              val (a, u) = tyvar (a, d)
                           in
                              (Var a, u)
                           end
               in
                  (Type.makeRegion (n, Type.region t), u)
               end
         in
            loop t
         end
      fun loopTyOpt (to: Type.t option, d: 'down): Type.t option * 'up =
         case to of
            NONE => (NONE, initUp)
          | SOME t =>
               let
                  val (t, u) = loopTy (t, d)
               in
                  (SOME t, u)
               end
      fun loopTypBind (tb: TypBind.t, d: 'down): TypBind.t * 'up =
         let
            val TypBind.T tbs = TypBind.node tb
            val (tbs, u) =
               loops (tbs, fn {def, tycon, tyvars} =>
                      let
                         val (d, tyvars, finish) = bindType (d, tyvars)
                         val (def, u) = loopTy (def, d)
                      in
                         ({def = def,
                           tycon = tycon,
                           tyvars = tyvars},
                          finish u)
                      end)
         in
            (TypBind.makeRegion (TypBind.T tbs, TypBind.region tb),
             u)
         end
      fun loopDatBind (db: DatBind.t, d: 'down): DatBind.t * 'up =
         let
            val DatBind.T {datatypes, withtypes} = DatBind.node db
            val (datatypes, u) =
               loops
               (datatypes, fn {cons, tycon, tyvars} =>
                let
                   val (d, tyvars, up) = bindType (d, tyvars)
                   val (cons, u) =
                      loops (cons, fn (con, arg) =>
                             let
                                val (arg, u) = loopTyOpt (arg, d)
                             in
                                ((con, arg), u)
                             end)
                in
                   ({cons = cons, tycon = tycon, tyvars = tyvars}, up u)
                end)
            val (withtypes, u') = loopTypBind (withtypes, d)
         in
            (DatBind.makeRegion (DatBind.T {datatypes = datatypes,
                                            withtypes = withtypes},
                                 DatBind.region db),
             combineUp (u, u'))
         end
      fun loopPat (p: Pat.t, d: 'down): Pat.t * 'up =
         let
            fun loop (p: Pat.t): Pat.t * 'up =
               let
                  fun doit n = Pat.makeRegion (n, Pat.region p)
                  fun do1 ((a, u), f) = (doit (f a), u)
                  fun do2 ((a1, u1), (a2, u2), f) =
                     (doit (f (a1, a2)), combineUp (u1, u2))
                  datatype z = datatype Pat.node
               in
                  case Pat.node p of
                     App (c, p) => do1 (loop p, fn p => App (c, p))
                   | Const _ => (p, initUp)
                   | Constraint (p, t) =>
                        do2 (loop p, loopTy (t, d), Constraint)
                   | FlatApp ps => do1 (loops (ps, loop), FlatApp)
                   | Layered {constraint, fixop, pat, var} =>
                        do2 (loopTyOpt (constraint, d), loop pat,
                             fn (constraint, pat) =>
                             Layered {constraint = constraint,
                                      fixop = fixop,
                                      pat = pat,
                                      var = var})
                   | List ps => do1 (loops (ps, loop), List)
                   | Or ps => do1 (loops (ps, loop), Or)
                   | Record {flexible, items} =>
                        let
                           val (items, u) =
                              Vector.mapAndFold
                              (items, initUp, fn ((f, i), u) =>
                               let
                                  datatype z = datatype Pat.Item.t
                                  val (i, u') =
                                     case i of
                                        Field p =>
                                           let
                                              val (p, u) = loop p
                                           in
                                              (Field p, u)
                                           end
                                      | Vid (v, to, po) =>
                                           let
                                              val (to, u) = loopTyOpt (to, d)
                                              val (po, u') = loopOpt po
                                           in
                                              (Vid (v, to, po),
                                               combineUp (u, u'))
                                           end
                               in
                                  ((f, i), combineUp (u, u'))
                               end)
                        in
                           (doit (Record {flexible = flexible,
                                          items = items}),
                            u)
                        end
                   | Tuple ps => do1 (loops (ps, loop), Tuple)
                   | Var _ => (p, initUp)
                   | Wild => (p, initUp)

               end
            and loopOpt opt =
               case opt of
                  NONE =>
                     (NONE, initUp)
                | SOME p =>
                     let
                        val (p, u) = loop p
                     in
                        (SOME p, u)
                     end
         in
            loop p
         end
      fun loopPrimKind (kind: PrimKind.t, d: 'down): PrimKind.t * 'up =
         let
            datatype z = datatype PrimKind.t
            fun do1 ((a, u), f) = (f a, u)
         in
            case kind of
               Address {attributes, name, ty} =>
                  do1 (loopTy (ty, d), fn ty =>
                       Address {attributes = attributes, name = name, ty = ty})
             | BuildConst {name, ty} =>
                  do1 (loopTy (ty, d), fn ty =>
                       BuildConst {name = name, ty = ty})
             | CommandLineConst {name, ty, value} =>
                  do1 (loopTy (ty, d), fn ty =>
                       CommandLineConst {name = name, ty = ty, value = value})
             | Const {name, ty} =>
                  do1 (loopTy (ty, d), fn ty =>
                       Const {name = name, ty = ty})
             | Export {attributes, name, ty} =>
                  do1 (loopTy (ty, d), fn ty =>
                       Export {attributes = attributes, name = name, ty = ty})
             | IImport {attributes, ty} =>
                  do1 (loopTy (ty, d), fn ty =>
                       IImport {attributes = attributes, ty = ty})
             | Import {attributes, name, ty} =>
                  do1 (loopTy (ty, d), fn ty =>
                       Import {attributes = attributes, name = name, ty = ty})
             | ISymbol {ty} =>
                  do1 (loopTy (ty, d), fn ty =>
                       ISymbol {ty = ty})
             | Prim {name, ty} =>
                  do1 (loopTy (ty, d), fn ty =>
                       Prim {name = name, ty = ty})
             | Symbol {attributes, name, ty} =>
                  do1 (loopTy (ty, d), fn ty =>
                       Symbol {attributes = attributes, name = name, ty = ty})
         end
      fun loopDec (d: Dec.t, down: 'down): Dec.t * 'up =
         let
            fun doit n = Dec.makeRegion (n, Dec.region d)
            fun do1 ((a, u), f) = (doit (f a), u)
            fun do2 ((a1, u1), (a2, u2), f) =
               (doit (f (a1, a2)), combineUp (u1, u2))
            fun doVec (ds: Dec.t vector, f: Dec.t vector -> Dec.node)
               : Dec.t * 'up =
               let
                  val (ds, u) = loops (ds, fn d => loopDec (d, down))
               in
                  (doit (f ds), u)
               end
            fun empty () = (d, initUp)
            datatype z = datatype Dec.node
         in
            case Dec.node d of
               Abstype {body, datBind} =>
                  let
                     val (body, u) = loopDec (body, down)
                     val (db, u') = loopDatBind (datBind, down)
                  in
                     (doit (Abstype {body = body, datBind = db}),
                      combineUp (u, u'))
                  end
             | Datatype rhs =>
                  let
                     datatype z = datatype DatatypeRhs.node
                     val (rhs, u) =
                        case DatatypeRhs.node rhs of
                           DatBind db =>
                              let
                                 val (db, u) = loopDatBind (db, down)
                              in
                                 (DatatypeRhs.makeRegion
                                  (DatBind db, DatatypeRhs.region rhs),
                                  u)
                              end
                         | Repl _ => (rhs, initUp)
                  in
                     (doit (Datatype rhs), u)
                  end
             | DoDec _ => empty ()
             | Exception ebs =>
                  let
                     val (ebs, u) =
                        loops (ebs, fn (c, rhs) =>
                               let
                                  datatype z = datatype EbRhs.node
                                  val (rhs, u) =
                                     case EbRhs.node rhs of
                                        Def _ => (rhs, initUp)
                                      | Gen to =>
                                           let
                                              val (to, u) = loopTyOpt (to, down)
                                           in
                                              (EbRhs.makeRegion
                                               (Gen to, EbRhs.region rhs),
                                               u)
                                           end
                               in
                                  ((c, rhs), u)
                               end)
                  in
                     (doit (Exception ebs), u)
                  end
             | Fix _ => (d, initUp)
             | Fun (tyvars, decs) =>
                  let
                     val (down, finish) = bindFunVal (down, tyvars)
                     val (decs, u) =
                        loops (decs, fn clauses =>
                               let
                                  val (clauses, u) =
                                     loops
                                     (clauses, fn {body, pats, resultType} =>
                                      let
                                         val (body, u) = loopExp (body, down)
                                         val (pats, u') =
                                            loops (pats, fn p =>
                                                   loopPat (p, down))
                                         val (resultType, u'') =
                                            loopTyOpt (resultType, down)
                                      in
                                         ({body = body,
                                           pats = pats,
                                           resultType = resultType},
                                          combineUp (u, combineUp (u', u'')))
                                      end)
                                 in
                                    (clauses, u)
                                 end)
                     val (tyvars, u) = finish u
                  in
                     (doit (Fun (tyvars, decs)), u)
                  end
             | Local (d, d') =>
                  do2 (loopDec (d, down), loopDec (d', down), Local)
             | Open _ => empty ()
             | Overload (i, x, tyvars, ty, ys) =>
                  let
                     val (down, finish) = bindFunVal (down, tyvars)
                     val (ty, up) = loopTy (ty, down)
                     val (tyvars, up) = finish up
                  in
                     (doit (Overload (i, x, tyvars, ty, ys)), up)
                  end
             | SeqDec ds => doVec (ds, SeqDec)
             | Type tb => do1 (loopTypBind (tb, down), Type)
             | Val {rvbs, tyvars, vbs} =>
                  let
                     val (down, finish) = bindFunVal (down, tyvars)
                     val (rvbs, u) =
                        loops (rvbs, fn {match, pat} =>
                               let
                                  val (match, u) = loopMatch (match, down)
                                  val (pat, u') = loopPat (pat, down)
                               in
                                  ({match = match,
                                    pat = pat},
                                   combineUp (u, u'))
                               end)
                     val (vbs, u') =
                        loops (vbs, fn {exp, pat} =>
                               let
                                  val (exp, u) = loopExp (exp, down)
                                  val (pat, u') = loopPat (pat, down)
                               in
                                  ({exp = exp,
                                    pat = pat},
                                   combineUp (u, u'))
                               end)
                     val (tyvars, u) = finish (combineUp (u, u'))
                  in
                     (doit (Val {rvbs = rvbs,
                                 tyvars = tyvars,
                                 vbs = vbs}),
                      u)
                  end
         end
      and loopExp (e: Exp.t, d: 'down): Exp.t * 'up =
         let
            val loopMatch = fn m => loopMatch (m, d)
            fun loop (e: Exp.t): Exp.t * 'up =
               let
                  fun empty () = (e, initUp)
                  val region = Exp.region e
                  fun doit n = Exp.makeRegion (n, region)
                  datatype z = datatype Exp.node
                  fun do1 ((a, u), f) = (doit (f a), u)
                  fun do2 ((a1, u1), (a2, u2), f) =
                     (doit (f (a1, a2)), combineUp (u1, u2))
                  fun do3 ((a1, u1), (a2, u2), (a3, u3), f) =
                     (doit (f (a1, a2, a3)), combineUp (u1, combineUp (u2, u3)))
                  fun doVec (es: Exp.t vector, f: Exp.t vector -> Exp.node)
                     : Exp.t * 'up =
                     let
                        val (es, u) = loops (es, loop)
                     in
                        (doit (f es), u)
                     end
               in
                  case Exp.node e of
                     Andalso (e1, e2) => do2 (loop e1, loop e2, Andalso)
                   | App (e1, e2) => do2 (loop e1, loop e2, App)
                   | Case (e, m) => do2 (loop e, loopMatch m, Case)
                   | Const _ => empty ()
                   | Constraint (e, t) => do2 (loop e, loopTy (t, d), Constraint)
                   | FlatApp es => doVec (es, FlatApp)
                   | Fn m => do1 (loopMatch m, Fn)
                   | Handle (e, m) => do2 (loop e, loopMatch m, Handle)
                   | If (e1, e2, e3) => do3 (loop e1, loop e2, loop e3, If)
                   | Let (dec, e) => do2 (loopDec (dec, d), loop e, Let)
                   | List ts => doVec (ts, List)
                   | Orelse (e1, e2) => do2 (loop e1, loop e2, Orelse)
                   | Prim kind => do1 (loopPrimKind (kind, d), Prim)
                   | Raise exn => do1 (loop exn, Raise)
                   | Record r =>
                        let
                           val (r, u) = Record.change (r, fn es =>
                                                       loops (es, loop))
                        in
                           (doit (Record r), u)
                        end
                   | Selector _ => empty ()
                   | Seq es => doVec (es, Seq)
                   | Var _ => empty ()
                   | While {expr, test} =>
                        do2 (loop expr, loop test, fn (expr, test) =>
                             While {expr = expr, test = test})
               end
         in
            loop e
         end
      and loopMatch (m, d) =
         let
            val (Match.T rules, region) = Match.dest m
            val (rules, u) =
               loops (rules, fn (p, e) =>
                      let
                         val (p, u) = loopPat (p, d)
                         val (e, u') = loopExp (e, d)
                      in
                         ((p, e), combineUp (u, u'))
                      end)
         in
            (Match.makeRegion (Match.T rules, region),
             u)
         end
   in
      loopDec (d, initDown)
   end

fun scope (dec: Dec.t): Dec.t =
   let
      fun bindFunVal (env, tyvars) =
         let
            val (env, tyvars) = Env.rename (env, tyvars)
            fun finish {free, mayNotBind} =
               let
                  val bound =
                     Vector.fromList
                     (Tyvars.toList
                      (Tyvars.+ (free, Tyvars.fromList (Vector.toList tyvars))))
                  val mayNotBind =
                     List.keepAll
                     (mayNotBind, fn a =>
                      not (Vector.exists (bound, fn a' =>
                                          Tyvar.sameName (a, a')))
                      orelse
                      let
                         open Layout
                         val _ =
                            Control.error
                            (Tyvar.region a,
                             seq [str "type variable ",
                                  Tyvar.layout a,
                                  str " scoped at an outer declaration"],
                             empty)
                      in
                         false
                      end)
               in
                  (bound,
                   {free = Tyvars.empty,
                    mayNotBind = List.append (Vector.toList tyvars, mayNotBind)})
               end
         in
            (env, finish)
         end
      fun bindType (env, tyvars) =
         let
            val (env, tyvars) = Env.rename (env, tyvars)
            fun finish {free, mayNotBind = _} =
               {free = Tyvars.- (free, Tyvars.fromList (Vector.toList tyvars)),
                mayNotBind = []}
         in
            (env, tyvars, finish)
         end
      fun tyvar (a, env) =
         let
            val a =
               case Env.peek (env, a) of
                  NONE => a
                | SOME a => a
         in
            (a, {free = Tyvars.singleton a,
                 mayNotBind = []})
         end
      fun combineUp ({free = f, mayNotBind = m}, {free = f', mayNotBind = m'}) =
         {free = Tyvars.+ (f, f'),
          mayNotBind = List.append (m, m')}
      val (dec, {free = unguarded, ...}) =
         processDec (dec, {bindFunVal = bindFunVal,
                           bindType = bindType,
                           combineUp = combineUp,
                           initDown = Env.empty,
                           initUp = {free = Tyvars.empty, mayNotBind = []},
                           tyvar = tyvar})
   in
      if Tyvars.isEmpty unguarded
         then
            let
               (* Walk down and bind a tyvar as soon as you sees it, removing
                * all lower binding occurrences of the tyvar.  Also, rename all
                * lower free occurrences of the tyvar to be the same as the
                * binding occurrence (so that they can share info).
                *)
               fun bindFunVal (env, tyvars: Tyvar.t vector) =
                  let
                     val domain = Env.domain env
                     val (env, tyvars) =
                        Env.rename
                        (env,
                         Vector.keepAll
                         (tyvars, fn a =>
                          not (List.exists
                               (domain, fn a' => Tyvar.sameName (a, a')))))
                  in
                     (env, fn () => (tyvars, ()))
                  end
               fun bindType (env, tyvars) =
                  let
                     val (env, tyvars) = Env.rename (env, tyvars)
                  in
                     (env, tyvars, fn () => ())
                  end
               fun tyvar (a, env) = (Env.lookup (env, a), ())
               val (dec, ()) =
                  processDec (dec, {bindFunVal = bindFunVal,
                                    bindType = bindType,
                                    combineUp = fn ((), ()) => (),
                                    initDown = Env.empty,
                                    initUp = (),
                                    tyvar = tyvar})
            in
               dec
            end
      else
         let
            val _ =
               List.foreach
               (Tyvars.toList unguarded, fn a =>
                let
                   open Layout
                in
                   Control.error (Tyvar.region a,
                                  seq [str "undefined type variable: ",
                                       Tyvar.layout a],
                                  empty)
                end)
         in
            dec
         end
   end

val scope = Trace.trace ("Scope.scope", Dec.layout, Dec.layout) scope

end
