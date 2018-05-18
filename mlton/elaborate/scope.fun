(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Scope (S: SCOPE_STRUCTS): SCOPE =
struct

open S
open Ast

structure Tyvars = UnorderedSet (Tyvar)
structure Tyvars =
   struct
      open Tyvars
      val fromVector = fn v =>
         Vector.fold (v, empty, fn (x, s) => add (s, x))
   end

fun ('down, 'up)
   processDec (d: Dec.t,
               {(* bindType is used at datatype and type declarations. *)
                bindType: ('down * Tyvar.t vector
                           -> 'down * ('up -> 'up)),
                (* bindFunVal is used at fun, overload, and val declarations. *)
                bindFunVal: ('down * Tyvar.t vector * Region.t
                             -> ('down * ('up -> Tyvar.t vector * 'up))),
                combineUp: 'up * 'up -> 'up,
                initDown: 'down,
                initUp: 'up,
                tyvar: Tyvar.t * 'down -> 'up
                }): Dec.t * 'up =
   let
      fun visits (xs: 'a vector, visitX: 'a -> 'up): 'up =
         Vector.fold (xs, initUp, fn (x, u) => combineUp (u, visitX x))
      fun loops (xs: 'a vector, loopX: 'a -> 'a * 'up): 'a vector * 'up =
         Vector.mapAndFold (xs, initUp, fn (x, u) =>
                            let
                               val (x, u') = loopX x
                            in
                               (x, combineUp (u, u'))
                            end)
      fun visitTy (t: Type.t, d: 'down): 'up =
         let
            datatype z = datatype Type.node
            fun visit (t: Type.t): 'up =
               case Type.node t of
                  Con (_, ts) => visits (ts, visit)
                | Paren t => visit t
                | Record r =>
                     Record.fold
                     (r, initUp, fn ((_, t), u) =>
                      combineUp (u, visit t))
                | Var a => tyvar (a, d)
         in
            visit t
         end
      fun visitTyOpt (to: Type.t option, d: 'down): 'up =
         case to of
            NONE => initUp
          | SOME t => visitTy (t, d)
      fun visitTypBind (tb: TypBind.t, d: 'down): 'up =
         let
            val TypBind.T tbs = TypBind.node tb
            val u =
               visits
               (tbs, fn {def, tyvars, ...} =>
                let
                   val (d, finish) = bindType (d, tyvars)
                in
                   finish (visitTy (def, d))
                end)
         in
            u
         end
      fun visitDatBind (db: DatBind.t, d: 'down): 'up =
         let
            val DatBind.T {datatypes, withtypes} = DatBind.node db
            val u =
               visits
               (datatypes, fn {cons, tyvars, ...} =>
                let
                   val (d, finish) = bindType (d, tyvars)
                in
                   finish (visits (cons, fn (_, arg) =>
                                   visitTyOpt (arg, d)))
                end)
            val u' = visitTypBind (withtypes, d)
         in
            combineUp (u, u')
         end
      fun visitPat (p: Pat.t, d: 'down): 'up =
         let
            datatype z = datatype Pat.node
            fun visit (p: Pat.t): 'up =
               (case Pat.node p of
                   App (_, p) => visit p
                 | Const _ => initUp
                 | Constraint (p, t) =>
                      combineUp (visit p, visitTy (t, d))
                 | FlatApp ps => visits (ps, visit)
                 | Layered {constraint, pat, ...} =>
                      combineUp (visitTyOpt (constraint, d), visit pat)
                 | List ps => visits (ps, visit)
                 | Or ps => visits (ps, visit)
                 | Paren p => visit p
                 | Record {items, ...} =>
                      Vector.fold
                      (items, initUp, fn ((_, _, i), u) =>
                       let
                          datatype z = datatype Pat.Item.t
                          val u' =
                             case i of
                                Field p => visit p
                              | Vid (_, to, po) =>
                                   let
                                      val u = visitTyOpt (to, d)
                                      val u' = visitOpt po
                                   in
                                      combineUp (u, u')
                                   end
                       in
                          combineUp (u, u')
                       end)
                 | Tuple ps => visits (ps, visit)
                 | Var _ => initUp
                 | Vector ps => visits (ps, visit)
                 | Wild => initUp)
            and visitOpt opt =
               (case opt of
                   NONE => initUp
                | SOME p => visit p)
         in
            visit p
         end
      fun visitPrimKind (kind: PrimKind.t, d: 'down): 'up =
         let
            datatype z = datatype PrimKind.t
         in
            case kind of
               Address {ty, ...} =>
                  visitTy (ty, d)
             | BuildConst {ty, ...} =>
                  visitTy (ty, d)
             | CommandLineConst {ty, ...} =>
                  visitTy (ty, d)
             | Const {ty, ...} =>
                  visitTy (ty, d)
             | Export {ty, ...} =>
                  visitTy (ty, d)
             | IImport {ty, ...} =>
                  visitTy (ty, d)
             | Import {ty, ...} =>
                  visitTy (ty, d)
             | ISymbol {ty} =>
                  visitTy (ty, d)
             | Prim {ty, ...} =>
                  visitTy (ty, d)
             | Symbol {ty, ...} =>
                  visitTy (ty, d)
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
                     val u' = visitDatBind (datBind, down)
                  in
                     (doit (Abstype {body = body, datBind = datBind}),
                      combineUp (u, u'))
                  end
             | Datatype rhs =>
                  let
                     datatype z = datatype DatatypeRhs.node
                     val u =
                        case DatatypeRhs.node rhs of
                           DatBind db => visitDatBind (db, down)
                         | Repl _ => initUp
                  in
                     (d, u)
                  end
             | DoDec e =>
                  do1 (loopExp (e, down), DoDec)
             | Exception ebs =>
                  let
                     val u =
                        visits (ebs, fn (_, rhs) =>
                                let
                                   datatype z = datatype EbRhs.node
                                   val u =
                                      case EbRhs.node rhs of
                                         Def _ => initUp
                                       | Gen to =>
                                            let
                                               val u = visitTyOpt (to, down)
                                            in
                                               u
                                            end
                                in
                                   u
                                end)
                  in
                     (d, u)
                  end
             | Fix _ => empty ()
             | Fun {tyvars, fbs} =>
                  let
                     val (down, finish) = bindFunVal (down, tyvars, Dec.region d)
                     val (fbs, u) =
                        loops (fbs, fn clauses =>
                               let
                                  val (clauses, u) =
                                     loops
                                     (clauses, fn {body, pats, resultType} =>
                                      let
                                         val (body, u) = loopExp (body, down)
                                         val u' =
                                            visits (pats, fn p =>
                                                    visitPat (p, down))
                                         val u'' =
                                            visitTyOpt (resultType, down)
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
                     (doit (Fun {tyvars = tyvars, fbs = fbs}), u)
                  end
             | Local (d, d') =>
                  do2 (loopDec (d, down), loopDec (d', down), Local)
             | Open _ => empty ()
             | Overload (i, x, tyvars, ty, ys) =>
                  let
                     val (down, finish) = bindFunVal (down, tyvars, Dec.region d)
                     val up = visitTy (ty, down)
                     val (tyvars, up) = finish up
                  in
                     (doit (Overload (i, x, tyvars, ty, ys)), up)
                  end
             | SeqDec ds => doVec (ds, SeqDec)
             | Type tb =>
                  let
                     val u = visitTypBind (tb, down)
                  in
                     (d, u)
                  end
             | Val {rvbs, tyvars, vbs} =>
                  let
                     val (down, finish) = bindFunVal (down, tyvars, Dec.region d)
                     val (rvbs, u) =
                        loops (rvbs, fn {match, pat} =>
                               let
                                  val (match, u) = loopMatch (match, down)
                                  val u' = visitPat (pat, down)
                               in
                                  ({match = match,
                                    pat = pat},
                                   combineUp (u, u'))
                               end)
                     val (vbs, u') =
                        loops (vbs, fn {exp, pat} =>
                               let
                                  val (exp, u) = loopExp (exp, down)
                                  val u' = visitPat (pat, down)
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
                   | Constraint (e, t) =>
                        let
                           val (e, u) = loop e
                           val u' = visitTy (t, d)
                        in
                           (doit (Constraint (e, t)),
                            combineUp (u, u'))
                        end
                   | FlatApp es => doVec (es, FlatApp)
                   | Fn m => do1 (loopMatch m, Fn)
                   | Handle (e, m) => do2 (loop e, loopMatch m, Handle)
                   | If (e1, e2, e3) => do3 (loop e1, loop e2, loop e3, If)
                   | Let (dec, e) => do2 (loopDec (dec, d), loop e, Let)
                   | List ts => doVec (ts, List)
                   | Orelse (e1, e2) => do2 (loop e1, loop e2, Orelse)
                   | Paren e => do1 (loop e, Paren)
                   | Prim kind => (e, visitPrimKind (kind, d))
                   | Raise exn => do1 (loop exn, Raise)
                   | Record r =>
                        let
                           val (r, u) =
                              Record.change
                              (r, fn res =>
                               loops (res, fn (r, e) =>
                                      let val (e', u) = loop e
                                      in ((r, e'), u)
                                      end))
                        in
                           (doit (Record r), u)
                        end
                   | Selector _ => empty ()
                   | Seq es => doVec (es, Seq)
                   | Var _ => empty ()
                   | Vector vs => doVec (vs, Vector)
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
                         val u = visitPat (p, d)
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
      fun bindFunVal ((), tyvars, regionDec) =
         let
            fun finish {free, mayNotBind} =
               let
                  val bound = Tyvars.+ (free, Tyvars.fromVector tyvars)
                  val mayNotBind =
                     List.keepAll
                     (mayNotBind, fn a =>
                      not (Tyvars.contains (bound, a))
                      orelse
                      let
                         open Layout
                         val _ =
                            Control.error
                            (Tyvar.region a,
                             seq [str "type variable scoped at an outer declaration: ",
                                  Tyvar.layout a],
                             seq [str "scoped at: ", Region.layout regionDec])
                      in
                         false
                      end)
                  val bound = Vector.fromList (Tyvars.toList bound)
               in
                  (bound,
                   {free = Tyvars.empty,
                    mayNotBind = List.append (Vector.toList tyvars, mayNotBind)})
               end
         in
            ((), finish)
         end
      fun bindType ((), tyvars) =
         let
            fun finish {free, mayNotBind = _} =
               {free = Tyvars.- (free, Tyvars.fromVector tyvars),
                mayNotBind = []}
         in
            ((), finish)
         end
      fun tyvar (a, ()) =
         {free = Tyvars.singleton a,
          mayNotBind = []}
      fun combineUp ({free = f, mayNotBind = m}, {free = f', mayNotBind = m'}) =
         {free = Tyvars.+ (f, f'),
          mayNotBind = List.append (m, m')}
      val (dec, _) =
         processDec (dec, {bindFunVal = bindFunVal,
                           bindType = bindType,
                           combineUp = combineUp,
                           initDown = (),
                           initUp = {free = Tyvars.empty, mayNotBind = []},
                           tyvar = tyvar})

      (* Walk down and bind a tyvar as soon as you see it, removing
       * all lower binding occurrences of the tyvar.
       *)
      fun bindFunVal (bound, tyvars: Tyvar.t vector, _) =
         let
            val tyvars =
               Vector.keepAll
               (tyvars, fn a =>
                not (Tyvars.contains (bound, a)))
            val bound =
               Tyvars.+ (bound, Tyvars.fromVector tyvars)
         in
            (bound, fn () => (tyvars, ()))
         end
      fun bindType (bound, tyvars) =
         let
            val bound = Tyvars.+ (bound, Tyvars.fromVector tyvars)
         in
            (bound, fn () => ())
         end
      fun tyvar (_, _) = ()
      val (dec, ()) =
         processDec (dec, {bindFunVal = bindFunVal,
                           bindType = bindType,
                           combineUp = fn ((), ()) => (),
                           initDown = Tyvars.empty,
                           initUp = (),
                           tyvar = tyvar})
   in
      dec
   end

val scope = Trace.trace ("Scope.scope", Dec.layout, Dec.layout) scope

end
