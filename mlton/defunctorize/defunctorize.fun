(* Copyright (C) 2015,2017 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Defunctorize (S: DEFUNCTORIZE_STRUCTS): DEFUNCTORIZE = 
struct

open S

local
   open CoreML
in
   structure Const = Const
   structure Cdec = Dec
   structure Cexp = Exp
   structure Clambda = Lambda
   structure Cpat = Pat
   structure Prim = Prim
   structure RealSize = RealSize
   structure Record = Record
   structure SortedRecord = SortedRecord
   structure SourceInfo = SourceInfo
   structure Ctype = Type
   structure WordSize = WordSize
   structure WordX = WordX
end

structure Field = Record.Field

local
   open Xml
in
   structure Xcases = Cases
   structure Con = Con
   structure Xdec = Dec
   structure Xexp = DirectExp
   structure Xlambda = Lambda
   structure Xpat = Pat
   structure XprimExp = PrimExp
   structure Tycon = Tycon
   structure Xtype = Type
   structure Tyvar = Tyvar
   structure Var = Var
   structure XvarExp = VarExp
end

structure NestedPat = NestedPat (open Xml)

structure MatchCompile =
   MatchCompile (open CoreML
                 structure Type = Xtype
                 structure NestedPat = NestedPat
                 structure Cases =
                    struct
                       type exp = Xexp.t

                       open Xcases
                       type t = exp t
                       val word = Word
                       fun con v =
                          Con (Vector.map
                               (v, fn {con, targs, arg, rhs} =>
                                (Xpat.T {con = con,
                                         targs = targs,
                                         arg = arg},
                                 rhs)))
                    end
                structure Exp =
                   struct
                      open Xexp
                      val lett = let1
                      val var = monoVar

                      fun detuple {tuple, body} =
                         Xexp.detuple
                         {tuple = tuple,
                          body = fn xts => body (Vector.map
                                                 (xts, fn (x, t) =>
                                                  (XvarExp.var x, t)))}

                      fun devector {vector, length, body} =
                         Xexp.devector
                         {vector = vector,
                          length = length,
                          body = fn xts => body (Vector.map
                                                 (xts, fn (x, t) =>
                                                  (XvarExp.var x, t)))}
                   end)

structure Xexp =
   struct
      open Xexp

      local
         fun exn (c: Con.t): Xexp.t =
            conApp {arg = NONE,
                    con = c,
                    targs = Vector.new0 (),
                    ty = Xtype.exn}
      in
         val bind = exn Con.bind
         val match = exn Con.match
      end
   end

fun enterLeave (e: Xexp.t, t, si): Xexp.t =
   Xexp.fromExp (Xml.Exp.enterLeave (Xexp.toExp e, t, si), t)

local
val matchDiagnostics: (unit -> unit) list ref = ref []
in
fun addMatchDiagnostic (diag, mkArg) =
   case diag of
      Control.Elaborate.DiagEIW.Error =>
         List.push (matchDiagnostics, Control.error o mkArg)
    | Control.Elaborate.DiagEIW.Ignore => ()
    | Control.Elaborate.DiagEIW.Warn =>
         List.push (matchDiagnostics, Control.warning o mkArg)
fun showMatchDiagnostics () = List.foreach (!matchDiagnostics, fn th => th ())
end

fun casee {ctxt: unit -> Layout.t,
           caseType: Xtype.t,
           cases: {exp: Xexp.t,
                   layPat: (unit -> Layout.t) option,
                   pat: NestedPat.t,
                   regionPat: Region.t} vector,
           conTycon,
           kind: (string * string),
           nest: string list,
           matchDiags: {nonexhaustiveExn: Control.Elaborate.DiagDI.t,
                        nonexhaustive: Control.Elaborate.DiagEIW.t,
                        redundant: Control.Elaborate.DiagEIW.t},
           noMatch,
           region: Region.t,
           test = (test: Xexp.t, testType: Xtype.t),
           tyconCons}: Xexp.t =
   let
      val nonexhaustiveExnDiag = #nonexhaustiveExn matchDiags
      val nonexhaustiveDiag = #nonexhaustive matchDiags
      val redundantDiag = #redundant matchDiags
      val cases = Vector.map (cases, fn {exp, layPat, pat, regionPat} =>
                              {exp = fn () => exp,
                               isDefault = false,
                               layPat = layPat,
                               numPats = ref 0,
                               numUses = ref 0,
                               pat = pat,
                               regionPat = regionPat})
      fun raiseExn (f, mayWrap) =
         let
            val e = Var.newNoname ()
            val exp = Xexp.raisee {exn = f e, extend = true, ty = caseType}
            val exp =
               fn () =>
               if let
                     open Control
                  in
                     !profile <> ProfileNone 
                     andalso !profileIL = ProfileSource
                     andalso !profileRaise
                  end
                  then case mayWrap of
                          NONE => exp
                        | SOME kind => 
                             enterLeave 
                             (exp, caseType,
                              SourceInfo.function 
                              {name = (concat ["<raise ", kind, ">"]) :: nest,
                               region = region})
               else exp
         in
            Vector.concat
            [cases,
             Vector.new1 {exp = exp,
                          isDefault = true,
                          layPat = NONE,
                          numPats = ref 0,
                          numUses = ref 0,
                          pat = NestedPat.make (NestedPat.Var e, testType),
                          regionPat = Region.bogus}]
         end
      val cases =
         let
            datatype z = datatype Cexp.noMatch
         in
            case noMatch of
               Impossible => cases
             | RaiseAgain =>
                  raiseExn (fn e => Xexp.monoVar (e, Xtype.exn), NONE)
             | RaiseBind => raiseExn (fn _ => Xexp.bind, SOME "Bind")
             | RaiseMatch => raiseExn (fn _ => Xexp.match, SOME "Match")
         end
      fun matchCompile () =                                  
         let
            val testVar = Var.newNoname ()
            val decs = ref []
            val cases =
               Vector.map
               (cases, fn {exp = e, numPats, numUses, pat = p, ...} =>
                let
                   val args = Vector.fromList (NestedPat.varsAndTypes p)
                   val (vars, tys) = Vector.unzip args
                   val func = Var.newNoname ()
                   val arg = Var.newNoname ()
                   val argType = Xtype.tuple tys
                   val funcType = Xtype.arrow (argType, caseType)
                   fun dec () =
                      Xdec.MonoVal
                      {var = func,
                       ty = funcType,
                       exp =
                       XprimExp.Lambda
                       (Xlambda.make
                        {arg = arg,
                         argType = argType,
                         body = (Xexp.toExp
                                 (Xexp.detupleBind
                                  {tuple = Xexp.monoVar (arg, argType),
                                   components = vars,
                                   body = e ()})),
                         mayInline = true})}
                   fun finish np =
                      (numPats := np
                       ; fn rename =>
                         (if 0 = !numUses then List.push (decs, dec ()) else ()
                          ; Int.inc numUses
                          ; (Xexp.app
                             {func = Xexp.monoVar (func, funcType),
                              arg =
                              Xexp.tuple {exps = (Vector.map
                                                  (args, fn (x, t) =>
                                                   Xexp.monoVar (rename x, t))),
                                          ty = argType},
                              ty = caseType})))
                in
                   (p, finish)
                end)
            val (body, nonexhaustiveExamples) =
               MatchCompile.matchCompile {caseType = caseType,
                                          cases = cases,
                                          conTycon = conTycon,
                                          region = region,
                                          test = testVar,
                                          testType = testType,
                                          tyconCons = tyconCons}
            (* Must convert to a normal expression to force everything. *)
            val body = Xexp.toExp body
            val nonexhaustiveExamples =
               if noMatch = Cexp.Impossible
                  then NONE
                  else let
                          val dropOnlyExns =
                             case nonexhaustiveExnDiag of
                                Control.Elaborate.DiagDI.Default =>
                                   {dropOnlyExns = false}
                              | Control.Elaborate.DiagDI.Ignore =>
                                   {dropOnlyExns = true}
                       in
                          nonexhaustiveExamples dropOnlyExns
                       end
         in
            (Xexp.let1 {var = testVar,
                        exp = test,
                        body = Xexp.lett {decs = !decs,
                                          body = Xexp.fromExp (body, caseType)}},
             nonexhaustiveExamples)
         end
      datatype z = datatype NestedPat.node
      fun lett (x, e) = Xexp.let1 {var = x, exp = test, body = e}
      fun wild e = lett (Var.newNoname (), e)
      val (exp, nonexhaustiveExamples) =
         if Vector.isEmpty cases
            then Error.bug "Defunctorize.casee: case with no patterns"
         else
            let
               val {exp = e, pat = p, numPats, numUses, ...} = Vector.first cases
               fun use () = (numPats := 1; numUses := 1)
               fun exhaustive exp = (exp, NONE)
               fun loop p =
                  case NestedPat.node p of
                     Wild => (use (); exhaustive (wild (e ())))
                   | Var x => (use (); exhaustive (lett (x, e ())))
                   | Record rps =>
                        let
                           val ps = SortedRecord.range rps
                           fun doitRecord () =
                              (* It's a flat record pattern.
                               * Generate the selects.
                               *)
                              let
                                 val _ = use ()
                                 val t = Var.newNoname ()
                                 val tuple = XvarExp.mono t
                                 val tys = Xtype.deTuple testType
                                 val (_, decs) =
                                    Vector.fold2
                                    (ps, tys, (0, []),
                                     fn (p, ty, (i, decs)) =>
                                     case NestedPat.node p of
                                        Var x =>
                                           (i + 1,
                                            Xdec.MonoVal
                                            {var = x,
                                             ty = ty,
                                             exp = (XprimExp.Select
                                                    {tuple = tuple,
                                                     offset = i})}
                                            :: decs)
                                      | Wild => (i + 1, decs)
                                      | _ => Error.bug "Defunctorize.casee: flat record")
                              in
                                 exhaustive (Xexp.let1
                                             {var = t, exp = test,
                                              body = Xexp.lett
                                              {decs = decs,
                                               body = e ()}})
                              end
                        in
                           if Vector.forall (ps, NestedPat.isVarOrWild)
                              then if Vector.length ps = 1
                                      then loop (Vector.first ps)
                                      else doitRecord ()
                           else matchCompile ()
                        end
                   | _ => matchCompile ()
            in
               loop p
            end
      (* diagnoseRedundant *)
      val _ =
         Vector.foreachr
         (cases, fn {isDefault, layPat = layPat,
                     numPats, numUses, regionPat = regionPat, ...} =>
          let
             fun doit (msg1, msg2) =
                let
                   open Layout
                in
                   addMatchDiagnostic
                   (redundantDiag,
                    fn () =>
                    (regionPat,
                     str (concat [#1 kind, msg1]),
                     align [seq [str (concat [msg2, ": "]),
                                 case layPat of
                                    NONE => Error.bug "Defunctorize.casee: redundant match with no lay"
                                  | SOME layPat => layPat ()],
                            ctxt ()]))
                end
          in
             if not isDefault andalso !numUses = 0
                then ((* Rule with no uses; fully redundant. *)
                      doit (" has redundant " ^ #2 kind,
                            "redundant pattern"))
             else if not isDefault andalso !numUses > 0 andalso !numUses < !numPats
                then ((* Rule with some uses but fewer uses than pats; partially redundant. *)
                      doit (" has " ^ #2 kind ^ " with redundancy",
                            "pattern with redundancy"))
             else ()
          end)
      (* diagnoseNonexhaustive *)
      val _ =
         Option.app
         (nonexhaustiveExamples, fn es =>
          let
             open Layout
          in
             addMatchDiagnostic
             (nonexhaustiveDiag,
              fn () =>
              (region,
               str (concat [#1 kind, " is not exhaustive"]),
               align [seq [str "missing pattern: ", es],
                      ctxt ()]))
          end)
   in
      exp
   end

val casee =
   Trace.trace ("Defunctorize.casee",
                Region.layout o #region,
                Xml.Exp.layout o Xexp.toExp)
   casee

fun 'a sortByField (v: (Field.t * 'a) vector): 'a vector =
   Vector.map (QuickSort.sortVector (v, fn ((f, _), (f', _)) =>
                                     Field.<= (f, f')),
               #2)

fun valDec (tyvars: Tyvar.t vector,
            x: Var.t,
            e: Xexp.t,
            et: Xtype.t,
            e': Xexp.t): Xexp.t =
   Xexp.lett {body = e',
              decs = [Xdec.PolyVal {exp = Xexp.toExp e,
                                    ty = et,
                                    tyvars = tyvars,
                                    var = x}]}

structure Xexp =
   struct
      open Xexp

      fun list (es: Xexp.t vector, ty: Xtype.t, {forceLeftToRight: bool})
         : Xexp.t =
         let
            val targs = #2 (valOf (Xtype.deConOpt ty))
            val eltTy = Vector.first targs
            val nill: Xexp.t =
               Xexp.conApp {arg = NONE,
                            con = Con.nill,
                            targs = targs,
                            ty = ty}
            val consArgTy = Xtype.tuple (Vector.new2 (eltTy, ty))
            val cons: Xexp.t * Xexp.t -> Xexp.t =
               fn (e1, e2) =>
               Xexp.conApp
               {arg = SOME (Xexp.tuple {exps = Vector.new2 (e1, e2),
                                        ty = consArgTy}),
                con = Con.cons,
                targs = targs,
                ty = ty}
         in
            if not forceLeftToRight
               then
                  (* Build the list right to left. *)
                  Vector.foldr (es, nill, fn (e, rest) =>
                                let
                                   val var = Var.newNoname ()
                                in
                                   Xexp.let1 {body = cons (e, monoVar (var, ty)),
                                              exp = rest,
                                              var = var}
                                end)
            else if Vector.length es < 20
               then Vector.foldr (es, nill, cons)
            else
               let
                  val revArgTy = Xtype.tuple (Vector.new2 (ty, ty))
                  val revTy = Xtype.arrow (revArgTy, ty)
                  val revVar = Var.newString "rev"
                  fun rev (e1, e2) =
                     Xexp.app
                     {func = Xexp.monoVar (revVar, revTy),
                      arg = Xexp.tuple {exps = Vector.new2 (e1, e2),
                                        ty = revArgTy},
                      ty = ty}
                  fun detuple2 (tuple: Xexp.t,
                                f: XvarExp.t * XvarExp.t -> Xexp.t): Xexp.t =
                     Xexp.detuple {body = fn xs => let
                                                      fun x i = #1 (Vector.sub (xs, i))
                                                   in
                                                      f (x 0, x 1)
                                                   end,
                                                tuple = tuple}
                  val revArg = Var.newNoname ()
                  val revLambda =
                     Xlambda.make
                     {arg = revArg,
                      argType = revArgTy,
                      mayInline = true,
                      body =
                      Xexp.toExp
                      (detuple2
                       (Xexp.monoVar (revArg, revArgTy), fn (l, ac) =>
                        let
                           val ac = Xexp.varExp (ac, ty)
                           val consArg = Var.newNoname ()
                        in
                           Xexp.casee
                           {cases =
                            Xcases.Con
                            (Vector.new2
                             ((Xpat.T {arg = NONE,
                                       con = Con.nill,
                                       targs = targs},
                               ac),
                              (Xpat.T {arg = SOME (consArg, consArgTy),
                                       con = Con.cons,
                                       targs = targs},
                               detuple2
                               (Xexp.monoVar (consArg, consArgTy),
                                fn (x, l) =>
                                rev (Xexp.varExp (l, ty),
                                     cons (Xexp.varExp (x, eltTy),
                                           ac)))))),
                            default = NONE,
                            test = Xexp.varExp (l, ty),
                            ty = ty}
                        end))}
                  val revDec =
                     Xdec.Fun
                     {decs = Vector.new1 {lambda = revLambda,
                                          ty = revTy,
                                          var = revVar},
                      tyvars = Vector.new0 ()}
                  val l = Var.newNoname ()
                  val (l, body) =
                     Vector.foldr
                     (es, (l, Xexp.lett {decs = [revDec],
                                         body = rev (Xexp.monoVar (l, ty),
                                                     nill)}),
                      fn (e, (l, body)) =>
                      let
                         val l' = Var.newNoname ()
                      in
                         (l',
                          Xexp.let1 {body = body,
                                     exp = cons (e, Xexp.monoVar (l', ty)),
                                     var = l})
                      end)
               in
                  Xexp.let1 {body = body,
                             exp = nill,
                             var = l}
               end
         end
   end

fun defunctorize (CoreML.Program.T {decs}) =
   let
      val {get = conExtraArgs: Con.t -> Xtype.t vector option,
           set = setConExtraArgs, destroy = destroy1, ...} =
         Property.destGetSetOnce (Con.plist, Property.initConst NONE)
      val {get = tyconExtraArgs: Tycon.t -> Xtype.t vector option,
           set = setTyconExtraArgs, destroy = destroy2, ...} =
         Property.destGetSetOnce (Tycon.plist, Property.initConst NONE)
      val {destroy = destroy3, hom = loopTy} =
         let
            fun con (c, ts) =
               let
                  val ts =
                     case tyconExtraArgs c of
                        NONE => ts
                      | SOME ts' => Vector.concat [ts', ts]
               in
                  Xtype.con (c, ts)
               end
         in
            Ctype.makeHom {con = con, var = Xtype.var}
         end
      val loopTy =
         Trace.trace
         ("Defunctorize.loopTy", Ctype.layout, Xtype.layout)
         loopTy
      fun conTargs (c: Con.t, ts: Ctype.t vector): Xtype.t vector =
         let
            val ts = Vector.map (ts, loopTy)
         in
            case conExtraArgs c of
               NONE => ts
             | SOME ts' => Vector.concat [ts', ts]
         end
      val {get = conTycon, set = setConTycon, ...} =
         Property.getSetOnce (Con.plist,
                              Property.initRaise ("conTycon", Con.layout))
      val {get = tyconCons: Tycon.t -> {con: Con.t,
                                        hasArg: bool} vector,
           set = setTyconCons, ...} =
         Property.getSetOnce (Tycon.plist,
                              Property.initRaise ("tyconCons", Tycon.layout))
      val setConTycon =
         Trace.trace2 
         ("Defunctorize.setConTycon", 
          Con.layout, Tycon.layout, Unit.layout)
         setConTycon
      val datatypes = ref []
      (* Process all the datatypes. *)
      fun loopDec (d: Cdec.t) =
         let
            datatype z = datatype Cdec.t
         in
            case d of
               Datatype dbs =>
                  let
                     val frees: Tyvar.t list ref = ref []
                     val _ =
                        Vector.foreach
                        (dbs, fn {cons, tyvars, ...} =>
                         let
                            fun var (a: Tyvar.t): unit =
                               let
                                  fun eq a' = Tyvar.equals (a, a')
                               in
                                  if Vector.exists (tyvars, eq)
                                     orelse List.exists (!frees, eq)
                                     then ()
                                  else List.push (frees, a)
                               end
                            val {destroy, hom} =
                               Ctype.makeHom {con = fn _ => (),
                                              var = var}
                            val _ =
                               Vector.foreach (cons, fn {arg, ...} =>
                                               Option.app (arg, hom))
                            val _ = destroy ()
                         in
                            ()
                         end)
                     val frees = !frees
                     val dbs =
                        if List.isEmpty frees
                           then dbs
                        else
                           let
                              val frees = Vector.fromList frees
                              val extra = Vector.map (frees, Xtype.var)
                           in
                              Vector.map
                              (dbs, fn {cons, tycon, tyvars} =>
                               let
                                  val _ = setTyconExtraArgs (tycon, SOME extra)
                                  val _ =
                                     Vector.foreach
                                     (cons, fn {con, ...} =>
                                      setConExtraArgs (con, SOME extra))
                               in
                                  {cons = cons,
                                   tycon = tycon,
                                   tyvars = Vector.concat [frees, tyvars]}
                               end)
                           end
                  in
                     Vector.foreach
                     (dbs, fn {cons, tycon, tyvars} =>
                      let
                         val _ =
                            setTyconCons (tycon,
                                          Vector.map (cons, fn {arg, con} =>
                                                      {con = con,
                                                       hasArg = isSome arg}))
                         val cons =
                            Vector.map
                            (cons, fn {arg, con} =>
                             (setConTycon (con, tycon)
                              ; {arg = Option.map (arg, loopTy),
                                 con = con}))

                         val _ = 
                            if Tycon.equals (tycon, Tycon.reff)
                               then ()
                            else
                               List.push (datatypes, {cons = cons,
                                                      tycon = tycon,
                                                      tyvars = tyvars})
                      in
                         ()
                      end)
                  end
             | Exception {con, ...} => setConTycon (con, Tycon.exn)
             | Fun {decs, ...} => Vector.foreach (decs, loopLambda o #lambda)
             | Val {rvbs, vbs, ...} =>
                  (Vector.foreach (rvbs, loopLambda o #lambda)
                   ; Vector.foreach (vbs, loopExp o #exp))
         end
      and loopExp (e: Cexp.t): unit =
         let
            datatype z = datatype Cexp.node
         in
            case Cexp.node e of
               App (e, e') => (loopExp e; loopExp e')
             | Case {rules, test, ...} =>
                  (loopExp test
                   ; Vector.foreach (rules, loopExp o #exp))
             | Con _ => ()
             | Const _ => ()
             | EnterLeave (e, _) => loopExp e
             | Handle {handler, try, ...} => (loopExp handler; loopExp try)
             | Lambda l => loopLambda l
             | Let (ds, e) => (Vector.foreach (ds, loopDec); loopExp e)
             | List es => Vector.foreach (es, loopExp)
             | PrimApp {args, ...} => Vector.foreach (args, loopExp)
             | Raise e => loopExp e
             | Record r => Record.foreach (r, loopExp)
             | Seq es => Vector.foreach (es, loopExp)
             | Var _ => ()
             | Vector es => Vector.foreach (es, loopExp)
         end
      and loopLambda (l: Clambda.t): unit =
         loopExp (#body (Clambda.dest l))
      fun loopPat (p: Cpat.t): NestedPat.t =
         let
            val (p, t) = Cpat.dest p
            val t' = loopTy t
            datatype z = datatype Cpat.node
            val p = 
               case p of
                  Con {arg, con, targs} =>
                     NestedPat.Con {arg = Option.map (arg, loopPat),
                                    con = con,
                                    targs = conTargs (con, targs)}
                | Const f =>
                     NestedPat.Const {const = f (),
                                      isChar = Ctype.isCharX t,
                                      isInt = Ctype.isInt t}
                | Layered (x, p) => NestedPat.Layered (x, loopPat p)
                | List ps =>
                     let
                        val targs = Vector.map (#2 (valOf (Ctype.deConOpt t)),
                                                loopTy)
                     in
                        Vector.foldr
                        (ps,
                         NestedPat.Con {arg = NONE,
                                        con = Con.nill,
                                        targs = targs},
                         fn (p, np) =>
                         NestedPat.Con {arg = SOME (NestedPat.tuple
                                                    (Vector.new2
                                                     (loopPat p,
                                                      NestedPat.make (np, t')))),
                                        con = Con.cons,
                                        targs = targs})
                     end
                | Record r =>
                     NestedPat.Record
                     (SortedRecord.fromVector
                      (Vector.map
                       (Ctype.deRecord t, fn (f, t: Ctype.t) =>
                        (f,
                         case Record.peek (r, f) of
                            NONE => NestedPat.make (NestedPat.Wild, loopTy t)
                          | SOME p => loopPat p))))
                | Or ps => NestedPat.Or (Vector.map (ps, loopPat))
                | Var x => NestedPat.Var x
                | Vector ps => NestedPat.Vector (Vector.map (ps, loopPat))
                | Wild => NestedPat.Wild
         in
            NestedPat.make (p, t')
         end
      val _ = Vector.foreach (decs, loopDec)
      (* Now, do the actual defunctorization. *)
      fun loopDec (d: Cdec.t, e: Xexp.t, et: Xtype.t): Xexp.t =
         let
            fun prefix (d: Xdec.t) =
               Xexp.lett {decs = [d], body = e}
            fun processLambdas v =
               Vector.map
               (Vector.rev v, fn {lambda, var} =>
                let
                   val {arg, argType, body, bodyType, mayInline} =
                      loopLambda lambda
                in
                   {lambda = Xlambda.make {arg = arg,
                                           argType = argType,
                                           body = Xexp.toExp body,
                                           mayInline = mayInline},
                    ty = Xtype.arrow (argType, bodyType),
                    var = var}
                end)
            datatype z = datatype Cdec.t
         in
            case d of
               Datatype _ => e
             | Exception {arg, con} =>
                  prefix (Xdec.Exception {arg = Option.map (arg, loopTy),
                                          con = con})
             | Fun {decs, tyvars} =>
                  prefix (Xdec.Fun {decs = processLambdas decs,
                                    tyvars = tyvars ()})
             | Val {matchDiags, rvbs, tyvars, vbs} =>
               let
                  val tyvars = tyvars ()
                  val bodyType = et
                  val e =
                     Vector.foldr
                     (vbs, e, fn ({ctxt, exp, layPat, nest, pat, regionPat}, e) =>
                      let
                         fun patDec (p: NestedPat.t,
                                     e: Xexp.t,
                                     body: Xexp.t,
                                     bodyType: Xtype.t,
                                     mayWarn: bool) =
                            casee {ctxt = ctxt,
                                   caseType = bodyType,
                                   cases = Vector.new1 {exp = body,
                                                        layPat = SOME layPat,
                                                        pat = p,
                                                        regionPat = regionPat},
                                   conTycon = conTycon,
                                   kind = ("declaration", "pattern"),
                                   nest = nest,
                                   matchDiags = if mayWarn
                                                   then matchDiags
                                                   else {nonexhaustiveExn = Control.Elaborate.DiagDI.Default,
                                                         nonexhaustive = Control.Elaborate.DiagEIW.Ignore,
                                                         redundant = Control.Elaborate.DiagEIW.Ignore},
                                   noMatch = Cexp.RaiseBind,
                                   region = regionPat,
                                   test = (e, NestedPat.ty p),
                                   tyconCons = tyconCons}
                         val isExpansive = Cexp.isExpansive exp
                         val (exp, expType) = loopExp exp
                         val pat = loopPat pat
                         fun vd (x: Var.t) = valDec (tyvars, x, exp, expType, e)
                      in
                         if Vector.isEmpty tyvars
                            then patDec (pat, exp, e, bodyType, true)
                         else if isExpansive
                            then
                               let
                                  val x = Var.newNoname ()
                                  val thunk =
                                     let
                                        open Xexp
                                     in
                                        toExp
                                        (lambda
                                         {arg = Var.newNoname (),
                                          argType = Xtype.unit,
                                          body = exp,
                                          bodyType = expType,
                                          mayInline = true})
                                     end
                                  val thunkTy =
                                     Xtype.arrow (Xtype.unit, expType)
                                  fun subst t =
                                     Xtype.substitute
                                     (t, Vector.map (tyvars, fn a =>
                                                     (a, Xtype.unit)))
                                  val body =
                                     Xexp.app
                                     {arg = Xexp.unit (),
                                      func =
                                      Xexp.var
                                      {targs = (Vector.map
                                                (tyvars, fn _ =>
                                                 Xtype.unit)),
                                       ty = subst thunkTy,
                                       var = x},
                                      ty = subst expType}
                                  val decs =
                                     [Xdec.PolyVal {exp = thunk,
                                                    ty = thunkTy,
                                                    tyvars = tyvars,
                                                    var = x}]
                               in
                                  patDec (NestedPat.replaceTypes (pat, subst),
                                          Xexp.lett {body = body, decs = decs},
                                          e, bodyType, true)
                               end
                         else
                            case NestedPat.node pat of
                               NestedPat.Wild => vd (Var.newNoname ())
                             | NestedPat.Var x => vd x
                             | _ =>
                                  (* Polymorphic pattern.
                                   *  val 'a Foo (y1, y2) = e
                                   * Expands to
                                   *  val 'a x = e
                                   *  val Foo (_, _) = x  (* for match warnings *)
                                   *  val 'a y1 = case x of Foo (y1', _) => y1'
                                   *  val 'a y2 = case x of Foo (_, y2') => y2'
                                   *)
                                  let
                                     val x = Var.newNoname ()
                                     val xt = expType
                                     val targs = Vector.map (tyvars, Xtype.var)
                                     val e =
                                        List.fold
                                        (NestedPat.varsAndTypes pat, e,
                                         fn ((y, yt), e) =>
                                         let
                                            val y' = Var.new y
                                            val pat =
                                               NestedPat.removeOthersReplace
                                               (pat, {old = y, new = y'})
                                         in
                                            valDec
                                            (tyvars,
                                             y,
                                             patDec (pat,
                                                     Xexp.var {targs = targs,
                                                               ty = xt,
                                                               var = x},
                                                     Xexp.monoVar (y', yt),
                                                     yt,
                                                     false),
                                             yt,
                                             e)
                                         end)
                                     fun instantiatePat () =
                                        let
                                           val pat = NestedPat.removeVars pat
                                           fun con (_, c, ts) = Xtype.con (c, ts)
                                           fun var (t, a) =
                                              if (Vector.exists
                                                  (tyvars, fn a' =>
                                                   Tyvar.equals (a, a')))
                                                 then Xtype.unit
                                              else t
                                           val {destroy, hom} =
                                              Xtype.makeHom {con = con,
                                                             var = var}
                                           val pat =
                                              NestedPat.replaceTypes
                                              (pat, hom)
                                           val _ = destroy ()
                                        in
                                           pat
                                        end
                                     val e =
                                        if NestedPat.isRefutable pat
                                           then
                                               let
                                                  val targs =
                                                     Vector.map (tyvars, fn _ =>
                                                                 Xtype.unit)
                                                  val pat = instantiatePat ()
                                               in
                                                  patDec
                                                  (pat,
                                                   Xexp.var
                                                   {targs = targs,
                                                    ty = NestedPat.ty pat,
                                                    var = x},
                                                   e,
                                                   bodyType,
                                                   true)
                                               end
                                        else e
                                  in
                                     valDec (tyvars, x, exp, expType, e)
                                  end
                      end)
               in
                  if Vector.isEmpty rvbs
                     then e
                  else
                     Xexp.lett {decs = [Xdec.Fun {decs = processLambdas rvbs,
                                                  tyvars = tyvars}],
                                body = e}
               end
         end
      and loopDecs (ds: Cdec.t vector, (e: Xexp.t, t: Xtype.t)): Xexp.t =
         loopDecsList (Vector.toList ds, (e, t))
      (* Convert vector->list to allow processed Cdecs to be GC'ed. *)
      and loopDecsList (ds: Cdec.t list, (e: Xexp.t, t: Xtype.t)): Xexp.t =
         List.foldr (ds, e, fn (d, e) => loopDec (d, e, t))
      and loopExp (e: Cexp.t): Xexp.t * Xtype.t =
         let
            val (n, ty) = Cexp.dest e
            val ty = loopTy ty
            fun conApp {arg, con, targs, ty} =
               if Con.equals (con, Con.reff)
                  then Xexp.primApp {args = Vector.new1 arg,
                                     prim = Prim.reff,
                                     targs = targs,
                                     ty = ty}
               else Xexp.conApp {arg = SOME arg,
                                 con = con,
                                 targs = targs,
                                 ty = ty}
            datatype z = datatype Cexp.node
            val exp =
               case n of
                  App (e1, e2) =>
                     let
                        val (e2, _) = loopExp e2
                     in
                        case Cexp.node e1 of
                           Con (con, targs) =>
                              conApp {arg = e2,
                                      con = con,
                                      targs = conTargs (con, targs),
                                      ty = ty}
                         | _ => 
                              Xexp.app {arg = e2,
                                        func = #1 (loopExp e1),
                                        ty = ty}
                     end
                | Case {ctxt, kind, nest, matchDiags, noMatch, region, rules, test, ...} =>
                     casee {ctxt = ctxt,
                            caseType = ty,
                            cases = Vector.map (rules, fn {exp, layPat, pat, regionPat} =>
                                                {exp = #1 (loopExp exp),
                                                 layPat = layPat,
                                                 pat = loopPat pat,
                                                 regionPat = regionPat}),
                            conTycon = conTycon,
                            kind = kind,
                            nest = nest,
                            matchDiags = matchDiags,
                            noMatch = noMatch,
                            region = region,
                            test = loopExp test,
                            tyconCons = tyconCons}
                | Con (con, targs) =>
                     let
                        val targs = conTargs (con, targs)
                     in
                        case Xtype.deArrowOpt ty of
                           NONE =>
                              Xexp.conApp {arg = NONE,
                                           con = con,
                                           targs = targs,
                                           ty = ty}
                         | SOME (argType, bodyType) =>
                              let
                                 val arg = Var.newNoname ()
                              in
                                 Xexp.lambda
                                 {arg = arg,
                                  argType = argType,
                                  body = (conApp
                                          {arg = Xexp.monoVar (arg, argType),
                                           con = con,
                                           targs = targs,
                                           ty = bodyType}),
                                  bodyType = bodyType,
                                  mayInline = true}
                              end
                     end
                | Const f =>
                     let
                        val c = f ()
                     in
                        if Xtype.equals (ty, Xtype.bool)
                           then
                              (case c of
                                  Const.Word w =>
                                     if WordX.isZero w
                                        then Xexp.falsee ()
                                     else Xexp.truee ()
                                | _ => Error.bug "Defunctorize.loopExp: Const:strange boolean constant")
                        else Xexp.const c
                     end
                | EnterLeave (e, si) =>
                     let
                        val (e, t) = loopExp e
                     in
                        enterLeave (e, t, si)
                     end
                | Handle {catch = (x, t), handler, try} =>
                     Xexp.handlee {catch = (x, loopTy t),
                                   handler = #1 (loopExp handler),
                                   try = #1 (loopExp try),
                                   ty = ty}
                | Lambda l => Xexp.lambda (loopLambda l)
                | Let (ds, e) => loopDecs (ds, loopExp e)
                | List es =>
                     let
                        (* Must evaluate list components left-to-right if there
                         * is more than one expansive expression.
                         *)
                        val numExpansive =
                           Vector.fold (es, 0, fn (e, n) =>
                                        if Cexp.isExpansive e then n + 1 else n)
                     in
                        Xexp.list (Vector.map (es, #1 o loopExp), ty,
                                   {forceLeftToRight = 2 <= numExpansive})
                     end
                | PrimApp {args, prim, targs} =>
                     let
                        val args = Vector.map (args, #1 o loopExp)
                        datatype z = datatype Prim.Name.t
                     in
                        if (case Prim.name prim of
                               Real_rndToReal (s1, s2) =>
                                  RealSize.equals (s1, s2)
                             | String_toWord8Vector => true
                             | Word_extdToWord (s1, s2, _) => 
                                  WordSize.equals (s1, s2)
                             | Word8Vector_toString => true
                             | _ => false)
                           then Vector.first args
                        else
                           Xexp.primApp {args = args,
                                         prim = Prim.map (prim, loopTy),
                                         targs = Vector.map (targs, loopTy),
                                         ty = ty}

                     end
                | Raise e => Xexp.raisee {exn = #1 (loopExp e), extend = true, ty = ty}
                | Record r =>
                     (* The components of the record have to be evaluated left to 
                      * right as they appeared in the source program, but then
                      * ordered according to sorted field name within the tuple.
                      *)
                     let
                        val fes = Record.toVector r
                     in
                        Xexp.seq
                        (Vector.map (fes, #1 o loopExp o #2), fn es =>
                         Xexp.tuple {exps = (sortByField
                                             (Vector.map2
                                              (fes, es, fn ((f, _), e) => (f, e)))),
                                     ty = ty})
                     end
                | Seq es => Xexp.sequence (Vector.map (es, #1 o loopExp))
                | Var (var, targs) =>
                     Xexp.var {targs = Vector.map (targs (), loopTy),
                               ty = ty,
                               var = var ()}
                | Vector es =>
                     Xexp.primApp {args = Vector.map (es, #1 o loopExp),
                                   prim = Prim.vector,
                                   targs = Vector.new1 (Xtype.deVector ty),
                                   ty = ty}
         in
            (exp, ty)
         end
      and loopLambda (l: Clambda.t) =
         let
            val {arg, argType, body, mayInline} = Clambda.dest l
            val (body, bodyType) = loopExp body
         in
            {arg = arg,
             argType = loopTy argType,
             body = body,
             bodyType = bodyType,
             mayInline = mayInline}
         end
      val body = Xexp.toExp (loopDecs (decs, (Xexp.unit (), Xtype.unit)))
      val _ = showMatchDiagnostics ()
      val _ = (destroy1 (); destroy2 (); destroy3 ())
   in
      Xml.Program.T {body = body,
                     datatypes = Vector.fromList (!datatypes),
                     overflow = NONE}
   end

end
