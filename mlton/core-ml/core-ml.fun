(* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor CoreML (S: CORE_ML_STRUCTS): CORE_ML = 
struct

open S

structure Field = Record.Field

fun maybeConstrain (x, t) =
   let
      open Layout
   in
      if !Control.showTypes
         then seq [x, str ": ", Type.layout t]
      else x
   end

fun layoutTargs (ts: Type.t vector) =
   let
      open Layout
   in
      if !Control.showTypes
         andalso 0 < Vector.length ts
         then list (Vector.toListMap (ts, Type.layout))
      else empty
   end

structure Pat =
   struct
      datatype t = T of {node: node,
                         ty: Type.t}
      and node =
         Con of {arg: t option,
                 con: Con.t,
                 targs: Type.t vector}
       | Const of unit -> Const.t
       | Layered of Var.t * t
       | List of t vector
       | Or of t vector
       | Record of t Record.t
       | Tuple of t vector
       | Var of Var.t
       | Wild

      local
         fun make f (T r) = f r
      in
         val dest = make (fn {node, ty} => (node, ty))
         val node = make #node
         val ty = make #ty
      end

      fun make (n, t) = T {node = n, ty = t}

      fun layout p =
         let
            val t = ty p
            open Layout
         in
            case node p of
               Con {arg, con, targs} =>
                  seq [Con.layout con,
                       layoutTargs targs,
                       case arg of
                          NONE => empty
                        | SOME p => seq [str " ", layout p]]
             | Const f => Const.layout (f ())
             | Layered (x, p) =>
                  seq [maybeConstrain (Var.layout x, t), str " as ", layout p]
             | List ps => list (Vector.toListMap (ps, layout))
             | Or ps => list (Vector.toListMap (ps, layout))
             | Record r =>
                  record (Vector.toListMap
                          (Record.toVector r, fn (f, p) =>
                           (Field.toString f, layout p)))
             | Tuple ps => tuple (Vector.toListMap (ps, layout))
             | Var x => maybeConstrain (Var.layout x, t)
             | Wild => str "_"
         end

      fun wild t = make (Wild, t)

      fun var (x, t) = make (Var x, t)

      fun tuple ps = make (Tuple ps, Type.tuple (Vector.map (ps, ty)))

      local
         fun bool c = make (Con {arg = NONE, con = c, targs = Vector.new0 ()},
                            Type.bool)
      in
         val falsee: t = bool Con.falsee
         val truee: t = bool Con.truee
      end

      fun isUnit (p: t): bool =
         case node p of
            Tuple v => 0 = Vector.length v
          | _ => false

      fun isWild (p: t): bool =
         case node p of
            Wild => true
          | _ => false

      fun isRefutable (p: t): bool =
         case node p of
            Con _ => true
          | Const _ => true
          | Layered (_, p) => isRefutable p
          | List _ => true
          | Or ps => Vector.exists (ps, isRefutable)
          | Record r => Record.exists (r, isRefutable)
          | Tuple ps => Vector.exists (ps, isRefutable)
          | Var _ => false
          | Wild => false

      fun foreachVar (p: t, f: Var.t -> unit): unit =
         let
            fun loop (p: t): unit =
               case node p of
                  Con _ => ()
                | Const _ => ()
                | Layered (x, p) => (f x; loop p)
                | List ps => Vector.foreach (ps, loop)
                | Or ps => Vector.foreach (ps, loop)
                | Record r => Record.foreach (r, loop)
                | Tuple ps => Vector.foreach (ps, loop)
                | Var x => f x
                | Wild => ()
         in
            loop p
         end
   end

structure NoMatch =
   struct
      datatype t = Impossible | RaiseAgain | RaiseBind | RaiseMatch
   end

datatype noMatch = datatype NoMatch.t

datatype dec =
   Datatype of {cons: {arg: Type.t option,
                       con: Con.t} vector,
                tycon: Tycon.t,
                tyvars: Tyvar.t vector} vector
 | Exception of {arg: Type.t option,
                 con: Con.t}
 | Fun of {decs: {lambda: lambda,
                  var: Var.t} vector,
           tyvars: unit -> Tyvar.t vector}
 | Val of {nonexhaustiveExnMatch: Control.Elaborate.DiagDI.t,
           nonexhaustiveMatch: Control.Elaborate.DiagEIW.t,
           rvbs: {lambda: lambda,
                  var: Var.t} vector,
           tyvars: unit -> Tyvar.t vector,
           vbs: {exp: exp,
                 lay: unit -> Layout.t,
                 nest: string list,
                 pat: Pat.t,
                 patRegion: Region.t} vector}
and exp = Exp of {node: expNode,
                  ty: Type.t}
and expNode =
   App of exp * exp
  | Case of {kind: string,
             lay: unit -> Layout.t,
             nest: string list,
             noMatch: noMatch,
             nonexhaustiveExnMatch: Control.Elaborate.DiagDI.t,
             nonexhaustiveMatch: Control.Elaborate.DiagEIW.t,
             redundantMatch: Control.Elaborate.DiagEIW.t,
             region: Region.t,
             rules: {exp: exp,
                     lay: (unit -> Layout.t) option,
                     pat: Pat.t} vector,
             test: exp}
  | Con of Con.t * Type.t vector
  | Const of unit -> Const.t
  | EnterLeave of exp * SourceInfo.t
  | Handle of {catch: Var.t * Type.t,
               handler: exp,
               try: exp}
  | Lambda of lambda
  | Let of dec vector * exp
  | List of exp vector
  | PrimApp of {args: exp vector,
                prim: Type.t Prim.t,
                targs: Type.t vector}
  | Raise of exp
  | Record of exp Record.t
  | Seq of exp vector
  | Var of (unit -> Var.t) * (unit -> Type.t vector)
and lambda = Lam of {arg: Var.t,
                     argType: Type.t,
                     body: exp,
                     mayInline: bool}

local
   open Layout
in
   fun layoutTyvars (ts: Tyvar.t vector) =
      case Vector.length ts of
         0 => empty
       | 1 => seq [str " ", Tyvar.layout (Vector.sub (ts, 0))]
       | _ => seq [str " ", tuple (Vector.toListMap (ts, Tyvar.layout))]

   fun layoutConArg {arg, con} =
      seq [Con.layout con,
           case arg of
              NONE => empty
            | SOME t => seq [str " of ", Type.layout t]]

   fun layoutDec d =
      case d of
         Datatype v =>
            seq [str "datatype",
                 align
                 (Vector.toListMap
                  (v, fn {cons, tycon, tyvars} =>
                   seq [layoutTyvars tyvars,
                        str " ", Tycon.layout tycon, str " = ",
                        align
                        (separateLeft (Vector.toListMap (cons, layoutConArg),
                                       "| "))]))]
       | Exception ca =>
            seq [str "exception ", layoutConArg ca]
       | Fun {decs, tyvars, ...} => layoutFuns (tyvars, decs)
       | Val {rvbs, tyvars, vbs, ...} =>
            align [layoutFuns (tyvars, rvbs),
                   align (Vector.toListMap
                          (vbs, fn {exp, pat, ...} =>
                           seq [str "val",
                                mayAlign [seq [layoutTyvars (tyvars ()),
                                               str " ", Pat.layout pat,
                                               str " ="],
                                          layoutExp exp]]))]
   and layoutExp (Exp {node, ...}) =
      case node of
         App (e1, e2) => paren (seq [layoutExp e1, str " ", layoutExp e2])
       | Case {rules, test, ...} =>
            Pretty.casee {default = NONE,
                          rules = Vector.map (rules, fn {exp, pat, ...} =>
                                              (Pat.layout pat, layoutExp exp)),
                          test = layoutExp test}
       | Con (c, targs) => seq [Con.layout c, layoutTargs targs]
       | Const f => Const.layout (f ())
       | EnterLeave (e, si) =>
            seq [str "EnterLeave ",
                 tuple [layoutExp e, SourceInfo.layout si]]
       | Handle {catch, handler, try} =>
            Pretty.handlee {catch = Var.layout (#1 catch),
                            handler = layoutExp handler,
                            try = layoutExp try}
       | Lambda l => layoutLambda l
       | Let (ds, e) =>
            Pretty.lett (align (Vector.toListMap (ds, layoutDec)),
                         layoutExp e)
       | List es => list (Vector.toListMap (es, layoutExp))
       | PrimApp {args, prim, targs} =>
            Pretty.primApp {args = Vector.map (args, layoutExp),
                            prim = Prim.layout prim,
                            targs = Vector.map (targs, Type.layout)}
       | Raise e => Pretty.raisee (layoutExp e)
       | Record r =>
            Record.layout
            {extra = "",
             layoutElt = layoutExp,
             layoutTuple = fn es => tuple (Vector.toListMap (es, layoutExp)),
             record = r,
             separator = " = "}
       | Seq es => Pretty.seq (Vector.map (es, layoutExp))
       | Var (var, targs) => 
            if !Control.showTypes
               then let 
                       open Layout
                       val targs = targs ()
                    in
                       if Vector.isEmpty targs
                          then Var.layout (var ())
                       else seq [Var.layout (var ()), str " ",
                                 Vector.layout Type.layout targs]
                    end
            else Var.layout (var ())
   and layoutFuns (tyvars, decs)  =
      if 0 = Vector.length decs
         then empty
      else
         align [seq [str "val rec", layoutTyvars (tyvars ())],
                indent (align (Vector.toListMap
                               (decs, fn {lambda as Lam {argType, body = Exp {ty = bodyType, ...}, ...}, var} =>
                                align [seq [maybeConstrain (Var.layout var, Type.arrow (argType, bodyType)), str " = "],
                                       indent (layoutLambda lambda, 3)])),
                        3)]
   and layoutLambda (Lam {arg, argType, body, ...}) =
      paren (align [seq [str "fn ", 
                         maybeConstrain (Var.layout arg, argType),
                         str " =>"],
                    layoutExp body])

   fun layoutExpWithType (exp as Exp {ty, ...}) =
      let
         val node = layoutExp exp
      in
         if !Control.showTypes
            then seq [node, str " : ", Type.layout ty]
         else node
      end
end

structure Lambda =
   struct
      datatype t = datatype lambda

      val make = Lam

      fun dest (Lam r) = r

      val bogus = make {arg = Var.newNoname (),
                        argType = Type.unit,
                        body = Exp {node = Seq (Vector.new0 ()),
                                    ty = Type.unit},
                        mayInline = true}
   end

structure Exp =
   struct
      type dec = dec
      type lambda = lambda
      datatype t = datatype exp
      datatype node = datatype expNode

      datatype noMatch = datatype noMatch

      val layout = layoutExp
      val layoutWithType = layoutExpWithType

      local
         fun make f (Exp r) = f r
      in
         val dest = make (fn {node, ty} => (node, ty))
         val node = make #node
         val ty = make #ty
      end

      fun make (n, t) = Exp {node = n,
                             ty = t}

      fun var (x: Var.t, ty: Type.t): t =
         make (Var (fn () => x, fn () => Vector.new0 ()), ty)

      fun isExpansive (e: t): bool =
         case node e of
            App (e1, e2) =>
               (case node e1 of
                   Con (c, _) => Con.equals (c, Con.reff) orelse isExpansive e2
                 | _ => true)
          | Case _ => true
          | Con _ => false
          | Const _ => false
          | EnterLeave _ => true
          | Handle _ => true
          | Lambda _ => false
          | Let _ => true
          | List es => Vector.exists (es, isExpansive)
          | PrimApp _ => true
          | Raise _ => true
          | Record r => Record.exists (r, isExpansive)
          | Seq _ => true
          | Var _ => false

      fun tuple es =
         if 1 = Vector.length es
            then Vector.sub (es, 0)
         else make (Record (Record.tuple es),
                    Type.tuple (Vector.map (es, ty)))

      val unit = tuple (Vector.new0 ())

      local
         fun bool c = make (Con (c, Vector.new0 ()), Type.bool)
      in
         val falsee: t = bool Con.falsee
         val truee: t = bool Con.truee
      end

      fun lambda (l as Lam {argType, body, ...}) =
         make (Lambda l, Type.arrow (argType, ty body))

      fun casee (z as {rules, ...}) =
         if 0 = Vector.length rules
            then Error.bug "CoreML.Exp.casee"
         else make (Case z, ty (#exp (Vector.sub (rules, 0))))

      fun iff (test, thenCase, elseCase): t =
         casee {kind = "if",
                lay = fn () => Layout.empty,
                nest = [],
                noMatch = Impossible,
                nonexhaustiveExnMatch = Control.Elaborate.DiagDI.Default,
                nonexhaustiveMatch = Control.Elaborate.DiagEIW.Ignore,
                redundantMatch = Control.Elaborate.DiagEIW.Ignore,
                region = Region.bogus,
                rules = Vector.new2 ({exp = thenCase,
                                      lay = NONE,
                                      pat = Pat.truee},
                                     {exp = elseCase,
                                      lay = NONE,
                                      pat = Pat.falsee}),
                test = test}

      fun andAlso (e1, e2) = iff (e1, e2, falsee)

      fun orElse (e1, e2) = iff (e1, truee, e2)

      fun whilee {expr, test} =
         let
            val loop = Var.newNoname ()
            val loopTy = Type.arrow (Type.unit, Type.unit)
            val call = make (App (var (loop, loopTy), unit), Type.unit)
            val lambda =
               Lambda.make
               {arg = Var.newNoname (),
                argType = Type.unit,
                body = iff (test,
                            make (Seq (Vector.new2 (expr, call)),
                                  Type.unit),
                            unit),
                mayInline = true}
         in
            make
            (Let (Vector.new1 (Fun {decs = Vector.new1 {lambda = lambda,
                                                        var = loop},
                                    tyvars = fn () => Vector.new0 ()}),
                  call),
             Type.unit)
         end

      fun foreachVar (e: t, f: Var.t -> unit): unit =
         let
            fun loop (e: t): unit =
               case node e of
                  App (e1, e2) => (loop e1; loop e2)
                | Case {rules, test, ...} =>
                     (loop test
                      ; Vector.foreach (rules, loop o #exp))
                | Con _ => ()
                | Const _ => ()
                | EnterLeave (e, _) => loop e
                | Handle {handler, try, ...} => (loop handler; loop try)
                | Lambda l => loopLambda l
                | Let (ds, e) =>
                     (Vector.foreach (ds, loopDec)
                      ; loop e)
                | List es => Vector.foreach (es, loop)
                | PrimApp {args, ...} => Vector.foreach (args, loop)
                | Raise e => loop e
                | Record r => Record.foreach (r, loop)
                | Seq es => Vector.foreach (es, loop)
                | Var (x, _) => f (x ())
            and loopDec d =
               case d of
                  Datatype _ => ()
                | Exception _ => ()
                | Fun {decs, ...} => Vector.foreach (decs, loopLambda o #lambda)
                | Val {rvbs, vbs, ...} =>
                     (Vector.foreach (rvbs, loopLambda o #lambda)
                      ; Vector.foreach (vbs, loop o #exp))
            and loopLambda (Lam {body, ...}) = loop body
         in
            loop e
         end
   end

structure Dec =
   struct
      datatype t = datatype dec

      val layout = layoutDec
   end

structure Program =
   struct
      datatype t = T of {decs: Dec.t vector}

      fun layouts (T {decs, ...}, output') =
         let
            open Layout
            (* Layout includes an output function, so we need to rebind output
             * to the one above.
             *)
            val output = output'
         in
            output (Layout.str "\n\nDecs:")
            ; Vector.foreach (decs, output o Dec.layout)
         end

(*       fun typeCheck (T {decs, ...}) =
 *       let
 *          fun checkExp (e: Exp.t): Ty.t =
 *             let
 *                val (n, t) = Exp.dest e
 *                val 
 *                datatype z = datatype Exp.t
 *                val t' =
 *                   case n of
 *                      App (e1, e2) =>
 *                         let
 *                            val t1 = checkExp e1
 *                            val t2 = checkExp e2
 *                         in
 *                            case Type.deArrowOpt t1 of
 *                               NONE => error "application of non-function"
 *                             | SOME (u1, u2) =>
 *                                  if Type.equals (u1, t2)
 *                                     then t2
 *                                  else error "function/argument mismatch"
 *                         end
 *                    | Case {rules, test} =>
 *                         let
 *                            val {pat, exp} = Vector.sub (rules, 0)
 *                         in
 *                            Vector.foreach (rules, fn {pat, exp} =>
 *                                            Type.equals
 *                                            (checkPat pat, 
 *                         end
 *             in
 *                                   
 *             end
 *       in
 *       end
 *)
   end

end
