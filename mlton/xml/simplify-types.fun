(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor SimplifyTypes (S: SIMPLIFY_TYPES_STRUCTS): SIMPLIFY_TYPES = 
struct

open S
structure I = Input
structure O = Output
open I.Atoms

structure PowerSetLat =
   struct
      datatype t = T of {isIn: bool ref,
                         whenIn: (unit -> unit) list ref} vector

      fun isIn (T v, i) =
         ! (#isIn (Vector.sub (v, i)))

      fun new (size: int) = T (Vector.tabulate (size, fn _ =>
                                                {isIn = ref false,
                                                 whenIn = ref []}))

      fun add (T v, i) =
         let
            val {isIn, whenIn, ...} = Vector.sub (v, i)
         in
            if !isIn
               then ()
            else (isIn := true
                  ; List.foreach (!whenIn, fn f => f ()))
         end

      fun whenIn (T v, i, f) =
         let
            val {isIn, whenIn, ...} = Vector.sub (v, i)
         in
            if !isIn
               then f ()
            else List.push (whenIn, f)
         end
   end

fun simplifyTypes (I.Program.T {body, datatypes, overflow}) =
   let
      val {get = tyconInfo: Tycon.t -> {used: PowerSetLat.t} option,
           set = setTyconInfo, ...} =
         Property.getSetOnce (Tycon.plist, Property.initConst NONE)
      val _ =
         Vector.foreach
         (datatypes, fn {tycon, tyvars, ...} =>
          setTyconInfo (tycon,
                        SOME {used = PowerSetLat.new (Vector.length tyvars)}))
      val _ =
         Vector.foreach
         (datatypes, fn {cons, tycon, tyvars} =>
          let
             val {get = tyvarIndex, set = setTyvarIndex, rem, ...} =
                Property.getSet
                (Tyvar.plist, Property.initRaise ("index", Tyvar.layout))
             val _ = Vector.foreachi (tyvars, fn (i, a) => setTyvarIndex (a, i))
             val {used, ...} = valOf (tyconInfo tycon)
             val {destroy, hom} =
                I.Type.makeHom
                {con = (fn (_, tc, ts) =>
                        fn () =>
                        case tyconInfo tc of
                           NONE => Vector.foreach (ts, fn t => t ())
                         | SOME {used, ...} =>
                              Vector.foreachi
                              (ts, fn (i, t) =>
                               PowerSetLat.whenIn (used, i, t))),
                 var = (fn (_, a) =>
                        let
                           val i = tyvarIndex a
                        in
                           fn () => PowerSetLat.add (used, i)
                        end)}
             val _ =
                Vector.foreach
                (cons, fn {arg, ...} =>
                 case arg of
                    NONE => ()
                  | SOME t => hom t ())
             val _ = Vector.foreach (tyvars, rem)
             val _ = destroy ()
          in
             ()
          end)
      val {get = tyconKeep: Tycon.t -> bool vector option,
           set = setTyconKeep, ...} =
         Property.getSetOnce (Tycon.plist, Property.initConst NONE)
      val {get = conKeep: Con.t -> bool vector option,
           set = setConKeep, ...} =
         Property.getSetOnce (Con.plist, Property.initConst NONE)
      val _ =
         Vector.foreach
         (datatypes, fn {cons, tycon, tyvars} =>
          let
             val {used, ...} = valOf (tyconInfo tycon)
             val v =
                Vector.tabulate
                (Vector.length tyvars, fn i => PowerSetLat.isIn (used, i))
             val _ = Vector.foreach (cons, fn {con, ...} =>
                                     setConKeep (con, SOME v))
             val u =
                if Vector.forall (v, fn b => b)
                   then NONE
                else SOME v
             val _ = setTyconKeep (tycon, u)
          in
             ()
          end)
      fun keep (v: 'a vector, bv: bool vector): 'a vector =
         Vector.keepAllMapi (v, fn (i, a) =>
                             if Vector.sub (bv, i)
                                then SOME a
                             else NONE)
      val {get = tyvarIsUsed: Tyvar.t -> bool ref, ...} =
         Property.get (Tyvar.plist, Property.initFun (fn _ => ref false))
      (* There is some mesiness with promises here for two reasons:
       * 1. The thunk is to make sure that even though we are using a type
       *    homomorphism, a type variable is only marked as used if it appears
       *    in the output.
       * 2. The promise is do avoid computing the same output multiple times.
       *    This is necessary because the type homomorphism only memoizes the
       *    mapping from type to thunk, *not* the thunk's output.
       *)
      val {hom = fixType: I.Type.t -> unit -> O.Type.t, ...} =
         I.Type.makeHom
         {con = (fn (_, tc, ts) =>
                 Promise.lazy
                 (fn () =>
                  let
                     val ts =
                        case tyconKeep tc of
                           NONE => ts
                         | SOME bv => keep (ts, bv)
                     val ts = Vector.map (ts, fn t => t ())
                  in
                     O.Type.con (tc, ts)
                  end)),
          var = (fn (_, a) =>
                 Promise.lazy
                 (fn () => (tyvarIsUsed a := true; O.Type.var a)))}
      val fixType = fn t => fixType t ()
      val fixType =
         Trace.trace 
         ("SimplifyTypes.fixType", I.Type.layout, O.Type.layout) 
         fixType
      val tyvarIsUsed = ! o tyvarIsUsed
      val datatypes =
         Vector.map (datatypes, fn {cons, tycon, tyvars} =>
                     {cons = Vector.map (cons, fn {arg, con} =>
                                         {arg = Option.map (arg, fixType),
                                          con = con}),
                      tycon = tycon,
                      tyvars = (case tyconKeep tycon of
                                   NONE => tyvars
                                 | SOME bv => keep (tyvars, bv))})
      val {get = varKeep: Var.t -> bool vector option,
           set = setVarKeep, ...} =
         Property.getSetOnce (Var.plist, Property.initConst NONE)
      fun fixVarExp (I.VarExp.T {targs, var}): O.VarExp.t =
         let
            val targs =
               case varKeep var of
                  NONE => targs
                | SOME bv => keep (targs, bv)
         in
            O.VarExp.T {targs = Vector.map (targs, fixType),
                        var = var}
         end
      val fixVarExp =
         Trace.trace 
         ("SimplifyTypes.fixVarExp", I.VarExp.layout, O.VarExp.layout) 
         fixVarExp
      fun fixConTargs (con: Con.t, targs: I.Type.t vector): O.Type.t vector =
         let
            val targs =
               case conKeep con of
                  NONE => targs
                | SOME bv => keep (targs, bv)
         in
            Vector.map (targs, fixType)
         end
      fun fixPat (I.Pat.T {arg, con, targs}): O.Pat.t =
         O.Pat.T {arg = Option.map (arg, fn (x, t) => (x, fixType t)),
                  con = con,
                  targs = fixConTargs (con, targs)}
      fun fixDec (d: I.Dec.t): O.Dec.t =
         case d of
            I.Dec.Exception {arg, con} =>
               O.Dec.Exception {arg = Option.map (arg, fixType),
                                con = con}
          | I.Dec.Fun {decs, tyvars} =>
               let
                  val decs =
                     Vector.map (decs, fn {lambda, ty, var} =>
                                 {lambda = fixLambda lambda,
                                  ty = fixType ty,
                                  var = var})
                  val bv = Vector.map (tyvars, tyvarIsUsed)
                  val tyvars = keep (tyvars, bv)
                  val _ =
                     Vector.foreach
                     (decs, fn {var, ...} => setVarKeep (var, SOME bv))
               in
                  O.Dec.Fun {decs = decs,
                             tyvars = tyvars}
               end
          | I.Dec.MonoVal {exp, ty, var} =>
               O.Dec.MonoVal {exp = fixPrimExp exp,
                              ty = fixType ty,
                              var = var}
          | I.Dec.PolyVal {exp, ty, tyvars, var} =>
               let
                  val exp = fixExp exp
                  val ty = fixType ty
                  val bv = Vector.map (tyvars, tyvarIsUsed)
                  val _ = setVarKeep (var, SOME bv)
               in
                  O.Dec.PolyVal {exp = exp,
                                 ty = ty,
                                 tyvars = keep (tyvars, bv),
                                 var = var}
               end
      and fixExp (e: I.Exp.t): O.Exp.t =
         let
            val {decs, result} = I.Exp.dest e
         in
            O.Exp.make {decs = List.map (decs, fixDec),
                        result = fixVarExp result}
         end
      and fixLambda (l: I.Lambda.t): O.Lambda.t =
         let
            val {arg, argType, body, mayInline} = I.Lambda.dest l
         in
            O.Lambda.make {arg = arg,
                           argType = fixType argType,
                           body = fixExp body,
                           mayInline = mayInline}
         end
      and fixPrimExp (e: I.PrimExp.t): O.PrimExp.t =
         case e of
            I.PrimExp.App {arg, func} => O.PrimExp.App {arg = fixVarExp arg,
                                                        func = fixVarExp func}
          | I.PrimExp.Case {cases, default, test} =>
               let
                  val cases =
                     case cases of
                        I.Cases.Con v =>
                           O.Cases.Con (Vector.map (v, fn (p, e) =>
                                                    (fixPat p, fixExp e)))
                      | I.Cases.Word (s, v) =>
                           O.Cases.Word
                           (s, Vector.map (v, fn (c, e) => (c, fixExp e)))
               in
                  O.PrimExp.Case {cases = cases,
                                  default = Option.map (default, fn (e, r) =>
                                                        (fixExp e, r)),
                                  test = fixVarExp test}
               end
          | I.PrimExp.ConApp {arg, con, targs} =>
               O.PrimExp.ConApp {arg = Option.map (arg, fixVarExp),
                                 con = con,
                                 targs = fixConTargs (con, targs)}
          | I.PrimExp.Const c => O.PrimExp.Const c
          | I.PrimExp.Handle {catch = (x, t), handler, try} =>
               O.PrimExp.Handle {catch = (x, fixType t),
                                 handler = fixExp handler,
                                 try = fixExp try}
          | I.PrimExp.Lambda l => O.PrimExp.Lambda (fixLambda l)
          | I.PrimExp.PrimApp {args, prim, targs} =>
               O.PrimExp.PrimApp {args = Vector.map (args, fixVarExp),
                                  prim = Prim.map (prim, fixType),
                                  targs = Vector.map (targs, fixType)}
          | I.PrimExp.Profile e => O.PrimExp.Profile e
          | I.PrimExp.Raise {exn, extend} =>
               O.PrimExp.Raise {exn = fixVarExp exn,
                                extend = extend}
          | I.PrimExp.Select {offset, tuple} =>
               O.PrimExp.Select {offset = offset,
                                 tuple = fixVarExp tuple}
          | I.PrimExp.Tuple xs => O.PrimExp.Tuple (Vector.map (xs, fixVarExp))
          | I.PrimExp.Var x => O.PrimExp.Var (fixVarExp x)
      val body = fixExp body
   in
      O.Program.T {datatypes = datatypes,
                   body = body,
                   overflow = overflow}
   end

end
