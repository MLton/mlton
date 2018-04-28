(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Globalize (S: GLOBALIZE_STRUCTS): GLOBALIZE = 
struct

open S
open Dec PrimExp

fun globalize {program = Program.T {datatypes, body, ...},
               lambdaFree,
               varGlobal: Var.t -> bool ref} =
   let
      val noConts =
         not (Exp.hasPrim (body, fn p =>
                           case Prim.name p of
                              Prim.Name.Thread_switchTo => true
                            | _ => false))
      local
         val {get: Tycon.t -> bool, set, destroy} =
            Property.destGetSetOnce (Tycon.plist, Property.initConst false)
         fun makeBig tycon = set (tycon, true)
         val _ = (Vector.foreach (datatypes, makeBig o #tycon)
                  ; makeBig Tycon.array
                  ; makeBig Tycon.arrow
                  ; makeBig Tycon.vector)
      in
         val tyconIsBig = get
         val destroyTycon = destroy
      end
      fun typeIsSmall t =
         let open Type
         in
            case dest t of
               Con (c, ts) =>
                  not (tyconIsBig c)
                  andalso if (Tycon.equals (c, Tycon.tuple)
                              orelse Tycon.equals (c, Tycon.reff))
                             then Vector.forall (ts, typeIsSmall)
                          else true
             | _ => Error.bug "Globalize.typeIsSmall: type variable"
         end
      val typeIsSmall =
         Trace.trace ("Globalize.typeIsSmall", Type.layout, Bool.layout)
         typeIsSmall
      val varIsGlobal = ! o varGlobal
      val isGlobal = varIsGlobal o VarExp.var
      fun areGlobal xs = Vector.forall (xs, isGlobal)
      fun makeGlobal x = varGlobal x := true
      val traceLoopExp =
         Trace.trace2 ("Globalize.loopExp", Exp.layout, Bool.layout, Bool.layout)
      val traceLoopDec =
         Trace.trace2 ("Globalize.loopDec", Dec.layout, Bool.layout, Bool.layout)
      fun loopExp arg =
         traceLoopExp (fn (e: Exp.t, once: bool) =>
                       List.fold (Exp.decs e, once, loopDec))
         arg
      and loopDec arg =
         traceLoopDec
         (fn (d, once) =>
           case d of
              MonoVal {var, ty, exp} =>
                 let
                    val (global, once) =
                       case exp of
                          App _ =>
                             (* If conts are used, then the application might
                              * call Thread_copyCurrent, in which case,
                              * subsequent stuff might run many times.
                              *)
                             (false, once andalso noConts)
                        | Case {cases, default, ...} =>
                             let
                                val once' =
                                   Cases.fold
                                   (cases, once, fn (e, b) =>
                                    loopExp (e, once) andalso b)
                                val once' =
                                   Option.fold (default, once',
                                                fn ((e, _), b) =>
                                                loopExp (e, once) andalso b)
                             in (false, once')
                             end
                        | ConApp {arg, ...} =>
                             (case arg of
                                 NONE => true
                               | SOME x => isGlobal x,
                                    once)
                        | Const _ => (true, once)
                        | Handle {try, handler, ...} =>
                             (false,
                              loopExp (handler, loopExp (try, once)))
                        | Lambda l =>
                             (loopLambda l
                              ; (Vector.forall (lambdaFree l, varIsGlobal),
                                 once))
                        | PrimApp {prim, args, ...} =>
                             let
                                val global =
                                   areGlobal args andalso
                                   ((Prim.isFunctional prim
                                     (* Don't want to move MLton_equal or MLton_hash
                                      * into the globals because polymorphic 
                                      * equality and hasing isn't implemented
                                      * there. 
                                      *)
                                     andalso
                                     (case Prim.name prim of
                                         Prim.Name.MLton_equal => false
                                       | Prim.Name.MLton_hash => false
                                       | _ => true))
                                    orelse
                                    (once andalso
                                     (case Prim.name prim of
                                         Prim.Name.Ref_ref => typeIsSmall ty
                                       | _ => false)))
                                val once =
                                    once andalso
                                    (case Prim.name prim of
                                        Prim.Name.Thread_copyCurrent => false
                                      | _ => true)
                             in
                                (global, once)
                             end
                        | Profile _ => (false, once)
                        | Raise _ => (false, once)
                        | Select {tuple, ...} => (isGlobal tuple, once)
                        | Tuple xs => (areGlobal xs, once)
                        | Var x => (isGlobal x, once)
                    val _ = if global then makeGlobal var else ()
                 in once
                 end
            | Fun {decs, ...} =>
                 (if Vector.isEmpty decs
                     then ()
                  else
                     let
                        val {lambda, ...} = Vector.first decs
                     in
                        if Vector.forall (lambdaFree lambda, varIsGlobal)
                           then Vector.foreach (decs, makeGlobal o #var)
                        else ()
                     end
                     ; Vector.foreach (decs, loopLambda o #lambda)
                     ; once)
            | _ => Error.bug "Globalize.loopDec: strange dec") arg
      and loopLambda (l: Lambda.t): unit =
         ignore (loopExp (Lambda.body l, false))
      val _ = loopExp (body, true)
      val _ = destroyTycon ()
   in
      ()
   end

end
