(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor CallCount(S: CALL_COUNT_STRUCTS): CALL_COUNT = 
struct

open S
open Dec PrimExp

fun instrument(program as Program.T{datatypes, body}, passName: string) =
   if !Control.instrument
      then
         let
            datatype kind =
               None
             | Lam of int (* for curried lambdas, how many arrows remain *)
             | Prim

            val {get = kind: Var.t -> kind, set} =
               Property.new(Var.plist, Property.initConst None)

            fun makeLam(l: Lambda.t): kind =
               let
                  fun loop(l, n) =
                     let val {decs, result} = Exp.dest(Lambda.body l)
                     in case decs of
                        [MonoVal{var, exp = Lambda l, ...}] =>
                           if Var.equals(var, VarExp.var result)
                              then loop(l, n + 1)
                           else n
                      | _ => n
                     end
               in Lam(loop(l, 0))
               end

            fun inc(name: string) : unit -> Dec.t =
               let
                  val exp =
                     PrimApp
                     {prim = Prim.newNullary(concat["MLTON_inc", passName, name]),
                      targs = [], args = []}
               in fn () =>
                  MonoVal{var = Var.newNoname(), ty = Type.unit, exp = exp}
               end

            val incCount = inc "Unknown"
            val incObvious = inc "Known"

            val program = Program.T{datatypes = datatypes,
                                    body = body}
            fun loopExp(e: Exp.t): Exp.t =
               let val {decs, result} = Exp.dest e
               in Exp.new{decs = loopDecs decs,
                          result = result}
               end
            and loopDecs(ds: Dec.t list): Dec.t list =
               case ds of
                  [] => []
                | d :: ds =>
                   case d of
                      MonoVal{var, ty, exp} =>
                         let
                            fun keep exp =
                               MonoVal{var = var, ty = ty, exp = exp}
                               :: loopDecs ds
                         in case exp of
                             App{func, ...} =>
                                let fun rest() = d :: loopDecs ds
                                in case kind(VarExp.var func) of
                                   None => incCount() :: rest()
                                 | Prim => rest()
                                 | Lam n => (if n >= 0
                                                then set(var, Lam(n - 1))
                                             else ()
                                             ; incObvious() :: rest())
                                end
                           | Lambda l =>
                                (set(var, 
                                     case Exp.decs(Lambda.body l) of
                                        [MonoVal{exp = PrimApp _, ...}] => Prim
                                      | _ => makeLam l)
                                 ; keep(Lambda(loopLambda l)))
                           | Case{test, cases, default} =>
                                keep
                                (Case{test = test,
                                      cases = List.map(cases, fn (p, e) =>
                                                       (p, loopExp e)),
                                      default = Option.map loopExp default})
                           | Handle{try, catch, handler} =>
                                keep(Handle{try = loopExp try,
                                            catch = catch,
                                            handler = loopExp handler})
                           | _ => d :: loopDecs ds
                         end
                    | PolyVal{var, tyvars, ty, exp} =>
                         PolyVal{var = var, tyvars = tyvars, ty = ty,
                                 exp = loopExp exp}
                         :: loopDecs ds
                    | Fun{tyvars, decs} =>
                         (List.foreach(decs, fn {var, lambda, ...} =>
                                       set(var, makeLam lambda))
                         ; Fun{tyvars = tyvars,
                               decs = List.map(decs, fn {var, ty, lambda} =>
                                               {var = var, ty = ty,
                                                lambda = loopLambda lambda})}
                          :: loopDecs ds)
                    | Exception _ => d :: loopDecs ds
            and loopLambda(l: Lambda.t): Lambda.t =
               let val {arg, argType, body} = Lambda.dest l
               in Lambda.new{arg = arg, argType = argType,
                             body = loopExp body}
               end

            val program = Program.T{datatypes = datatypes,
                                    body = loopExp body}

         in Program.clear program
            ; program
         end
   else program
end
