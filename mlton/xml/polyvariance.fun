(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(*
 * Duplicate a let bound function at each variable reference
 * if cost is smaller than threshold.
 * 
 *)
functor Polyvariance (S: XML_TRANSFORM_STRUCTS): XML_TRANSFORM = 
struct

open S
datatype z = datatype Dec.t
datatype z = datatype PrimExp.t

structure Type =
   struct
      open Type

      fun containsArrow t = containsTycon (t, Tycon.arrow)

      fun isHigherOrder t =
         case deArrowOpt t of
            NONE => false
          | SOME (t1, t2) => containsArrow t1 orelse isHigherOrder t2

(*
      val isHigherOrder =
         Trace.trace 
         ("Polyvariance.isHigherOrder", layout, Bool.layout) 
         isHigherOrder
*)

   end

fun lambdaSize (Program.T {body, ...}): Lambda.t -> int =
   let
      val {get = size: Lambda.t -> int, set, ...} =
         Property.getSetOnce (Lambda.plist,
                              Property.initRaise ("size", Lambda.layout))
      fun loopExp (e: Exp.t, n: int): int =
         List.fold
         (Exp.decs e, n, fn (d, n) =>
          case d of
             MonoVal {exp, ...} => loopPrimExp (exp, n + 1)
           | PolyVal {exp, ...} => loopExp (exp, n + 1)
           | Fun {decs, ...} => Vector.fold (decs, n, fn ({lambda, ...}, n) =>
                                             loopLambda (lambda, n))
           | Exception _ => n + 1)
      and loopLambda (l: Lambda.t, n): int =
         let val m = loopExp (Lambda.body l, 0)
         in set (l, m); m + n
         end
      and loopPrimExp (e: PrimExp.t, n: int): int =
         case e of
            Case {cases, default, ...} =>
               let
                  val n = n + 1
               in
                  Cases.fold
                  (cases,
                   (case default of
                       NONE => n
                     | SOME (e, _) => loopExp (e, n)),
                       fn (e, n) => loopExp (e, n))
               end
          | Handle {try, handler, ...} =>
               loopExp (try, loopExp (handler, n + 1))
          | Lambda l => loopLambda (l, n + 1)
          | Profile _ => n
          | _ => n + 1
      val _ = loopExp (body, 0)
   in 
      size
   end

fun shouldDuplicate (program as Program.T {body, ...}, hofo, small, product)
   : Var.t -> bool =
   let
      val costs: (Var.t * int * int * int) list ref = ref []
      val lambdaSize = lambdaSize program
      fun isOK (var: Var.t, size: int, numOccurrences: int): bool =
         let val cost = (numOccurrences - 1) * (size - small)
         in List.push (costs, (var, size, numOccurrences, cost))
            ; cost <= product
         end
      type info = {numOccurrences: int ref,
                   shouldDuplicate: bool ref}
      val {get = varInfo: Var.t -> info option, set = setVarInfo, ...} =
         Property.getSetOnce (Var.plist, Property.initConst NONE)
      fun new {lambda = _, ty, var}: unit =
         if not hofo orelse Type.isHigherOrder ty
            then setVarInfo (var, SOME {numOccurrences = ref 0,
                                        shouldDuplicate = ref false})
         else ()
      fun loopExp (e: Exp.t, numDuplicates: int): unit =
         let
            fun loopVar (x: VarExp.t): unit =
               case varInfo (VarExp.var x) of
                  NONE => ()
                | SOME {numOccurrences, ...} =>
                     numOccurrences := !numOccurrences + numDuplicates
            fun loopVars xs = Vector.foreach (xs, loopVar)
            val {decs, result} = Exp.dest e
            val rec loopDecs =
               fn [] => loopVar result
                | dec :: decs =>
                     case dec of
                        MonoVal {var, ty, exp} =>
                           (case exp of
                               Lambda l =>
                                  (new {var = var, ty = ty, lambda = l}
                                   ; loopDecs decs
                                   ; let
                                        val body = Lambda.body l
                                        val numDuplicates =
                                           case varInfo var of
                                              NONE => numDuplicates
                                               | SOME {numOccurrences,
                                                       shouldDuplicate} =>
                                                 if isOK (var, lambdaSize l,
                                                          !numOccurrences)
                                                    then (shouldDuplicate := true
                                                          ; !numOccurrences)
                                                 else numDuplicates
                                     in loopExp (body, numDuplicates)
                                     end)
                             | _ =>
                                  let
                                     val loopExp =
                                        fn e => loopExp (e, numDuplicates)
                                     val _ =
                                        case exp of
                                           App {func, arg} =>
                                              (loopVar func; loopVar arg)
                                         | Case {test, cases, default} =>
                                              (loopVar test
                                               ; Cases.foreach (cases, loopExp)
                                               ; (Option.app
                                                  (default, loopExp o #1)))
                                         | ConApp {arg, ...} =>
                                              Option.app (arg, loopVar)
                                         | Const _ => ()
                                         | Handle {try, handler, ...} =>
                                              (loopExp try; loopExp handler)
                                         | Lambda _ =>
                                              Error.bug "Polyvariance.loopExp.loopDecs: unexpected Lambda"
                                         | PrimApp {args, ...} => loopVars args
                                         | Profile _ => ()
                                         | Raise {exn, ...} => loopVar exn
                                         | Select {tuple, ...} => loopVar tuple
                                         | Tuple xs => loopVars xs
                                         | Var x => loopVar x
                                  in
                                     loopDecs decs
                                  end)
                      | Fun {decs = lambdas, ...} =>
                           let
                              val _ = (Vector.foreach (lambdas, new)
                                       ; loopDecs decs)
                              val dups =
                                 Vector.fold
                                 (lambdas, [], fn ({var, lambda, ...}, dups) =>
                                  let val body = Lambda.body lambda
                                  in case varInfo var of
                                     NONE =>
                                        (loopExp (body, numDuplicates); dups)
                                   | SOME info =>
                                        {body = body,
                                         size = lambdaSize lambda,
                                         info = info} :: dups
                                  end)
                           in case dups of
                              [] => ()
                            | _ => 
                                 let
                                    val size =
                                       List.fold
                                       (dups, 0, fn ({size, ...}, n) => n + size)
                                    val numOccurrences =
                                       List.fold
                                       (dups, 0,
                                        fn ({info = {numOccurrences, ...}, ...},
                                            n) => n + !numOccurrences)
                                 in if isOK (if Vector.isEmpty lambdas
                                                then Error.bug "Polyvariance.loopExp.loopDecs: empty lambdas"
                                             else
                                                #var (Vector.first lambdas),
                                             size, numOccurrences)
                                       then (List.foreach
                                             (dups,
                                              fn {body,
                                                  info = {shouldDuplicate, ...},
                                                  ...} =>
                                              (shouldDuplicate := true
                                               ; loopExp (body, numOccurrences))))
                                    else
                                       List.foreach
                                       (dups, fn {body, ...} =>
                                        loopExp (body, numDuplicates))
                                 end
                           end
                      | _ => Error.bug "Polyvariance.loopExp.loopDecs: strange dec"
         in loopDecs decs
         end
      val _ = loopExp (body, 1)
      fun sort l =
         List.insertionSort (l, fn ((_, _, _, c), (_, _, _, c')) => c < c')
      val _ =
         Control.diagnostics
         (fn layout => 
          List.foreach
          (sort (!costs), fn (x, size, numOcc, c) =>
           layout (let open Layout
                   in seq [Var.layout x,
                           str " ", Int.layout size,
                           str " ", Int.layout numOcc,
                           str " ", Int.layout c]
                   end)))
   in
      fn x =>
      case varInfo x of
         NONE => false
       | SOME {shouldDuplicate, ...} => !shouldDuplicate
   end

fun transform (program as Program.T {datatypes, body, overflow},
               hofo: bool,
               small: int,
               product: int) =
   let
      val shouldDuplicate = shouldDuplicate (program, hofo, small, product)
      datatype info =
         Replace of Var.t
       | Dup of {
                 duplicates: Var.t list ref
                 }
      val {get = varInfo: Var.t -> info, set = setVarInfo, ...} =
         Property.getSet (Var.plist,
                          Property.initRaise ("Polyvariance.info", Var.layout))
      fun loopVar (x: VarExp.t): VarExp.t =
         VarExp.mono
         (let val x = VarExp.var x
          in case varInfo x of
             Replace y => y
           | Dup {duplicates, ...} =>
                let val x' = Var.new x
                in List.push (duplicates, x')
                   ; x'
                end
          end)
      fun loopVars xs = Vector.map (xs, loopVar)
      fun bind (x: Var.t): Var.t =
         let val x' = Var.new x
         in setVarInfo (x, Replace x')
            ; x'
         end
      fun bindVarType (x, t) = (bind x, t)
      fun bindPat (Pat.T {con, targs, arg}) =
         Pat.T {con = con,
                targs = targs,
                arg = Option.map (arg, bindVarType)}
      fun new {lambda = _, ty = _, var}: unit =
         if shouldDuplicate var
            then setVarInfo (var, Dup {duplicates = ref []})
         else ignore (bind var)
      fun loopExp (e: Exp.t): Exp.t =
         let
            val {decs, result} = Exp.dest e
         in
            Exp.make (loopDecs (decs, result))
         end
      and loopLambda (l: Lambda.t): Lambda.t =
         let
            val {arg, argType, body, mayInline} = Lambda.dest l
         in
            Lambda.make {arg = bind arg,
                         argType = argType,
                         body = loopExp body,
                         mayInline = mayInline}
         end
      and loopDecs (ds: Dec.t list, result): {decs: Dec.t list,
                                              result: VarExp.t} =
         case ds of
            [] => {decs = [], result = loopVar result}
          | d :: ds =>
               case d of
                  MonoVal {var, ty, exp} =>
                     (case exp of
                         Lambda l =>
                            let
                               val _ = new {var = var, ty = ty, lambda = l}
                               val {decs, result} = loopDecs (ds, result)
                               val decs =
                                  case varInfo var of
                                     Replace var =>
                                        MonoVal {var = var, ty = ty,
                                                 exp = Lambda (loopLambda l)}
                                        :: decs
                                   | Dup {duplicates, ...} =>
                                        List.fold
                                        (!duplicates, decs, fn (var, decs) =>
                                         MonoVal {var = var, ty = ty,
                                                  exp = Lambda (loopLambda l)}
                                         :: decs)
                            in {decs = decs, result = result}
                            end
                       | _ => 
                            let
                               val exp =
                                  case exp of
                                     App {func, arg} =>
                                        App {func = loopVar func,
                                             arg = loopVar arg}
                                   | Case {test, cases, default} =>
                                        let
                                           datatype z = datatype Cases.t
                                           val cases =
                                              case cases of
                                                 Con cases =>
                                                    Con
                                                    (Vector.map
                                                     (cases, fn (p, e) =>
                                                      (bindPat p, loopExp e)))
                                               | Word (s, v) =>
                                                    Word
                                                    (s, (Vector.map
                                                         (v, fn (z, e) =>
                                                          (z, loopExp e))))
                                        in
                                           Case {test = loopVar test,
                                                 cases = cases,
                                                 default =
                                                 Option.map
                                                 (default, fn (e, r) =>
                                                  (loopExp e, r))}
                                        end
                                   | ConApp {con, targs, arg} =>
                                        ConApp {con = con,
                                                targs = targs,
                                                arg = Option.map (arg, loopVar)}
                                   | Const _ => exp
                                   | Handle {try, catch, handler} =>
                                        Handle {try = loopExp try,
                                                catch = bindVarType catch,
                                                handler = loopExp handler}
                                   | Lambda _ =>
                                        Error.bug "Polyvariance.loopDecs: unexpected Lambda"
                                   | PrimApp {prim, targs, args} =>
                                        PrimApp {prim = prim,
                                                 targs = targs,
                                                 args = loopVars args}
                                   | Profile _ => exp
                                   | Raise {exn, extend} =>
                                        Raise {exn = loopVar exn,
                                               extend = extend}
                                   | Select {tuple, offset} =>
                                        Select {tuple = loopVar tuple,
                                                offset = offset}
                                   | Tuple xs => Tuple (loopVars xs)
                                   | Var x => Var (loopVar x)
                               val var = bind var
                               val {decs, result} = loopDecs (ds, result)
                            in {decs = (MonoVal {var = var, ty = ty, exp = exp}
                                        :: decs),
                                result = result}
                            end)
                | Fun {decs, ...} =>
                     let
                        val _ = Vector.foreach (decs, new)
                        val {decs = ds, result} = loopDecs (ds, result)
                        val ac =
                           ref [Vector.keepAllMap
                                (decs, fn {var, ty, lambda} =>
                                 case varInfo var of
                                    Replace var =>
                                       SOME {var = var, ty = ty,
                                             lambda = loopLambda lambda}
                                  | Dup _ => NONE)]
                        val dups =
                           Vector.keepAllMap
                           (decs, fn dec as {var, ...} =>
                            case varInfo var of
                               Replace _ => NONE
                             | Dup {duplicates, ...} => SOME (dec, !duplicates))
                        val _ =
                           Vector.foreach
                           (dups, fn ({var, ...}, duplicates) =>
                            List.foreach
                            (duplicates, fn var' =>
                             let
                                val vars =
                                   Vector.map
                                   (dups, fn ({var = var'', ...}, _) =>
                                    if Var.equals (var, var'')
                                       then (setVarInfo (var, Replace var')
                                             ; var')
                                    else bind var'')
                             in List.push
                                (ac,
                                 Vector.map2
                                 (dups, vars,
                                  fn (({ty, lambda, ...}, _), var) =>
                                  {var = var, ty = ty,
                                   lambda = loopLambda lambda}))
                             end))
                        val decs = Vector.concat (!ac)
                     in {decs = Fun {tyvars = Vector.new0 (),
                                     decs = decs} :: ds,
                         result = result}
                     end
                | _ => Error.bug "Polyvariance.loopDecs: saw bogus dec"
      val body = loopExp body
      val overflow =
         Option.map (overflow, fn x =>
                     case varInfo x of
                        Replace y => y
                      | _ => Error.bug "Polyvariance.duplicate: duplicating Overflow?")
      val program =
         Program.T {datatypes = datatypes,
                    body = body,
                    overflow = overflow}
      val _ = Program.clear program
   in
      program
   end

val transform =
   fn p =>
   case !Control.polyvariance of
      NONE => p
    | SOME {hofo, rounds, small, product} =>
         let
            fun loop (p, n) =
               if n = rounds
                  then p
               else let
                       val p =
                          Control.pass
                          {display = Control.Layouts Program.layouts,
                           name = "duplicate" ^ (Int.toString (n + 1)),
                           stats = Program.layoutStats,
                           style = Control.No,
                           suffix = "post.xml",
                           thunk = fn () => shrink (transform (p, hofo, small, product))}
                    in
                       loop (p, n + 1)
                    end
         in loop (p, 0)
         end

end
