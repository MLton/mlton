(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* This simplifier is based on the following article.
 *   Shrinking Lambda Expressions in Linear Time.
 *   Journal of Functional Programming. Vol 7, no 5, 1997.
 *)

functor Shrink (S: SHRINK_STRUCTS): SHRINK = 
struct

open S
open Dec PrimExp

type int = Int.t

val traceShrinkExp =
   Trace.trace ("Xml.Shrink.shrinkExp", Exp.layout, Exp.layout)

val traceShrinkLambda =
   Trace.trace ("Xml.Shrink.shrinkLambda", Lambda.layout, Lambda.layout)

fun inc (r: int ref, n) =
   let val n = !r + n
   in Assert.assert ("Xml.Shrink.inc", fn () => n >= 0)
      ; r := n
   end

structure VarInfo =
   struct
      datatype t =
         Mono of monoVarInfo
       | Poly of VarExp.t
      and value =
         ConApp of {con: Con.t,
                    targs: Type.t vector,
                    arg: t option}
       | Const of Const.t
       | Lambda of {isInlined: bool ref,
                    lam: Lambda.t}
       | Tuple of t vector
      withtype monoVarInfo = {numOccurrences: int ref,
                              value: value option ref,
                              varExp: VarExp.t}

      local
         open Layout
      in
         val rec layout =
            fn Mono {numOccurrences, value, varExp} =>
                 record [("numOccurrences", Int.layout (!numOccurrences)),
                         ("value", Option.layout layoutValue (!value)),
                         ("varExp", VarExp.layout varExp)]
             | Poly x => seq [str "Poly ", VarExp.layout x]
         and layoutValue =
            fn ConApp {con, arg, ...} =>
                 seq [Con.layout con,
                      case arg of
                         NONE => empty
                       | SOME i => paren (layout i)]
             | Const c => Const.layout c
             | Lambda {isInlined, ...} =>
                  seq [str "Lambda ", Bool.layout (!isInlined)]
             | Tuple is => Vector.layout layout is
      end

      val inc =
         fn (i, n) =>
         case i of
            Mono {numOccurrences = r, ...} => inc (r, n)
          | Poly _ => ()

      val inc =
         Trace.trace2 ("Xml.Shrink.VarInfo.inc", layout, Int.layout, Unit.layout) inc

      fun delete i = inc (i, ~1)

      val delete = Trace.trace ("Xml.Shrink.VarInfo.delete", layout, Unit.layout) delete

      fun deletes is = Vector.foreach (is, delete)

      val varExp =
         fn Mono {varExp, ...} => varExp
          | Poly x => x
   end

structure InternalVarInfo =
   struct
      datatype t =
         VarInfo of VarInfo.t
       | Self

      val layout =
         fn VarInfo i => VarInfo.layout i
          | Self => Layout.str "self"
   end

structure MonoVarInfo =
   struct
      type t = VarInfo.monoVarInfo
   end

structure Value =
   struct
      datatype t = datatype VarInfo.value

      fun toPrimExp v =
         case v of
            ConApp {con, targs, arg} =>
               PrimExp.ConApp {con = con,
                               targs = targs,
                               arg = Option.map (arg, VarInfo.varExp)}
          | Const c => PrimExp.Const c
          | Lambda {lam, ...} => PrimExp.Lambda lam
          | Tuple vs => PrimExp.Tuple (Vector.map (vs, VarInfo.varExp))
   end

fun shrinkOnce (Program.T {datatypes, body, overflow}) =
   let
      (* Keep track of the number of constuctors in each datatype so that
       * we can eliminate redundant defaults.
       *)
      val {get = conNumCons: Con.t -> int , set = setConNumCons, ...} =
         Property.getSetOnce (Con.plist, Property.initConst ~1)
      val _ =
         Vector.foreach
         (datatypes, fn {cons, ...} =>
          let
             val n = Vector.length cons
          in
             Vector.foreach (cons, fn {con, ...} => setConNumCons (con, n))
          end)
      fun isExhaustive (cases: exp Cases.t): bool =
         case cases of
            Cases.Con v =>
               (0 < Vector.length v
                andalso (Vector.length v
                         = conNumCons (Pat.con (#1 (Vector.sub (v, 0))))))
          | _ => false
      val {get = varInfo: Var.t -> InternalVarInfo.t, set = setVarInfo, ...} =
         Property.getSet (Var.plist,
                          Property.initRaise ("shrink varInfo", Var.layout))
      val setVarInfo =
         Trace.trace2 ("Xml.Shrink.setVarInfo",
                       Var.layout, InternalVarInfo.layout, Unit.layout)
         setVarInfo
      val varInfo =
         Trace.trace ("Xml.Shrink.varInfo", Var.layout, InternalVarInfo.layout)
         varInfo
      fun monoVarInfo x =
         case varInfo x of
            InternalVarInfo.VarInfo (VarInfo.Mono i) => i
          | _ => Error.bug "Xml.Shrink.monoVarInfo"
      fun varExpInfo (x as VarExp.T {var, ...}): VarInfo.t =
         case varInfo var of
            InternalVarInfo.Self => VarInfo.Poly x
          | InternalVarInfo.VarInfo i => i
      val varExpInfo =
         Trace.trace ("Xml.Shrink.varExpInfo", VarExp.layout, VarInfo.layout) varExpInfo
      fun varExpInfos xs = Vector.map (xs, varExpInfo)
      fun replaceInfo (x: Var.t,
                       {numOccurrences = r, ...}: MonoVarInfo.t,
                       i: VarInfo.t): unit =
         (VarInfo.inc (i, !r)
          ; setVarInfo (x, InternalVarInfo.VarInfo i))
      val replaceInfo =
         Trace.trace ("Xml.Shrink.replaceInfo",
                      fn (x, _, i) => Layout.tuple [Var.layout x,
                                                    VarInfo.layout i],
                      Unit.layout)
         replaceInfo
      fun replace (x, i) = replaceInfo (x, monoVarInfo x, i)
      val shrinkVarExp = VarInfo.varExp o varExpInfo
      fun shrinkVarExps xs = Vector.map (xs, shrinkVarExp)
      local
         fun handleBoundVar (x, ts, _) =
            setVarInfo (x,
                        if Vector.isEmpty ts
                           then (InternalVarInfo.VarInfo
                                 (VarInfo.Mono {numOccurrences = ref 0,
                                                value = ref NONE,
                                                varExp = VarExp.mono x}))
                        else InternalVarInfo.Self)
         fun handleVarExp x = VarInfo.inc (varExpInfo x, 1)
      in
         fun countExp (e: Exp.t): unit =
            Exp.foreach {exp = e,
                         handleBoundVar = handleBoundVar,
                         handleExp = fn _ => (),
                         handlePrimExp = fn _ => (),
                         handleVarExp = handleVarExp}
      end
      fun deleteVarExp (x: VarExp.t): unit =
         VarInfo.inc (varExpInfo x, ~1)
      fun deleteExp (e: Exp.t): unit = Exp.foreachVarExp (e, deleteVarExp)
      val deleteExp =
         Trace.trace ("Xml.Shrink.deleteExp", Exp.layout, Unit.layout) deleteExp
      fun deleteLambda l = deleteExp (Lambda.body l)
      (*---------------------------------------------------*)
      (*                    shrinkExp                    *)
      (*---------------------------------------------------*)
      fun shrinkExp arg: Exp.t =
         traceShrinkExp
         (fn (e: Exp.t) =>
          let
             val {decs, result} = Exp.dest e
          in
             Exp.make {decs = shrinkDecs decs,
                       result = shrinkVarExp result}
          end) arg
      and shrinkDecs (decs: Dec.t list): Dec.t list =
         case decs of
            [] => []
          | dec :: decs =>
               case dec of
                  Exception _ => dec :: shrinkDecs decs
                | PolyVal {var, tyvars, ty, exp} =>
                     Dec.PolyVal {var = var, tyvars = tyvars, ty = ty,
                                  exp = shrinkExp exp}
                     :: shrinkDecs decs
                | Fun {tyvars, decs = decs'} =>
                     if Vector.isEmpty tyvars
                        then
                           let
                              val decs' =
                                 Vector.keepAll
                                 (decs', fn {lambda, var, ...} =>
                                  let
                                     val {numOccurrences, value, ...} =
                                        monoVarInfo var
                                  in if 0 = !numOccurrences
                                        then (deleteLambda lambda; false)
                                     else (value := (SOME
                                                     (Value.Lambda
                                                      {isInlined = ref false,
                                                       lam = lambda}))
                                           ; true)
                                  end)
                              val decs = shrinkDecs decs
                              (* Need to walk over all the decs and remove
                               * their value before shrinking any of them
                               * because they are mutually recursive.
                               *)
                              val decs' =
                                 Vector.keepAll
                                 (decs', fn {var, lambda, ...} =>
                                  let
                                     val {numOccurrences, value, ...} =
                                        monoVarInfo var
                                  in
                                     case !value of
                                        SOME (Value.Lambda {isInlined, ...}) =>
                                           not (!isInlined)
                                           andalso
                                           if 0 = !numOccurrences
                                              then (deleteLambda lambda
                                                    ; false)
                                           else (value := NONE; true)
                                      | _ => Error.bug "Xml.Shrink.shrinkDecs: should be a lambda"
                                  end)
                           in
                              if Vector.isEmpty decs'
                                 then decs
                              else
                                 Dec.Fun {tyvars = tyvars,
                                          decs =
                                          Vector.map
                                          (decs', fn {var, ty, lambda} =>
                                           {var = var,
                                            ty = ty,
                                            lambda = shrinkLambda lambda})}
                                 :: decs
                           end
                     else
                        Dec.Fun {tyvars = tyvars,
                                 decs =
                                 Vector.map
                                 (decs', fn {var, ty, lambda} =>
                                  {var = var,
                                   ty = ty,
                                   lambda = shrinkLambda lambda})}
                        :: shrinkDecs decs
                | MonoVal b =>
                     shrinkMonoVal (b, fn () => shrinkDecs decs)
      and shrinkMonoVal ({var, ty, exp},
                         rest: unit -> Dec.t list) =
         let
            val info as {numOccurrences, value, ...} = monoVarInfo var
            fun finish (exp, decs) =
               MonoVal {var = var, ty = ty, exp = exp} :: decs
            fun nonExpansive (delete: unit -> unit,
                              set: unit -> (unit -> PrimExp.t) option) =
               if 0 = !numOccurrences
                  then (delete (); rest ())
               else let
                       val s = set ()
                       val decs = rest ()
                    in if 0 = !numOccurrences
                          then (delete (); decs)
                       else (case s of
                                NONE => decs
                              | SOME n => finish (n (), decs))
                    end
            fun expansive (e: PrimExp.t) = finish (e, rest ())
            fun nonExpansiveCon (delete, v: Value.t) =
               nonExpansive
               (delete,
                fn () => (value := SOME v
                          ; SOME (fn () => Value.toPrimExp v)))
            fun expression (e: Exp.t): Dec.t list =
               let
                  val {decs = decs', result} = Exp.dest (shrinkExp e)
                  val _ = replaceInfo (var, info, varExpInfo result)
                  val decs = rest ()
               in decs' @ decs
               end
         in
            case exp of
               App {func, arg} =>
                  let
                     val arg = varExpInfo arg
                     fun normal func =
                        expansive (App {func = func,
                                        arg = VarInfo.varExp arg})
                  in case varExpInfo func of
                     VarInfo.Poly x => normal x
                   | VarInfo.Mono {numOccurrences, value, varExp, ...} => 
                        case (!numOccurrences, !value) of
                           (1, SOME (Value.Lambda {isInlined, lam = l})) =>
                              if not (Lambda.mayInline l)
                                 then normal varExp
                              else
                                 let
                                    val {arg = form, body, ...} = Lambda.dest l
                                 in
                                    VarInfo.inc (arg, ~1)
                                    ; replace (form, arg)
                                    ; isInlined := true
                                    ; numOccurrences := 0
                                    ; expression body
                                 end
                         | _ => normal varExp
                  end
             | Case {test, cases, default} =>
                  let
                     fun match (cases, f): Dec.t list =
                        let
                           val _ = deleteVarExp test
                           fun step (i, (c, e), ()) =
                              if f c
                                 then
                                    (Vector.foreachR (cases, i + 1,
                                                      Vector.length cases,
                                                      deleteExp o #2)
                                     ; Option.app (default, deleteExp o #1)
                                     ; Vector.Done (expression e))
                              else (deleteExp e; Vector.Continue ())
                           fun done () =
                              case default of
                                 SOME (e, _) => expression e
                               | NONE => Error.bug "Xml.Shrink.shrinkMonoVal: Case, match"
                        in Vector.fold' (cases, 0, (), step, done)
                        end
                     fun normal test =
                        let
                           (* Eliminate redundant default case. *)
                           val default =
                              if isExhaustive cases
                                 then (Option.app (default, deleteExp o #1)
                                       ; NONE)
                              else Option.map (default, fn (e, r) =>
                                               (shrinkExp e, r))
                        in
                           expansive
                           (Case {test = test,
                                  cases = Cases.map (cases, shrinkExp),
                                  default = default})
                        end
                  in
                     case varExpInfo test of
                        VarInfo.Poly test => normal test
                      | VarInfo.Mono {value, varExp, ...} => 
                           case (cases, !value) of
                              (Cases.Con cases,
                               SOME (Value.ConApp {con = c, arg, ...})) =>
                              let
                                 val match =
                                    fn f =>
                                    match (cases,
                                           fn Pat.T {con = c', arg, ...} =>
                                           Con.equals (c, c')
                                           andalso f arg)
                              in case arg of
                                 NONE => match Option.isNone
                               | SOME v =>
                                    match
                                    (fn SOME (x, _) => (replace (x, v); true)
                                  | _ => false)
                              end
                             | (_, SOME (Value.Const c)) =>
                                  (case (cases, c) of
                                      (Cases.Word (_, l), Const.Word w) =>
                                         match (l, fn w' => WordX.equals (w, w'))
                                    | _ => Error.bug "Xml.Shrink.shrinkMonoVal: Case, strange case")
                             | (_, NONE) => normal varExp
                             | _ => Error.bug "Xml.Shrink.shrinkMonoVal: Case, default"
                  end
             | ConApp {con, targs, arg} =>
                  if Con.equals (con, Con.overflow)
                     then
                        expansive
                        (ConApp
                         {con = con,
                          targs = targs,
                          arg = Option.map (arg, shrinkVarExp)})
                  else
                     let
                        val arg = Option.map (arg, varExpInfo)
                     in nonExpansiveCon
                        (fn () => Option.app (arg, VarInfo.delete),
                         Value.ConApp {con = con, targs = targs, arg = arg})
                     end                             
             | Const c => nonExpansiveCon (fn () => (), Value.Const c)
             | Handle {try, catch, handler} =>
                  expansive (Handle {try = shrinkExp try,
                                     catch = catch,
                                     handler = shrinkExp handler})
             | Lambda l =>
                  let val isInlined = ref false
                  in nonExpansive
                     (fn () => if !isInlined then () else deleteLambda l,
                      fn () => (value := SOME (Value.Lambda
                                               {isInlined = isInlined,
                                                lam = l})
                                ; SOME (fn () => Lambda (shrinkLambda l))))
                  end
             | PrimApp {prim, args, targs} =>
                  let
                     fun make () =
                        PrimApp {prim = prim, targs = targs,
                                 args = shrinkVarExps args}
                  in if Prim.maySideEffect prim
                        then expansive (make ())
                     else nonExpansive (fn () => (), fn () => SOME make)
                  end
             | Profile _ => expansive exp
             | Raise {exn, extend} =>
                  expansive (Raise {exn = shrinkVarExp exn, extend = extend})
             | Select {tuple, offset} =>
                  let
                     fun normal x = Select {tuple = x, offset = offset}
                  in case varExpInfo tuple of
                     VarInfo.Poly x => finish (normal x, rest ())
                   | VarInfo.Mono {numOccurrences, value, varExp, ...} =>
                        nonExpansive
                        (fn () => inc (numOccurrences, ~1),
                         fn () =>
                         case !value of
                            NONE => SOME (fn () => normal varExp)
                          | SOME (Value.Tuple vs) => 
                               (inc (numOccurrences, ~1)
                                ; replaceInfo (var, info, Vector.sub (vs, offset))
                                ; NONE)
                          | _ => Error.bug "Xml.Shrink.shrinkMonoVal: Select")
                  end
             | Tuple xs =>
                  let val xs = varExpInfos xs
                  in nonExpansiveCon (fn () => VarInfo.deletes xs,
                                      Value.Tuple xs)
                  end
             | Var x => let val x = varExpInfo x
                        in replaceInfo (var, info, x)
                           ; VarInfo.inc (x, ~1)
                           ; rest ()
                        end
         end
      and shrinkLambda l: Lambda.t =
         traceShrinkLambda
         (fn l => 
          let
             val {arg, argType, body, mayInline} = Lambda.dest l
          in
             Lambda.make {arg = arg,
                          argType = argType,
                          body = shrinkExp body,
                          mayInline = mayInline}
          end) l
      val _ = countExp body
      val _ =
         Option.app
         (overflow, fn x =>
          case varInfo x of
             InternalVarInfo.VarInfo i => VarInfo.inc (i, 1)
           | _ => Error.bug "Xml.Shrink.shrinkOnce: strange overflow var")
      val body = shrinkExp body
      (* Must lookup the overflow variable again because it may have been set
       * during shrinking.
       *)
      val overflow =
         Option.map
         (overflow, fn x =>
          case varInfo x of
             InternalVarInfo.VarInfo i => VarExp.var (VarInfo.varExp i)
           | _ => Error.bug "Xml.Shrink.shrinkOnce: strange overflow var")
      val _ = Exp.clear body
      val _ = Vector.foreach (datatypes, fn {cons, ...} =>
                              Vector.foreach (cons, Con.clear o #con))
   in
      Program.T {datatypes = datatypes,
                 body = body,
                 overflow = overflow}
   end

val shrinkOnce =
   Trace.trace ("Xml.Shrink.shrinkOnce", Program.layout, Program.layout) shrinkOnce

val shrink = shrinkOnce o shrinkOnce

structure SccFuns = SccFuns (S)

val shrink = shrink o SccFuns.sccFuns

val shrink =
   Trace.trace ("Xml.Shrink.shrink", Program.layout, Program.layout) shrink

end
