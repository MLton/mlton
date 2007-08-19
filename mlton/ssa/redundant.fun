(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Redundant (S: REDUNDANT_STRUCTS): REDUNDANT = 
struct

open S

type int = Int.t

datatype z = datatype Exp.t
datatype z = datatype Transfer.t

structure Element:
   sig
      structure Class:
         sig
            type t

            val plist: t -> PropertyList.t
         end

      type t

      val class: t -> Class.t
      val fixedPoint: unit -> unit
      val forceDistinct: t vector -> unit
      val new: 'a vector * ('a -> PropertyList.t) -> t vector
      val new1: unit -> t
      val refine: {coarse: t, fine: t} vector -> unit
   end =
   struct
      datatype t = T of {class: class ref}
      and class = Class of {coarserThan: refinement list ref,
                            elements: t vector,
                            plist: PropertyList.t}
      withtype refinement = {coarse: t, fine: t} vector

      structure Element =
         struct
            datatype t = datatype t
         end

      structure Class =
         struct
            datatype t = datatype class

            local
               fun make f (Class r) = f r
            in
               val coarserThan = make #coarserThan
               val plist = make #plist
            end

            fun new elements =
               Class {coarserThan = ref [],
                      elements = elements,
                      plist = PropertyList.new ()}

            val bogus = new (Vector.new0 ())
         end

      local
         fun make f (T r) = f r
      in
         val class = ! o make #class
      end

      fun setClass (T {class, ...}, c) = class := c

      fun 'a new (elements: 'a vector, plist: 'a -> PropertyList.t): t vector =
         let
            val classes: t list ref list ref = ref []
            val {destroy, get = class: 'a -> t list ref, ...} =
               Property.destGet
               (plist, Property.initFun (fn _ =>
                                         let
                                            val class = ref []
                                            val () = List.push (classes, class)
                                         in
                                            class
                                         end))
            val elements =
               Vector.map (elements, fn a =>
                           let
                              val elt = T {class = ref Class.bogus}
                              val () = List.push (class a, elt)
                           in
                              elt
                           end)
            val () = destroy ()
            val () = List.foreach (!classes, fn r =>
                                   let
                                      val elements = Vector.fromList (!r)
                                      val class = Class.new elements
                                      val () =
                                         Vector.foreach
                                         (elements, fn e => setClass (e, class))
                                   in
                                      ()
                                   end)
         in
            elements
         end

      fun new1 () =
         let
            val e = T {class = ref Class.bogus}
            val c = Class.new (Vector.new1 e)
            val () = setClass (e, c)
         in
            e
         end

      fun forceDistinct (es: t vector): unit =
         Vector.foreach
         (es, fn e =>
          let
             val c = Class.new (Vector.new1 e)
             val () = setClass (e, c)
          in
             ()
          end)

      structure Refinement =
         struct
            type t = refinement

            fun group (v: t, sel): t list =
               let
                  val classes = ref []
                  val {destroy, get: Class.t -> {coarse: Element.t,
                                                 fine: Element.t} list ref,
                       ...} =
                     Property.destGet
                     (Class.plist,
                      Property.initFun (fn _ =>
                                        let
                                           val r = ref []
                                           val () = List.push (classes, r)
                                        in
                                           r
                                        end))
                  val () =
                     Vector.foreach
                     (v, fn cf => List.push (get (class (sel cf)), cf))
                  val () = destroy ()
               in 
                  List.fold (!classes, [], fn (r, ac) =>
                             Vector.fromList (!r) :: ac)
               end

            fun store (v: t): unit =
               List.push (Class.coarserThan
                          (class (#coarse (Vector.sub (v, 0)))),
                          v)
         end

      val todo: Refinement.t list ref = ref []

      fun refine (v: Refinement.t): unit =
         List.foreach
         (Refinement.group (v, #fine), fn v =>
          let
             val oldClass = class (#fine (Vector.sub (v, 0)))
             val classes = Refinement.group (v, #coarse)
          in
             case classes of
                [_] => Refinement.store v
              | _ =>
                   let
                      val () =
                         todo
                         := (List.fold
                             (! (Class.coarserThan oldClass), !todo, op ::))
                   in
                      List.foreach
                      (classes, fn v =>
                       let
                          val () = Refinement.store v
                          val elements = Vector.map (v, #fine)
                          val c = Class.new elements
                          val () = Vector.foreach (elements, fn e =>
                                                   setClass (e, c))
                       in
                          ()
                       end)
                   end
          end)

      fun fixedPoint () =
         let
            fun loop () =
               case !todo of
                  [] => ()
                | r :: rs => (todo := rs
                              ; refine r
                              ; loop ())
         in
            loop ()
         end
   end

structure Class = Element.Class

structure Eqrel:>
   sig
      type t

      val classes: t -> int list list
      val element: t * int -> Element.t
      val elements: t -> Element.t vector
      val forceDistinct: t -> unit
      val fromTypes: Type.t vector -> t
      val layout: t -> Layout.t
      val make: Element.t vector -> t
      val refine: {coarse: t, fine: t} -> unit
      val unify: t * t -> unit
   end =
   struct
      datatype t = T of Element.t vector

      val make = T

      fun elements (T v) = v

      fun element (r, i) = Vector.sub (elements r, i)

      fun forceDistinct (T v) = Element.forceDistinct v

      fun fromTypes ts = T (Element.new (ts, Type.plist))

      fun refine {coarse = T cv, fine = T fv} =
         Element.refine
         (Vector.map2 (cv, fv, fn (c, f) => {coarse = c, fine = f}))

      fun unify (r, r') =
         (refine {coarse = r, fine = r'}
          ; refine {coarse = r', fine = r})

      fun classes (T v) =
         let
            val classes = ref []
            val {get = classIndices: Class.t -> int list ref, destroy, ...} =
               Property.destGet (Class.plist,
                                 Property.initFun
                                 (fn _ =>
                                  let
                                     val r = ref []
                                     val () = List.push (classes, r)
                                  in
                                     r
                                  end))
            val () =
               Vector.foreachi
               (v, fn (i, e) =>
                List.push (classIndices (Element.class e), i))
            val () = destroy ()
         in
            List.fold (!classes, [], fn (r, ac) => !r :: ac)
         end

      val layout = (List.layout (List.layout Int.layout)) o classes
   end

fun redundant (Program.T {datatypes, globals, functions, main}) =
   let
      val {get = funcInfo: Func.t -> {arg: Eqrel.t, return: Eqrel.t option},
           set = setFuncInfo, ...} =
         Property.getSetOnce
         (Func.plist, Property.initRaise ("Redundant.info", Func.layout))
      val {get = labelInfo: Label.t -> Eqrel.t,
           set = setLabelInfo, ...} =
         Property.getSetOnce
         (Label.plist, Property.initRaise ("Redundant.info", Label.layout))
      val {get = varInfo : Var.t -> Element.t,
           set = setVarInfo, ...} =
         Property.getSetOnce
         (Var.plist, Property.initFun (fn _ => Element.new1 ()))
      fun varEquiv xs = Eqrel.make (Vector.map (xs, varInfo))
      (* compute the fixed point *)
      val () =
         let
            fun makeFormalsRel (xs: (Var.t * Type.t) vector): Eqrel.t =
               let
                  val eqrel = Eqrel.fromTypes (Vector.map (xs, #2))
                  val () =
                     Vector.foreachi
                     (xs, fn (i, (x, _)) =>
                      setVarInfo (x, Eqrel.element (eqrel, i)))
               in
                  eqrel
               end
            (* initialize all varInfo and funcInfo *)
            val () =
               List.foreach
               (functions, fn f =>
                let
                   val {name, args, returns, ...} = Function.dest f
                in
                   setFuncInfo (name, {arg = makeFormalsRel args,
                                       return = Option.map (returns,
                                                            Eqrel.fromTypes)})
                end)
            (* Add the calls to all the funcInfos *)
            val () =
               List.foreach
               (functions, fn f =>
                let 
                   val {name, blocks, ...} = Function.dest f
                   val {return, ...} = funcInfo name
                   val _ =
                      Vector.foreach (blocks, fn Block.T {label, args, ...} =>
                                      setLabelInfo (label, makeFormalsRel args))
                in
                   Vector.foreach
                   (blocks, fn Block.T {transfer, ...} =>
                    case transfer of
                       Call {func, args, return = ret, ...} =>
                          let
                             val {arg = arg', return = return'} = funcInfo func
                             val _ = Eqrel.refine {coarse = varEquiv args,
                                                   fine = arg'}
                          in
                             case ret of
                                Return.Dead => ()
                              | Return.NonTail {cont, ...} =>
                                   Option.app (return', fn e =>
                                               Eqrel.unify (e, labelInfo cont))
                              | Return.Tail =>
                                   (case (return, return') of
                                       (SOME e, SOME e') => Eqrel.unify (e, e')
                                     | _ => ())
                          end
                      | Case {cases = Cases.Con cases, ...} =>
                           (* For now, assume that constructor arguments
                            * are never redundant.  Thus all case branches
                            * need to have trivial equivalence relations.
                            *)
                           Vector.foreach (cases, fn (_, l) =>
                                           Eqrel.forceDistinct (labelInfo l))

                      | Goto {dst, args, ...} =>
                           Eqrel.refine {coarse = varEquiv args,
                                         fine = labelInfo dst}
                      | Return xs =>
                           Eqrel.refine {coarse = varEquiv xs,
                                         fine = valOf return}
                      | _ => ())
                end)
            val _ = Element.fixedPoint ()
         in ()
         end
      val _ = 
         Control.diagnostics
         (fn display =>
          List.foreach
          (functions, fn f => 
           let
              open Layout
               val {name, blocks, ...} = Function.dest f
               val {arg, return} = funcInfo name
               val () =
                  display (seq [Func.layout name,
                                str "  ",
                                Eqrel.layout arg,
                                Option.layout Eqrel.layout return])
               val () =
                  Vector.foreach
                  (blocks, fn Block.T {label, ...} =>
                   let
                      val arg = labelInfo label
                   in
                      display (seq [str "\t",
                                    Label.layout label,
                                    str " ",
                                    Eqrel.layout arg])
                   end)
           in
              ()
           end))
      val {get = replacement : Var.t -> Var.t option,
           set = setReplacement, ...} =
         Property.getSetOnce (Var.plist, Property.initConst NONE)
      datatype red =
         Useful
       | Redundant of int (* the index it is the same as *)
      (* Turn an equivalence relation on 0 ... n - 1 into a red vector by
       * choosing a representative of each class.
       *)
      fun makeReds (r: Eqrel.t): red vector =
         let
            val {get = rep: Class.t -> int option ref, destroy, ...} =
               Property.destGet (Class.plist,
                                 Property.initFun (fn _ => ref NONE))
            val reds =
               Vector.mapi
               (Eqrel.elements r, fn (i, e) =>
                let
                   val r = rep (Element.class e)
                in
                   case !r of
                      NONE => (r := SOME i; Useful)
                    | SOME i => Redundant i
                end)
            val () = destroy ()
         in
            reds
         end
      fun redundantFormals (xs: (Var.t * Type.t) vector, r: Eqrel.t)
         : red vector * (Var.t * Type.t) vector =
         let
            val reds = makeReds r
            val xs =
               Vector.keepAllMap2
               (xs, reds, fn (x, red) =>
                case red of
                   Useful => SOME x
                 | Redundant i =>
                      (setReplacement (#1 x, SOME (#1 (Vector.sub (xs, i))))
                       ; NONE))
         in
            (reds, xs)
         end
      fun keepUseful (reds: red vector, xs: 'a vector): 'a vector =
         Vector.keepAllMap2 (reds, xs, fn (r, x) =>
                             case r of
                                Useful => SOME x
                              | _ => NONE)
      val {get = funcReds : Func.t -> {argsRed: red vector,
                                       args: (Var.t * Type.t) vector,
                                       returnsRed: red vector option,
                                       returns: Type.t vector option},
           set = setFuncReds, ...} =
         Property.getSetOnce (Func.plist,
                              Property.initRaise ("funcReds", Func.layout))
      val {get = labelReds: Label.t -> {argsRed: red vector,
                                        args: (Var.t * Type.t) vector},
           set = setLabelReds, ...} =
         Property.getSetOnce (Label.plist,
                              Property.initRaise ("labelReds", Label.layout))
      val _ =
         List.foreach
         (functions, fn f =>
          let
             val {name, args, blocks, returns, ...} = Function.dest f
             val {arg, return} = funcInfo name
             val (returnsRed, returns) =
                (case (returns, return) of
                    (SOME r, SOME r') =>
                       let
                          val returnsRed = makeReds r'
                          val returns = keepUseful (returnsRed, r)
                       in
                          (SOME returnsRed, SOME returns)
                       end
                  | _ => (NONE, NONE))
             val (argsRed, args) = redundantFormals (args, arg)
          in
             setFuncReds (name, {args = args,
                                 argsRed = argsRed,
                                 returns = returns,
                                 returnsRed = returnsRed}) ;
             Vector.foreach
             (blocks, fn Block.T {label, args, ...} =>
              let
                 val (argsRed, args) = redundantFormals (args, labelInfo label)
              in
                 setLabelReds (label, {args = args,
                                       argsRed = argsRed})
              end)
          end)
      fun loopVar x =
         case replacement x of
            NONE => x
          | SOME y => y
      fun loopVars xs = Vector.map (xs, loopVar)
      val functions =
         List.revMap
         (functions, fn f =>
          let
             val {blocks, mayInline, name, raises, start, ...} = Function.dest f
             val {args, returns, returnsRed, ...} = funcReds name
             val blocks =
                Vector.map
                (blocks, fn Block.T {label, statements, transfer, ...} =>
                 let
                    val {args, ...} = labelReds label
                    val statements =
                       Vector.map
                       (statements, fn Statement.T {var, ty, exp} =>
                        Statement.T {var = var,
                                     ty = ty,
                                     exp = Exp.replaceVar (exp, loopVar)})
                    val transfer =
                       case transfer of
                          Arith {prim, args, overflow, success, ty} =>
                             Arith {prim = prim,
                                    args = loopVars args,
                                    overflow = overflow,
                                    success = success,
                                    ty = ty}
                        | Bug => Bug
                        | Call {func, args, return} =>
                             Call {func = func, 
                                   args = loopVars (keepUseful 
                                                    (#argsRed (funcReds func),
                                                     args)),
                                   return = return}
                        | Case {test, cases, default} =>
                             Case {test = loopVar test, 
                                   cases = cases,
                                   default = default}
                        | Goto {dst, args} =>
                             Goto {dst = dst,
                                   args = loopVars (keepUseful 
                                                    (#argsRed (labelReds dst), 
                                                     args))}
                        | Raise xs => Raise (loopVars xs)
                        | Return xs =>
                             Return (loopVars
                                     (keepUseful (valOf returnsRed, xs)))
                        | Runtime {prim, args, return} =>
                             Runtime {prim = prim,
                                      args = loopVars args,
                                      return = return}
                 in
                    Block.T {label = label,
                             args = args,
                             statements = statements,
                             transfer = transfer}
                 end)
             val f = Function.new {args = args,
                                   blocks = blocks,
                                   mayInline = mayInline,
                                   name = name,
                                   raises = raises,
                                   returns = returns,
                                   start = start}
             val _ = Function.clear f
          in
             f
          end)
      val p = Program.T {datatypes = datatypes,
                         globals = globals,
                         functions = functions,
                         main = main}
      val _ = Program.clearTop p
   in
      p
   end

end
