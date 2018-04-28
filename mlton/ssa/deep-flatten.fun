(* Copyright (C) 2009,2017 Matthew Fluet.
 * Copyright (C) 2004-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor DeepFlatten (S: SSA2_TRANSFORM_STRUCTS): SSA2_TRANSFORM = 
struct

open S

datatype z = datatype Exp.t
datatype z = datatype Statement.t
datatype z = datatype Transfer.t

structure Tree = Tree (structure Seq = Prod)

structure TypeTree =
   struct
      datatype t = datatype Tree.t

      datatype info =
         Flat
       | NotFlat of {ty: Type.t,
                     var: Var.t option}

      type t = info Tree.t

      fun layout (t: t): Layout.t =
         Tree.layout
         (t,
          let
             open Layout
          in
             fn Flat => str "Flat"
              | NotFlat {ty, var} =>
                   seq [str "NotFlat ",
                        record [("ty", Type.layout ty),
                                ("var", Option.layout Var.layout var)]]
          end)

      val isFlat: t -> bool =
         fn T (i, _) =>
         case i of
            Flat => true
          | NotFlat _ => false
   end

structure VarTree =
   struct
      open TypeTree

      val labelRoot: t * Var.t -> t =
         fn (t as T (info, ts), x) =>
         case info of
            Flat => t
          | NotFlat {ty, ...} => T (NotFlat {ty = ty, var = SOME x}, ts)

      val fromTypeTree: TypeTree.t -> t = fn t => t

      val foldRoots: t * 'a * (Var.t * 'a -> 'a) -> 'a =
         fn (t, a, f) =>
         let
            fun loop (T (info, children), a: 'a): 'a =
               case info of
                  Flat => Prod.fold (children, a, loop)
                | NotFlat {var, ...} =>
                     case var of
                        NONE => Error.bug "DeepFlatten.VarTree.foldRoots"
                      | SOME x => f (x, a)
         in
            loop (t, a)
         end

      fun foreachRoot (t, f) = foldRoots (t, (), f o #1)

      val rootsOnto: t * Var.t list -> Var.t list =
         fn (t, ac) =>
         List.appendRev (foldRoots (t, [], op ::), ac)

      val rec dropVars: t -> t =
         fn T (info, ts) =>
         let
            val info =
               case info of
                  Flat => Flat
                | NotFlat {ty, ...} => NotFlat {ty = ty, var = NONE}
         in
            T (info, Prod.map (ts, dropVars))
         end

      fun fillInRoots (t: t, {base: Var.t Base.t, offset: int})
         : t * Statement.t list =
         let
            fun loop (t as T (info, ts), offset, ac) =
               case info of
                  Flat =>
                     let
                        val (ts, (offset, ac)) =
                           Vector.mapAndFold
                           (Prod.dest ts, (offset, ac),
                            fn ({elt = t, isMutable}, (offset, ac)) =>
                            let
                               val (t, offset, ac) = loop (t, offset, ac)
                            in
                               ({elt = t, isMutable = isMutable},
                                (offset, ac))
                            end)
                     in
                        (T (Flat, Prod.make ts), offset, ac)
                     end
                | NotFlat {ty, var} =>
                     let
                        val (t, ac) =
                           case var of
                              NONE =>
                                 let
                                    val var = Var.newNoname ()
                                 in
                                    (T (NotFlat {ty = ty, var = SOME var}, ts),
                                     Bind
                                     {exp = Select {base = base,
                                                    offset = offset},
                                      ty = ty,
                                      var = SOME var} :: ac)
                                 end
                            | SOME _ => (t, ac)
                     in
                        (t, offset + 1, ac)
                     end
            val (t, _, ac) = loop (t, offset, [])
         in
            (t, ac)
         end

      val fillInRoots =
         Trace.trace2 ("DeepFlatten.VarTree.fillInRoots",
                       layout,
                       fn {base, offset} =>
                       Layout.record [("base", Base.layout (base, Var.layout)),
                                      ("offset", Int.layout offset)],
                       Layout.tuple2 (layout, List.layout Statement.layout))
         fillInRoots
   end

fun flatten {base: Var.t Base.t option,
             from: VarTree.t,
             offset: int,
             to: TypeTree.t}: {offset: int} * VarTree.t * Statement.t list =
   let
      val Tree.T (from, fs) = from
   in
      case from of
         VarTree.Flat =>
            if TypeTree.isFlat to
               then flattensAt {base = base,
                                froms = fs,
                                offset = offset,
                                tos = Tree.children to}
            else Error.bug "DeepFlatten.flatten: cannot flatten from Flat to NotFlat"
       | VarTree.NotFlat {ty, var} =>
            let
               val (var, ss) =
                  case var of
                     NONE =>
                        let
                           val base =
                              case base of
                                 NONE => Error.bug "DeepFlatten.flatten: flatten missing base"
                               | SOME base => base
                           val result = Var.newNoname ()
                        in
                           (result,
                            [Bind {exp = Select {base = base,
                                                 offset = offset},
                                   ty = ty,
                                   var = SOME result}])
                        end
                   | SOME var => (var, [])
               val (r, ss) =
                  if TypeTree.isFlat to
                     then
                        let
                           val (_, r, ss') =
                              flattensAt {base = SOME (Base.Object var),
                                          froms = fs,
                                          offset = 0,
                                          tos = Tree.children to}
                        in
                           (r, ss @ ss')
                        end
                  else (Tree.T (VarTree.NotFlat {ty = ty, var = SOME var},
                                fs),
                        ss)
            in
               ({offset = 1 + offset}, r, ss)
            end
   end
and flattensAt {base: Var.t Base.t option,
                froms: VarTree.t Prod.t,
                offset: int,
                tos: TypeTree.t Prod.t} =
   let
      val (ts, (off, ss)) =
         Vector.map2AndFold
         (Prod.dest froms, Prod.dest tos, ({offset = offset}, []),
          fn ({elt = f, isMutable}, {elt = t, ...}, ({offset}, ss)) =>
          let
             val () =
                if isMutable
                   then Error.bug "DeepFlatten.flattensAt: mutable"
                else ()
             val ({offset}, t, ss') =
                flatten {base = base,
                         from = f,
                         offset = offset,
                         to = t}
          in
             ({elt = t, isMutable = false},
              ({offset = offset}, ss' @ ss))
          end)
   in
      (off, Tree.T (VarTree.Flat, Prod.make ts), ss)
   end

fun coerceTree {from: VarTree.t, to: TypeTree.t}: VarTree.t * Statement.t list =
   let
      val (_, r, ss) =
         flatten {base = NONE,
                  from = from,
                  offset = 0,
                  to = to}
   in
      (r, ss)
   end

val coerceTree =
   let
      open Layout
   in
      Trace.trace ("DeepFlatten.coerceTree",
                   fn {from, to} =>
                   record [("from", VarTree.layout from),
                           ("to", TypeTree.layout to)],
                   fn (vt, ss) =>
                   tuple [VarTree.layout vt,
                          List.layout Statement.layout ss])
      coerceTree
   end

structure Flat =
   struct
      datatype t = Flat | NotFlat

      val toString: t -> string =
         fn Flat => "Flat"
          | NotFlat => "NotFlat"

      val layout = Layout.str o toString
   end

datatype z = datatype Flat.t

structure Value =
   struct
      datatype t =
         Ground of Type.t
       | Object of object Equatable.t
       | Weak of {arg: t}
      withtype object = {args: t Prod.t,
                         coercedFrom: t AppendList.t ref,
                         con: ObjectCon.t,
                         finalOffsets: int vector option ref,
                         finalTree: TypeTree.t option ref,
                         finalType: Type.t option ref,
                         finalTypes: Type.t Prod.t option ref,
                         flat: Flat.t ref}

      fun layout (v: t): Layout.t =
         let
            open Layout
         in
            case v of
               Ground t => Type.layout t
             | Object e =>
                  Equatable.layout
                  (e, fn {args, con, flat, ...} => 
                   seq [str "Object ",
                        record [("args", Prod.layout (args, layout)),
                                ("con", ObjectCon.layout con),
                                ("flat", Flat.layout (! flat))]])
             | Weak {arg, ...} => seq [str "Weak ", layout arg]
         end

      val ground = Ground

      val traceCoerce =
         Trace.trace ("DeepFlatten.Value.coerce",
                      fn {from, to} =>
                      Layout.record [("from", layout from),
                                     ("to", layout to)],
                      Unit.layout)

      val traceUnify =
         Trace.trace2 ("DeepFlatten.Value.unify", layout, layout, Unit.layout)

      val rec unify: t * t -> unit =
         fn arg =>
         traceUnify
         (fn (v, v') =>
          case (v, v') of
             (Ground _, Ground _) => ()
           | (Object e, Object e') =>
                let
                   val callDont = ref false
                   val () =
                      Equatable.equate
                      (e, e',
                       fn (z as {args = a, coercedFrom = c, flat = f, ...},
                           z' as {args = a', coercedFrom = c', flat = f', ...}) =>
                       let
                          val () = unifyProd (a, a')
                       in
                          case (!f, !f') of
                             (Flat, Flat) =>
                                (c := AppendList.append (!c', !c); z)
                           | (Flat, NotFlat) =>
                                (callDont := true; z)
                           | (NotFlat, Flat) =>
                                (callDont := true; z')
                           | (NotFlat, NotFlat) => z
                       end)
                in
                   if !callDont
                      then dontFlatten v
                   else ()
                end
           | (Weak {arg = a, ...}, Weak {arg = a', ...}) =>
                unify (a, a')
           | _ => Error.bug "DeepFlatten.unify: strange") arg
      and unifyProd =
         fn (p, p') =>
         Vector.foreach2
         (Prod.dest p, Prod.dest p',
          fn ({elt = e, ...}, {elt = e', ...}) => unify (e, e'))
      and dontFlatten: t -> unit =
         fn v =>
         case v of
            Object e =>
               let
                  val {coercedFrom, flat, ...} = Equatable.value e
               in
                  case ! flat of
                     Flat =>
                        let
                           val () = flat := NotFlat
                           val from = !coercedFrom
                           val () = coercedFrom := AppendList.empty
                        in
                           AppendList.foreach (from, fn v' => unify (v, v'))
                        end
                   | NotFlat => ()
               end
          | _ => ()

      val rec coerce =
         fn arg as {from, to} =>
         traceCoerce
         (fn _ =>
          case (from, to) of
             (Ground _, Ground _) => ()
           | (Object e, Object e') =>
                if Equatable.equals (e, e')
                   then ()
                else
                   Equatable.whenComputed
                   (e', fn {args = a', coercedFrom = c', flat = f', ...} =>
                    let
                       val {args = a, con, ...} = Equatable.value e
                    in
                       if Prod.someIsMutable a orelse ObjectCon.isVector con
                          then unify (from, to)
                       else
                          case !f' of
                             Flat => (AppendList.push (c', from)
                                      ; coerceProd {from = a, to = a'})
                           | NotFlat => unify (from, to)
                    end)
           | (Weak _, Weak _) => unify (from, to)
           | _ => Error.bug "DeepFlatten.coerce: strange") arg
      and coerceProd =
         fn {from = p, to = p'} =>
         Vector.foreach2
         (Prod.dest p, Prod.dest p', fn ({elt = e, ...}, {elt = e', ...}) =>
          coerce {from = e, to = e'})

      fun mayFlatten {args, con}: bool =
         (* Don't flatten constructors, since they are part of a sum type.
          * Don't flatten unit.
          * Don't flatten vectors (of course their components can be
          * flattened).
          * Don't flatten objects with mutable fields, since sharing must be
          * preserved.
          *)
         not (Prod.isEmpty args)
         andalso Prod.allAreImmutable args
         andalso (case con of
                     ObjectCon.Con _ => false
                   | ObjectCon.Tuple => true
                   | ObjectCon.Vector => false)

      fun objectFields {args, con} =
         let
            (* Don't flatten object components that are immutable fields.  Those
             * have already had a chance to be flattened by other passes.
             *)
            val _  =
               if (case con  of
                      ObjectCon.Con _ => true
                    | ObjectCon.Tuple => true
                    | ObjectCon.Vector => false)
                  then Vector.foreach (Prod.dest args, fn {elt, isMutable} =>
                                       if isMutable
                                          then ()
                                       else dontFlatten elt)
               else ()
            val flat =
               if mayFlatten {args = args, con = con}
                  then Flat.Flat
               else Flat.NotFlat
         in
            {args = args,
             coercedFrom = ref AppendList.empty,
             con = con,
             finalOffsets = ref NONE,
             finalTree = ref NONE,
             finalType = ref NONE,
             finalTypes = ref NONE,
             flat = ref flat}
         end

      fun object f =
         Object (Equatable.delay (fn () => objectFields (f ())))

      val tuple: t Prod.t -> t =
         fn vs =>
         Object (Equatable.new (objectFields {args = vs, con = ObjectCon.Tuple}))

      val tuple =
         Trace.trace ("DeepFlatten.Value.tuple",
                      fn p => Prod.layout (p, layout),
                      layout)
         tuple

      fun weak (arg: t) = Weak {arg = arg}

      val deObject: t -> object option =
         fn v =>
         case v of
            Object e => SOME (Equatable.value e)
          | _ => NONE

      val traceFinalType =
         Trace.trace ("DeepFlatten.Value.finalType", layout, Type.layout)
      val traceFinalTypes =
         Trace.trace ("DeepFlatten.Value.finalTypes",
                      layout,
                      fn p => Prod.layout (p, Type.layout))

      fun finalTree (v: t): TypeTree.t =
         let
            fun notFlat (): TypeTree.info =
               TypeTree.NotFlat {ty = finalType v, var = NONE}
         in
            case deObject v of
               NONE => Tree.T (notFlat (), Prod.empty ())
             | SOME {args, finalTree = r, flat, ...} =>
                  Ref.memoize
                  (r, fn () =>
                   let
                      val info =
                         case !flat of
                            Flat => TypeTree.Flat
                          | NotFlat => notFlat ()
                   in
                      Tree.T (info, Prod.map (args, finalTree))
                   end)
         end
      and finalType arg: Type.t =
         traceFinalType
         (fn v =>
          case v of
             Ground t => t
           | Object e =>
                let
                   val {finalType = r, ...} = Equatable.value e
                in
                   Ref.memoize (r, fn () => Prod.elt (finalTypes v, 0))
                end
           | Weak {arg, ...} => Type.weak (finalType arg)) arg
      and finalTypes arg: Type.t Prod.t =
         traceFinalTypes
         (fn v =>
          case deObject v of
             NONE =>
                Prod.make (Vector.new1 {elt = finalType v,
                                        isMutable = false})
           | SOME {args, con, finalTypes, flat, ...} =>
                Ref.memoize
                (finalTypes, fn () =>
                 let
                    val args = prodFinalTypes args
                 in
                    case !flat of
                       Flat => args
                     | NotFlat =>
                          Prod.make
                          (Vector.new1
                           {elt = Type.object {args = args, con = con},
                            isMutable = false})
                 end)) arg
      and prodFinalTypes (p: t Prod.t): Type.t Prod.t =
         Prod.make
         (Vector.fromList
          (Vector.foldr
           (Prod.dest p, [], fn ({elt, isMutable = i}, ac) =>
            Vector.foldr
            (Prod.dest (finalTypes elt), ac, fn ({elt, isMutable = i'}, ac) =>
             {elt = elt, isMutable = i orelse i'} :: ac))))
   end

structure Object =
   struct
      type t = Value.object

      fun select ({args, ...}: t, offset): Value.t =
         Prod.elt (args, offset)

      fun finalOffsets ({args, finalOffsets = r, ...}: t): int vector =
         Ref.memoize
         (r, fn () =>
          Vector.fromListRev
          (#2 (Prod.fold
               (args, (0, []), fn (elt, (offset, offsets)) =>
                (offset + Prod.length (Value.finalTypes elt),
                 offset :: offsets)))))

      fun finalOffset (object, offset) =
         Vector.sub (finalOffsets object, offset)
   end

fun transform2 (program as Program.T {datatypes, functions, globals, main}) =
   let
      val {get = conValue: Con.t -> Value.t option ref, ...} =
         Property.get (Con.plist, Property.initFun (fn _ => ref NONE))
      val conValue =
         Trace.trace ("DeepFlatten.conValue",
                      Con.layout, Ref.layout (Option.layout Value.layout))
         conValue
      datatype 'a make =
         Const of 'a
       | Make of unit -> 'a
      val traceMakeTypeValue =
         Trace.trace ("DeepFlatten.makeTypeValue",
                      Type.layout o #1,
                      Layout.ignore)
      fun makeValue m =
         case m of
            Const v => v
          | Make f => f ()
      fun needToMakeProd p =
         Vector.exists (Prod.dest p, fn {elt, ...} =>
                        case elt of
                           Const _ => false
                         | Make _ => true)
      fun makeProd p = Prod.map (p, makeValue)
      val {get = makeTypeValue: Type.t -> Value.t make, ...} =
         Property.get
         (Type.plist,
          Property.initRec
          (traceMakeTypeValue
           (fn (t, makeTypeValue) =>
            let
               fun const () = Const (Value.ground t)
               datatype z = datatype Type.dest
            in
               case Type.dest t of
                  Object {args, con} =>
                     let
                        val args = Prod.map (args, makeTypeValue)
                        fun doit () =
                           if needToMakeProd args
                              orelse Value.mayFlatten {args = args, con = con}
                              then
                                 Make
                                 (fn () =>
                                  Value.object (fn () => {args = makeProd args,
                                                          con = con}))
                           else const ()
                        datatype z = datatype ObjectCon.t
                     in
                        case con of
                           Con c =>
                              Const (Ref.memoize
                                     (conValue c, fn () =>
                                      makeValue (doit ())))
                         | Tuple => doit ()
                         | Vector => doit ()
                     end
                | Weak t =>
                     (case makeTypeValue t of
                         Const _ => const ()
                       | Make f => Make (fn () => Value.weak (f ())))
                | _ => const ()
            end)))
      fun typeValue (t: Type.t): Value.t =
         makeValue (makeTypeValue t)
      val typeValue =
         Trace.trace ("DeepFlatten.typeValue", Type.layout, Value.layout)
         typeValue
      val (coerce, coerceProd) = (Value.coerce, Value.coerceProd)
      fun inject {sum, variant = _} = typeValue (Type.datatypee sum)
      fun object {args, con, resultType} =
         let
            val m = makeTypeValue resultType
         in
            case con of
               NONE =>
                  (case m of
                      Const v => v
                    | Make _ => Value.tuple args)
             | SOME _ =>
                  (case m of
                      Const v =>
                         let
                            val () =
                               case Value.deObject v of
                                  NONE => ()
                                | SOME {args = args', ...} =>
                                     coerceProd {from = args, to = args'}
                         in
                            v
                         end
                    | _ => Error.bug "DeepFlatten.object: strange con value")
         end
      val object =
         Trace.trace
         ("DeepFlatten.object",
          fn {args, con, ...} =>
          Layout.record [("args", Prod.layout (args, Value.layout)),
                         ("con", Option.layout Con.layout con)],
          Value.layout)
         object
      val deWeak : Value.t -> Value.t =
         fn v =>
         case v of
            Value.Ground t =>
               typeValue (case Type.dest t of
                             Type.Weak t => t
                           | _ => Error.bug "DeepFlatten.primApp: deWeak")
          | Value.Weak {arg, ...} => arg
          | _ => Error.bug "DeepFlatten.primApp: Value.deWeak"
      fun primApp {args, prim, resultVar = _, resultType} =
         let
            fun weak v =
               case makeTypeValue resultType of
                  Const v => v
                | Make _ => Value.weak v
            fun arg i = Vector.sub (args, i)
            fun result () = typeValue resultType
            datatype z = datatype Prim.Name.t
            fun dontFlatten () =
               (Vector.foreach (args, Value.dontFlatten)
                ; result ())
            fun equal () =
               (Value.unify (arg 0, arg 1)
                ; Value.dontFlatten (arg 0)
                ; result ())
         in
            case Prim.name prim of
               Array_toArray =>
                  let
                     val res = result ()
                     val () =
                        case (Value.deObject (arg 0), Value.deObject res) of
                           (NONE, NONE) => ()
                         | (SOME {args = a, ...}, SOME {args = a', ...}) =>
                              Vector.foreach2
                              (Prod.dest a, Prod.dest a',
                               fn ({elt = v, ...}, {elt = v', ...}) =>
                               Value.unify (v, v'))
                         | _ => Error.bug "DeepFlatten.primApp: Array_toArray"
                  in
                     res
                  end
             | Array_toVector =>
                  let
                     val res = result ()
                     val () =
                        case (Value.deObject (arg 0), Value.deObject res) of
                           (NONE, NONE) => ()
                         | (SOME {args = a, ...}, SOME {args = a', ...}) =>
                              Vector.foreach2
                              (Prod.dest a, Prod.dest a',
                               fn ({elt = v, ...}, {elt = v', ...}) =>
                               Value.unify (v, v'))
                         | _ => Error.bug "DeepFlatten.primApp: Array_toVector"
                  in
                     res
                  end
             | FFI _ =>
                  (* Some imports, like Real64.modf, take ref cells that can not
                   * be flattened.
                   *)
                  dontFlatten ()
             | MLton_eq => equal ()
             | MLton_equal => equal ()
             | MLton_size => dontFlatten ()
             | MLton_share => dontFlatten ()
             | Weak_get => deWeak (arg 0)
             | Weak_new => 
                  let val a = arg 0
                  in (Value.dontFlatten a; weak a)
                  end
             | _ => result ()
         end
      fun base b =
         case b of
            Base.Object obj => obj
          | Base.VectorSub {vector, ...} => vector
      fun select {base, offset} =
         let
            datatype z = datatype Value.t
         in
            case base of
               Ground t =>
                  (case Type.dest t of
                      Type.Object {args, ...} =>
                         typeValue (Prod.elt (args, offset))
                    | _ => Error.bug "DeepFlatten.select: Ground")
             | Object e => Object.select (Equatable.value e, offset)
             | _ => Error.bug "DeepFlatten.select:"
         end
      fun update {base, offset, value} =
         coerce {from = value,
                 to = select {base = base, offset = offset}}
      fun const c = typeValue (Type.ofConst c)
      val {func, value = varValue, ...} =
         analyze {base = base,
                  coerce = coerce,
                  const = const,
                  filter = fn _ => (),
                  filterWord = fn _ => (),
                  fromType = typeValue,
                  inject = inject,
                  layout = Value.layout,
                  object = object,
                  primApp = primApp,
                  program = program,
                  select = fn {base, offset, ...} => select {base = base,
                                                             offset = offset},
                  update = update,
                  useFromTypeOnBinds = false}
      (* Don't flatten outermost part of formal parameters. *)
      fun dontFlattenFormals (xts: (Var.t * Type.t) vector): unit =
         Vector.foreach (xts, fn (x, _) => Value.dontFlatten (varValue x))
      val () =
         List.foreach
         (functions, fn f =>
          let
             val {args, blocks, ...} = Function.dest f
             val () = dontFlattenFormals args
             val () = Vector.foreach (blocks, fn Block.T {args, ...} =>
                                      dontFlattenFormals args)
          in
             ()
          end)
      val () =
         Control.diagnostics
         (fn display =>
          let
             open Layout
             val () =
                Vector.foreach
                (datatypes, fn Datatype.T {cons, ...} =>
                 Vector.foreach
                 (cons, fn {con, ...} =>
                  display (Option.layout Value.layout (! (conValue con)))))
             val () =
                Program.foreachVar
                (program, fn (x, _) =>
                 display
                 (seq [Var.layout x, str " ", Value.layout (varValue x)]))
          in
             ()
          end)
      (* Transform the program. *)
      val datatypes =
         Vector.map
         (datatypes, fn Datatype.T {cons, tycon} =>
          let
             val cons =
                Vector.map
                (cons, fn {con, args} =>
                 let
                    val args =
                       case ! (conValue con) of
                          NONE => args
                        | SOME v => 
                             case Type.dest (Value.finalType v) of
                                Type.Object {args, ...} => args
                              | _ => Error.bug "DeepFlatten.datatypes: strange con"
                 in
                    {args = args, con = con}
                 end)
          in
             Datatype.T {cons = cons, tycon = tycon}
          end)
      val valueType = Value.finalType
      fun valuesTypes vs = Vector.map (vs, Value.finalType)
      val {get = varTree: Var.t -> VarTree.t, set = setVarTree, ...} =
         Property.getSetOnce (Var.plist,
                              Property.initRaise ("tree", Var.layout))
      val setVarTree =
         Trace.trace2 ("DeepFlatten.setVarTree",
                       Var.layout, VarTree.layout, Unit.layout)
         setVarTree
      fun simpleVarTree (x: Var.t): unit =
         setVarTree
         (x, VarTree.labelRoot (VarTree.fromTypeTree
                                (Value.finalTree (varValue x)),
                                x))
      fun transformFormals xts =
         Vector.map (xts, fn (x, _) =>
                     let
                        val () = simpleVarTree x
                     in
                        (x, Value.finalType (varValue x))
                     end)
      fun replaceVar (x: Var.t): Var.t =
         let
            fun bug () = Error.bug (concat ["DeepFlatten.replaceVar ", Var.toString x])
            val Tree.T (info, _) = varTree x
         in
            case info of
               VarTree.Flat => bug ()
             | VarTree.NotFlat {var, ...} =>
                  case var of
                     NONE => bug ()
                   | SOME y => y
         end
      fun transformBind {exp, ty, var}: Statement.t list =
         let
            fun simpleTree () = Option.app (var, simpleVarTree)
            fun doit (e: Exp.t) =
               let
                  val ty =
                     case var of
                        NONE => ty
                      | SOME var => valueType (varValue var)
               in
                  [Bind {exp = e, ty = ty, var = var}]
               end
            fun simple () =
               (simpleTree ()
                ; doit (Exp.replaceVar (exp, replaceVar)))
            fun none () = []
         in
            case exp of
               Exp.Const _ => simple ()
             | Inject _ => simple ()
             | Object {args, con} =>
                  (case var of
                      NONE => none ()
                    | SOME var =>
                         let
                            val v = varValue var
                         in
                            case Value.deObject v of
                               NONE => simple ()
                             | SOME {args = expects, flat, ...} =>
                                  let
                                     val z =
                                        Vector.map2
                                        (args, Prod.dest expects,
                                         fn (arg, {elt, isMutable}) =>
                                         let
                                            val (vt, ss) =
                                               coerceTree
                                               {from = varTree arg,
                                                to = Value.finalTree elt}
                                         in
                                            ({elt = vt,
                                              isMutable = isMutable},
                                             ss)
                                         end)
                                     val vts = Vector.map (z, #1)
                                     fun set info =
                                        setVarTree (var,
                                                    Tree.T (info,
                                                            Prod.make vts))
                                  in
                                     case !flat of
                                        Flat => (set VarTree.Flat; none ())
                                      | NotFlat =>
                                           let
                                              val ty = Value.finalType v
                                              val () =
                                                 set (VarTree.NotFlat
                                                      {ty = ty,
                                                       var = SOME var})
                                              val args =
                                                 Vector.fromList
                                                 (Vector.foldr
                                                  (vts, [],
                                                   fn ({elt = vt, ...}, ac) =>
                                                   VarTree.rootsOnto (vt, ac)))
                                              val obj =
                                                 Bind
                                                 {exp = Object {args = args,
                                                                con = con},
                                                  ty = ty,
                                                  var = SOME var}
                                           in
                                              Vector.foldr
                                              (z, [obj],
                                               fn ((_, ss), ac) => ss @ ac)
                                           end
                                  end
                         end)
             | PrimApp _ => simple ()
             | Select {base, offset} =>
                  (case var of
                      NONE => none ()
                    | SOME var =>
                         let
                            val baseVar = Base.object base
                         in
                            case Value.deObject (varValue baseVar) of
                               NONE => simple ()
                             | SOME obj => 
                                  let
                                     val Tree.T (info, children) =
                                        varTree baseVar
                                     val {elt = child, isMutable} =
                                        Prod.sub (children, offset)
                                     val (child, ss) =
                                        case info of
                                           VarTree.Flat => (child, [])
                                         | VarTree.NotFlat _ =>
                                              let
                                                 val child =
                                                    (* Don't simplify a select out
                                                     * of a mutable field.
                                                     * Something may have mutated
                                                     * it.
                                                     *)
                                                    if isMutable
                                                       then VarTree.dropVars child
                                                    else child
                                              in
                                                 VarTree.fillInRoots
                                                 (child,
                                                  {base = Base.map (base, replaceVar),
                                                   offset = (Object.finalOffset
                                                             (obj, offset))})
                                              end
                                     val () = setVarTree (var, child)
                                  in
                                     ss
                                  end
                         end)
             | Var x =>
                  (Option.app (var, fn y => setVarTree (y, varTree x))
                   ; none ())
         end
      fun transformStatement (s: Statement.t): Statement.t list =
         let
            fun simple () = [Statement.replaceUses (s, replaceVar)]
         in
            case s of
               Bind b => transformBind b
             | Profile _ => simple ()
             | Update {base, offset, value} =>
                  let
                     val baseVar =
                        case base of
                           Base.Object x => x
                         | Base.VectorSub {vector = x, ...} => x
                  in
                     case Value.deObject (varValue baseVar) of
                        NONE => simple ()
                      | SOME object => 
                           let
                              val ss = ref []
                              val child =
                                 Value.finalTree (Object.select (object, offset))
                              val offset = Object.finalOffset (object, offset)
                              val base = Base.map (base, replaceVar)
                              val us =
                                 if not (TypeTree.isFlat child)
                                    then [Update {base = base,
                                                  offset = offset,
                                                  value = replaceVar value}]
                                 else
                                    let
                                       val (vt, ss') =
                                          coerceTree {from = varTree value,
                                                      to = child}
                                       val () = ss := ss' @ (!ss)
                                       val r = ref offset
                                       val us = ref []
                                       val () =
                                          VarTree.foreachRoot
                                          (vt, fn var =>
                                           let
                                              val offset = !r
                                              val () = r := 1 + !r
                                           in
                                              List.push (us,
                                                         Update {base = base,
                                                                 offset = offset,
                                                                 value = var})
                                           end)
                                    in
                                       !us
                                    end
                           in
                              !ss @ us
                           end
                  end
         end
      val transformStatement =
         Trace.trace ("DeepFlatten.transformStatement",
                      Statement.layout,
                      List.layout Statement.layout)
         transformStatement
      fun transformStatements ss =
         Vector.concatV
         (Vector.map (ss, Vector.fromList o transformStatement))
      fun transformTransfer t = Transfer.replaceVar (t, replaceVar)
      val transformTransfer =
         Trace.trace ("DeepFlatten.transformTransfer",
                      Transfer.layout, Transfer.layout)
         transformTransfer
      fun transformBlock (Block.T {args, label, statements, transfer}) =
         Block.T {args = transformFormals args,
                  label = label,
                  statements = transformStatements statements,
                  transfer = transformTransfer transfer}
      fun transformFunction (f: Function.t): Function.t =
          let
             val {args, mayInline, name, start, ...} = Function.dest f
             val {raises, returns, ...} = func name
             val args = transformFormals args
             val raises = Option.map (raises, valuesTypes)
             val returns = Option.map (returns, valuesTypes)
             val blocks = ref []
             val () =
                Function.dfs (f, fn b =>
                              (List.push (blocks, transformBlock b)
                               ; fn () => ()))
          in
             Function.new {args = args,
                           blocks = Vector.fromList (!blocks),
                           mayInline = mayInline,
                           name = name,
                           raises = raises,
                           returns = returns,
                           start = start}
          end
      val globals = transformStatements globals
      val functions = List.revMap (functions, transformFunction)
      val program =
         Program.T {datatypes = datatypes,
                    functions = functions,
                    globals = globals,
                    main = main}
      val () = Program.clear program
   in
      shrink program
   end

end
