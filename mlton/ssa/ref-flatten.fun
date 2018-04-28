(* Copyright (C) 2009,2017 Matthew Fluet.
 * Copyright (C) 2004-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor RefFlatten (S: SSA2_TRANSFORM_STRUCTS): SSA2_TRANSFORM = 
struct

open S

structure Graph = DirectedGraph
structure Node = Graph.Node

datatype z = datatype Exp.t
datatype z = datatype Statement.t
datatype z = datatype Transfer.t

structure Finish =
   struct
      datatype t = T of {flat: Type.t Prod.t option, 
                         ty: Type.t}

      val _: t -> Layout.t =
         fn T {flat, ty} =>
         let
            open Layout
         in
            record [("flat",
                     Option.layout (fn p => Prod.layout (p, Type.layout)) flat),
                    ("ty", Type.layout ty)]
         end
   end

structure Value =
   struct
      datatype t =
         GroundV of Type.t
       | Complex of computed Equatable.t
      and computed =
         ObjectC of object
       | WeakC of {arg: t,
                   finalType: Type.t option ref,
                   originalType: Type.t}
      and object =
         Obj of {args: t Prod.t,
                 con: ObjectCon.t,
                 finalComponents: Type.t Prod.t option ref,
                 finalOffsets: int vector option ref,
                 finalType: Type.t option ref,
                 flat: flat ref,
                 originalType: Type.t}
      and flat =
         NotFlat
       | Offset of {object: object,
                    offset: int}
       | Unknown

      fun delay (f: unit -> computed): t = Complex (Equatable.delay f)

      datatype value =
         Ground of Type.t
       | Object of object
       | Weak of {arg: t,
                  finalType: Type.t option ref,
                  originalType: Type.t}

      val value: t -> value =
         fn GroundV t => Ground t
          | Complex e =>
               case Equatable.value e of
                  ObjectC obj => Object obj
                | WeakC w => Weak w

      local
         open Layout
      in
         fun layout v: Layout.t =
            case v of
               GroundV t => Type.layout t
             | Complex e =>
                  Equatable.layout 
                  (e,
                   fn ObjectC ob => layoutObject ob
                    | WeakC {arg, ...} => seq [str "Weak ", layout arg])
         and layoutFlat (f: flat): Layout.t =
            case f of
               NotFlat => str "NotFlat"
             | Offset {offset, ...} =>
                  seq [str "Offset ",
                       record [("offset", Int.layout offset)]]
             | Unknown => str "Unknown"
         and layoutObject (Obj {args, con, flat, ...}) =
            seq [str "Object ",
                 record [("args", Prod.layout (args, layout)),
                         ("con", ObjectCon.layout con),
                         ("flat", layoutFlat (! flat))]]
      end

      fun originalType (v: t) =
         case value v of
            Ground t => t
          | Object (Obj {originalType = t, ...}) => t
          | Weak {originalType = t, ...} => t
   end

structure Flat =
   struct
      datatype t = datatype Value.flat
   end

structure Object =
   struct
      datatype t = datatype Value.object

      val layout = Value.layoutObject

      fun equals (Obj {flat = f, ...}, Obj {flat = f', ...}) = f = f'

      val select: t * int -> Value.t =
         fn (Obj {args, ...}, offset) =>
         Prod.elt (args, offset)
   end

datatype z = datatype Object.t

structure Value =
   struct
      open Value

      val ground = GroundV

      val deObject: t -> Object.t option =
         fn v =>
         case value v of
            Object ob => SOME ob
          | _ => NONE

      fun deFlat {inner: t, outer: Object.t}: Object.t option =
         case value inner of
            Object (z as Obj {flat, ...}) =>
               (case ! flat of
                   Flat.Offset {object, ...} =>
                      if Object.equals (object, outer) then SOME z else NONE
                 | _ => NONE)
          | _ => NONE

      fun dontFlatten (v: t): unit =
         case value v of
            Object (Obj {flat, ...}) => flat := NotFlat
          | _ => ()

      fun isUnit v =
         case v of
            GroundV t => Type.isUnit t
          | _ => false

      fun objectC {args: t Prod.t, con: ObjectCon.t, originalType}
         : computed =
         let
             (* Only may flatten objects with mutable fields, and where the field
              * isn't unit.  Flattening a unit field could lead to a problem
              * because the containing object might be otherwise immutable, and
              * hence the unit ref would lose its identity.  We can fix this
              * once objects have a notion of identity independent of mutability.
              *)
             val flat =
                ref
                (if Vector.exists (Prod.dest args, fn {elt, isMutable} =>
                                   isMutable andalso not (isUnit elt))
                    andalso not (ObjectCon.isVector con)
                    then Unknown
                 else NotFlat)
          in
             ObjectC (Obj {args = args,
                           con = con,
                           finalComponents = ref NONE,
                           finalOffsets = ref NONE,
                           finalType = ref NONE,
                           flat = flat,
                           originalType = originalType})
          end

      val computed: computed -> t =
         fn c => Complex (Equatable.new c)

      fun weakC (a: t): computed =
         WeakC {arg = a,
                finalType = ref NONE,
                originalType = Type.weak (originalType a)}

      val weak = computed o weakC

      fun tuple (args: t Prod.t, originalType: Type.t): t =
         computed (objectC {args = args,
                            con = ObjectCon.Tuple,
                            originalType = originalType})

      val tuple =
         Trace.trace ("RefFlatten.Value.tuple", fn (p, _) => Prod.layout (p, layout),
                      layout)
         tuple

      val rec unify: t * t -> unit =
         fn z =>
         case z of
            (GroundV t, GroundV t') =>
               if Type.equals (t, t') then ()
               else Error.bug "RefFlatten.Value.unify: unequal Grounds"
          | (Complex e, Complex e') =>
               Equatable.equate
               (e, e', fn (c, c') =>
                case (c, c') of
                   (ObjectC (Obj {args = a, flat = f, ...}), 
                    ObjectC (Obj {args = a', flat = f', ...})) =>
                      let
                         val () = unifyProd (a, a')
                         val () =
                            case (!f, !f') of
                               (_, NotFlat) => f := NotFlat
                             | (NotFlat, _) => f' := NotFlat
                             | (Offset _, _) =>
                                  Error.bug "RefFlatten.Value.unify: Offset"
                             | (_, Offset _) =>
                                  Error.bug "RefFlatten.Value.unify: Offset"
                             | _ => ()
                      in
                         c
                      end
                 | (WeakC {arg = a, ...}, WeakC {arg = a', ...}) =>
                      (unify (a, a'); c)
                 | _ => Error.bug "RefFlatten.Value.unify: strange Complex")
          | _ => Error.bug "RefFlatten.Value.unify: Complex with Ground"
      and unifyProd =
         fn (p, p') =>
         Vector.foreach2
         (Prod.dest p, Prod.dest p',
          fn ({elt = e, ...}, {elt = e', ...}) => unify (e, e'))

      fun coerce {from, to} = unify (from, to)

      val coerce =
         Trace.trace ("RefFlatten.Value.coerce",
                      fn {from, to} =>
                      Layout.record [("from", layout from),
                                     ("to", layout to)],
                      Unit.layout)
         coerce
   end

structure Size = TwoPointLattice (val bottom = "small"
                                  val top = "large")

structure VarInfo =
   struct
      datatype useStatus =
         InTuple of {object: Object.t,
                     objectVar: Var.t,
                     offset: int}
       | Unused

      datatype t =
         Flattenable of {components: Var.t vector,
                         defBlock: Label.t,
                         useStatus: useStatus ref}
       | Unflattenable

      fun layout (i: t): Layout.t =
         let
            open Layout
         in
            case i of
               Flattenable {components, defBlock, useStatus} =>
                  seq [str "Flattenable ",
                       record [("components",
                                Vector.layout Var.layout components),
                               ("defBlock", Label.layout defBlock),
                               ("useStatus",
                                (case !useStatus of
                                    InTuple {object, objectVar, offset} =>
                                       seq [str "InTuple ",
                                            record [("object",
                                                     Object.layout object),
                                                    ("objectVar",
                                                     Var.layout objectVar),
                                                    ("offset",
                                                     Int.layout offset)]]
                                  | Unused => str "Unused"))]]
             | Unflattenable => str "Unflattenable"
         end
   end

fun transform2 (program as Program.T {datatypes, functions, globals, main}) =
   let
      val {get = conValue: Con.t -> Value.t option ref, ...} =
         Property.get (Con.plist, Property.initFun (fn _ => ref NONE))
      val conValue =
         Trace.trace ("RefFlatten.conValue",
                      Con.layout, Ref.layout (Option.layout Value.layout))
         conValue
      datatype 'a make =
         Const of 'a
       | Make of unit -> 'a
      fun needToMakeProd p =
         Vector.exists (Prod.dest p, fn {elt, ...} =>
                        case elt of
                           Const _ => false
                         | Make _ => true)
      fun makeProd p =
         Prod.map (p, fn m =>
                   case m of
                      Const v => v
                    | Make f => f ())
      val {get = makeTypeValue: Type.t -> Value.t make, ...} =
         Property.get
         (Type.plist,
          Property.initRec
          (fn (t, makeTypeValue) =>
           let
              fun const () = Const (Value.ground t)
              datatype z = datatype Type.dest
           in
              case Type.dest t of
                 Object {args, con} =>
                    let
                       fun doit () =
                          let
                             val args = Prod.map (args, makeTypeValue)
                             val mayFlatten =
                                Vector.exists (Prod.dest args, #isMutable)
                                andalso not (ObjectCon.isVector con)
                          in
                             if mayFlatten orelse needToMakeProd args
                                then Make (fn () =>
                                           Value.delay
                                           (fn () =>
                                            Value.objectC {args = makeProd args,
                                                           con = con,
                                                           originalType = t}))
                             else const ()
                          end
                       datatype z = datatype ObjectCon.t
                    in
                       case con of
                          Con c =>
                             Const
                             (Ref.memoize
                              (conValue c, fn () =>
                               case doit () of
                                  Const v => v
                                | Make f =>
                                     let
                                        val v = f ()
                                        (* Constructors can never be
                                         * flattened into other objects.
                                         *)
                                        val () = Value.dontFlatten v
                                     in
                                        v
                                     end))
                        | Tuple => doit ()
                        | Vector => doit ()
                    end
               | Weak t =>
                    (case makeTypeValue t of
                        Const _ => const ()
                      | Make f =>
                           Make (fn () =>
                                 Value.delay (fn () => Value.weakC (f ()))))
               | _ => const ()
           end))
      fun typeValue (t: Type.t): Value.t =
          case makeTypeValue t of
             Const v => v
           | Make f => f ()
      val typeValue =
         Trace.trace ("RefFlatten.typeValue", Type.layout, Value.layout) typeValue
      val coerce = Value.coerce
      fun inject {sum, variant = _} = typeValue (Type.datatypee sum)
      fun object {args, con, resultType} =
         let
            val m = makeTypeValue resultType
         in
            case con of
               NONE =>
                  (case m of
                      Const v => v
                    | Make _ => Value.tuple (args, resultType))
             | SOME _ =>
                  (case m of
                      Const v =>
                         let
                            val () =
                               case Value.deObject v of
                                  NONE => ()
                                | SOME (Obj {args = args', ...}) =>
                                     Vector.foreach2
                                     (Prod.dest args, Prod.dest args',
                                      fn ({elt = a, ...}, {elt = a', ...}) =>
                                      coerce {from = a, to = a'})
                         in
                            v
                         end
                    | _ => Error.bug "RefFlatten.object: strange con value")
         end
      val object =
         Trace.trace
         ("RefFlatten.object",
          fn {args, con, ...} =>
          Layout.record [("args", Prod.layout (args, Value.layout)),
                         ("con", Option.layout Con.layout con)],
          Value.layout)
         object
      val deWeak: Value.t -> Value.t =
         fn v =>
         case Value.value v of
            Value.Ground t =>
               typeValue (case Type.dest t of
                             Type.Weak t => t
                           | _ => Error.bug "RefFlatten.deWeak")
          | Value.Weak {arg, ...} => arg
          | _ => Error.bug "RefFlatten.deWeak"
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
                ; result ())
         in
            case Prim.name prim of
               Array_toArray =>
                  let
                     val res = result ()
                     datatype z = datatype Value.value
                     val () =
                        case (Value.value (arg 0), Value.value res) of
                           (Ground _, Ground _) => ()
                         | (Object (Obj {args = a, ...}),
                            Object (Obj {args = a', ...})) =>
                              Vector.foreach2
                              (Prod.dest a, Prod.dest a',
                               fn ({elt = v, ...}, {elt = v', ...}) =>
                               Value.unify (v, v'))
                         | _ => Error.bug "RefFlatten.primApp: Array_toArray"
                  in
                     res
                  end
             | Array_toVector =>
                  let
                     val res = result ()
                     datatype z = datatype Value.value
                     val () =
                        case (Value.value (arg 0), Value.value res) of
                           (Ground _, Ground _) => ()
                         | (Object (Obj {args = a, ...}),
                            Object (Obj {args = a', ...})) =>
                              Vector.foreach2
                              (Prod.dest a, Prod.dest a',
                               fn ({elt = v, ...}, {elt = v', ...}) =>
                               Value.unify (v, v'))
                         | _ => Error.bug "RefFlatten.primApp: Array_toVector"
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
            datatype z = datatype Value.value
         in
            case Value.value base of
               Ground t =>
                  (case Type.dest t of
                      Type.Object {args, ...} =>
                         typeValue (Prod.elt (args, offset))
                    | _ => Error.bug "RefFlatten.select: Ground")
             | Object ob => Object.select (ob, offset)
             | _ => Error.bug "RefFlatten.select"
         end
      fun update {base, offset, value} =
         (coerce {from = value,
                  to = select {base = base, offset = offset}}
          (* Don't flatten the component of the update, 
           * else sharing will be broken.
           *)
          ; Value.dontFlatten value)
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
      val varObject = Value.deObject o varValue
      (* Mark a variable as Flattenable if all its uses are contained in a single
       * basic block, there is a single use in an object construction, and
       * all other uses follow the object construction.
       *
       * ...
       * r: (t ref) = (t)
       * ... <no uses of r> ...
       * x: (... * (t ref) * ...) = (..., r, ...)
       * ... <other assignments to r> ...
       *
       *)
      datatype z = datatype VarInfo.t
      datatype z = datatype VarInfo.useStatus
      val {get = varInfo: Var.t -> VarInfo.t ref, ...} =
         Property.get (Var.plist,
                       Property.initFun (fn _ => ref VarInfo.Unflattenable))
      val varInfo =
         Trace.trace ("RefFlatten.varInfo",
                      Var.layout, Ref.layout VarInfo.layout)
         varInfo
      fun use x = varInfo x := Unflattenable
      val use = Trace.trace ("RefFlatten.use", Var.layout, Unit.layout) use
      fun uses xs = Vector.foreach (xs, use)
      fun loopStatement (s: Statement.t, current: Label.t): unit =
         case s of
            Bind {exp = Exp.Object {args, ...}, var, ...} =>
               (case var of
                   NONE => uses args
                 | SOME var =>
                      case Value.deObject (varValue var) of
                         NONE => uses args
                       | SOME object =>
                            let
                               val () =
                                  varInfo var
                                  := Flattenable {components = args,
                                                  defBlock = current,
                                                  useStatus = ref Unused}
                            in
                               Vector.foreachi
                               (args, fn (offset, x) =>
                                let
                                   val r = varInfo x
                                in
                                   case !r of
                                      Flattenable {defBlock, useStatus, ...} =>
                                         (if Label.equals (current, defBlock)
                                             andalso (case !useStatus of
                                                         InTuple _ => false
                                                       | Unused => true)
                                             then (useStatus
                                                   := (InTuple
                                                       {object = object,
                                                        objectVar = var,
                                                        offset = offset}))
                                          else r := Unflattenable)
                                    | Unflattenable => ()
                                end)
                            end)
          | Statement.Update {base, value, ...} =>
               (use value
                ; (case base of
                      Base.Object r =>
                         let
                            val i = varInfo r
                         in
                            case ! i of
                               Flattenable {defBlock, useStatus, ...} =>
                                  if Label.equals (current, defBlock)
                                     andalso (case !useStatus of
                                                 InTuple _ => true
                                               | Unused => false)
                                     then ()
                                  else i := Unflattenable
                             | Unflattenable => ()
                         end
                    | Base.VectorSub _ => ()))
          | _ => Statement.foreachUse (s, use)
      val loopStatement =
         Trace.trace2
         ("RefFlatten.loopStatement", Statement.layout, Label.layout,
          Unit.layout)
         loopStatement
      fun loopStatements (ss, label) =
         Vector.foreach (ss, fn s => loopStatement (s, label))
      fun loopTransfer t = Transfer.foreachVar (t, use)
      val globalLabel = Label.newNoname ()
      val () = loopStatements (globals, globalLabel)
      val () =
         List.foreach
         (functions, fn f =>
          Function.dfs
          (f, fn Block.T {label, statements, transfer, ...} =>
           (loopStatements (statements, label)
            ; loopTransfer transfer
            ; fn () => ())))
      fun foreachObject (f): unit =
         let
            fun loopStatement s =
               case s of
                  Bind {exp = Exp.Object {args, ...}, var, ...} =>
                     Option.app
                     (var, fn var =>
                      case Value.value (varValue var) of
                         Value.Ground _ => ()
                       | Value.Object obj => f (var, args, obj)
                       | _ => 
                            Error.bug 
                            "RefFlatten.foreachObject: Object with strange value")
                | _ => ()
            val () = Vector.foreach (globals, loopStatement)
            val () =
               List.foreach
               (functions, fn f =>
                let
                   val {blocks, ...} = Function.dest f
                in
                   Vector.foreach
                   (blocks, fn Block.T {statements, ...} =>
                    Vector.foreach (statements, loopStatement))
                end)
         in
            ()
         end
      (* Try to flatten each ref. *)
      val () =
         foreachObject
         (fn (var, _, Obj {flat, ...}) =>
          let
             datatype z = datatype Flat.t
             fun notFlat () = flat := NotFlat
             val () =
                case ! (varInfo var) of
                   Flattenable {useStatus, ...} =>
                      (case !useStatus of
                          InTuple {object = obj', offset = i', ...} =>
                             (case ! flat of
                                 NotFlat => ()
                               | Offset {object = obj'', offset = i''} =>
                                    if i' = i'' andalso Object.equals (obj', obj'')
                                       then ()
                                       else notFlat ()
                               | Unknown => flat := Offset {object = obj',
                                                            offset = i'})
                        | Unused => notFlat ())
                 | Unflattenable => notFlat ()
          in
             ()
          end)
      val () =
         foreachObject
         (fn (_, args, obj) =>
          let
             datatype z = datatype Flat.t
             (* Check that all arguments that are represented by flattening them
              * into the object are available as an explicit allocation.
              *)
             val () =
                Vector.foreach
                (args, fn a =>
                 case Value.deFlat {inner = varValue a, outer = obj} of
                    NONE => ()
                  | SOME (Obj {flat, ...}) =>
                       case ! (varInfo a) of
                          Flattenable _ => ()
                        | Unflattenable =>
                             flat := NotFlat)
          in
             ()
          end)
      (*
       * The following code disables flattening of some refs to ensure
       * space safety.  Flattening a ref into an object that has
       * another component that contains a value of unbounded size (a
       * large object) could keep the large object alive beyond where
       * it should be.  So, we first use a simple fixed point to
       * figure out which types have values of unbounded size.  Then,
       * for each reference to a mutable object, if we are trying to
       * flatten it into an object that has another component with a
       * large value and the container is not live in this block (we
       * approximate liveness), then don't allow the flattening to
       * happen.
       *
       * Vectors may be objects of unbounded size.
       * Weak pointers may not be objects of unbounded size; weak
       * pointers do not keep pointed-to object live.
       * Instances of recursive datatypes may be objects of unbounded
       * size.
       *)
      val {get = tyconSize: Tycon.t -> Size.t, ...} =
         Property.get (Tycon.plist, Property.initFun (fn _ => Size.new ()))
      (* Force (mutually) recursive datatypes to top. *)
      val {get = nodeTycon: unit Node.t -> Tycon.t, 
           set = setNodeTycon, ...} =
         Property.getSetOnce 
         (Node.plist, Property.initRaise ("nodeTycon", Node.layout))
      val {get = tyconNode: Tycon.t -> unit Node.t, 
           set = setTyconNode, ...} =
         Property.getSetOnce 
         (Tycon.plist, Property.initRaise ("tyconNode", Tycon.layout))
      val graph = Graph.new ()
      val () =
         Vector.foreach
         (datatypes, fn Datatype.T {tycon, ...} =>
          let
             val node = Graph.newNode graph
             val () = setTyconNode (tycon, node)
             val () = setNodeTycon (node, tycon)
          in 
             ()
          end)
      val () =
         Vector.foreach
         (datatypes, fn Datatype.T {cons, tycon} =>
          let
             val n = tyconNode tycon
             datatype z = datatype Type.dest
             val {get = dependsOn, destroy = destroyDependsOn} =
                Property.destGet
                (Type.plist,
                 Property.initRec
                 (fn (t, dependsOn) =>
                  case Type.dest t of
                     Datatype tc =>
                        (ignore o Graph.addEdge)
                        (graph, {from = n, to = tyconNode tc})
                   | Object {args, ...} =>
                        Prod.foreach (args, dependsOn)
                   | _ => ()))
             val () = Vector.foreach (cons, fn {args, ...} =>
                                      Prod.foreach (args, dependsOn))
             val () = destroyDependsOn ()
          in
             ()
          end)
      val () =
         List.foreach
         (Graph.stronglyConnectedComponents graph, fn ns =>
          let
             fun doit () =
                List.foreach
                (ns, fn n =>
                 Size.makeTop (tyconSize (nodeTycon n)))
          in
             case ns of
                [n] => if Node.hasEdge {from = n, to = n}
                          then doit ()
                       else ()
              | _ => doit ()
          end)
      val {get = typeSize: Type.t -> Size.t, ...} =
         Property.get (Type.plist,
                       Property.initRec
                       (fn (t, typeSize) =>
                        let
                           val s = Size.new ()
                           fun dependsOn (t: Type.t): unit =
                              Size.<= (typeSize t, s)
                           datatype z = datatype Type.dest
                           val () =
                              case Type.dest t of
                                 CPointer => ()
                               | Datatype tc => Size.<= (tyconSize tc, s)
                               | IntInf => Size.makeTop s
                               | Object {args, con, ...} =>
                                    if ObjectCon.isVector con
                                       then Size.makeTop s
                                    else Prod.foreach (args, dependsOn)
                               | Real _ => ()
                               | Thread => Size.makeTop s
                               | Weak _ => ()
                               | Word _ => ()
                        in
                           s
                        end))
      val () =
         Vector.foreach
         (datatypes, fn Datatype.T {cons, tycon} =>
          let
             val s = tyconSize tycon
             fun dependsOn (t: Type.t): unit = Size.<= (typeSize t, s)
             val () = Vector.foreach (cons, fn {args, ...} =>
                                      Prod.foreach (args, dependsOn))
          in
             ()
          end)
      fun typeIsLarge (t: Type.t): bool =
         Size.isTop (typeSize t)
      fun objectHasAnotherLarge (Object.Obj {args, ...}, {offset: int}) =
         Vector.existsi (Prod.dest args, fn (i, {elt, ...}) =>
                         i <> offset
                         andalso typeIsLarge (Value.originalType elt))
      val () =
         List.foreach
         (functions, fn f =>
          let
             val {blocks, ...} = Function.dest f
          in
             Vector.foreach
             (blocks, fn Block.T {statements, transfer, ...} =>
              let
                 fun containerIsLive (x: Var.t) =
                    Vector.exists
                    (statements, fn s =>
                     case s of
                        Bind {exp, var = SOME x', ...} =>
                           Var.equals (x, x')
                           andalso (case exp of
                                       Exp.Select _ => true
                                     | _ => false)
                      | _ => false)
                 fun use (x: Var.t) =
                    case Value.value (varValue x) of
                       Value.Object (Obj {flat, ...}) =>
                          (case !flat of
                              Flat.Offset {object, offset} =>
                                 if objectHasAnotherLarge (object,
                                                           {offset = offset})
                                    andalso not (containerIsLive x)
                                    then flat := Flat.NotFlat
                                 else ()
                            | _ => ())
                     | _ => ()
                 val () = Vector.foreach (statements, fn s =>
                                          Statement.foreachUse (s, use))
                 val () = Transfer.foreachVar (transfer, use)
              in
                 ()
              end)
          end)
      (* Mark varInfo as Unflattenable if varValue is.  This done after all the
       * other parts of the analysis so that varInfo is consistent with the
       * varValue.
       *)
      val () =
         Program.foreachVar
         (program, fn (x, _) =>
          let
             val r = varInfo x
          in
             case !r of
                Flattenable _ =>
                   (case Value.deObject (varValue x) of
                       NONE => ()
                     | SOME (Obj {flat, ...}) =>
                          (case !flat of
                              Flat.NotFlat => r := Unflattenable
                            | _ => ()))
              | Unflattenable => ()
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
                 (seq [Var.layout x, str " ",
                       record [("value", Value.layout (varValue x)),
                               ("varInfo", VarInfo.layout (! (varInfo x)))]]))
          in
             ()
          end)
      (* Conversion from values to types. *)
      datatype z = datatype Finish.t
      val traceValueType = 
         Trace.trace ("RefFlatten.valueType", Value.layout, Type.layout)
      fun valueType arg: Type.t =
         traceValueType
         (fn (v: Value.t) =>
         let
            datatype z = datatype Value.value
         in
            case Value.value v of
               Ground t => t
             | Object z => objectType z
             | Weak {arg, finalType, ...} =>
                  Ref.memoize (finalType, fn () => Type.weak (valueType arg))
         end) arg
      and objectFinalComponents (obj as Obj {args, finalComponents, ...}) =
         Ref.memoize
         (finalComponents, fn () =>
          Prod.make
          (Vector.fromList
           (Vector.foldr
            (Prod.dest args, [], fn ({elt, isMutable = i}, ac) =>
             case Value.deFlat {inner = elt, outer = obj} of
                NONE => {elt = valueType elt, isMutable = i} :: ac
              | SOME z => 
                   Vector.foldr
                   (Prod.dest (objectFinalComponents z), ac,
                    fn ({elt, isMutable = i'}, ac) =>
                    {elt = elt, isMutable = i orelse i'} :: ac)))))
      and objectFinalOffsets (z as Obj {args, finalOffsets, flat, ...}) =
         Ref.memoize
         (finalOffsets, fn () =>
          let
             val initial =
                case ! flat of
                   Flat.Offset {object, offset} => objectOffset (object, offset)
                 | _ => 0
             val (_, offsets) =
                Vector.fold
                (Prod.dest args, (initial, []), fn ({elt, ...}, (offset, ac)) =>
                 let
                    val width =
                       case Value.deFlat {inner = elt, outer = z} of
                          NONE => 1
                        | SOME z => Prod.length (objectFinalComponents z)
                 in
                    (offset + width, offset :: ac)
                 end)
          in
             Vector.fromListRev offsets
          end)
      and objectOffset (z: Object.t, offset: int): int =
         Vector.sub (objectFinalOffsets z, offset)
      and objectType (z as Obj {con, finalType, flat, ...}): Type.t =
         Ref.memoize
         (finalType, fn () =>
          case ! flat of
             Flat.Offset {object, ...} => objectType object
           | _ => Type.object {args = objectFinalComponents z,
                               con = con})
      (* Transform the program. *)
      fun transformFormals (xts: (Var.t * Type.t) vector)
         : (Var.t * Type.t) vector =
         Vector.map (xts, fn (x, _) => (x, valueType (varValue x)))
      val extraSelects: Statement.t list ref = ref []
      fun flattenValues (object: Var.t,
                         obj as Obj {args, ...},
                         ac: Var.t list): Var.t list =
         Vector.foldri
         (Prod.dest args, ac, fn (i, {elt, ...}, ac) =>
          case Value.deFlat {inner = elt, outer = obj} of
             NONE => 
                let
                   val var = Var.newNoname ()
                   val () =
                      List.push
                      (extraSelects,
                       Bind
                       {exp = Select {base = Base.Object object,
                                      offset = objectOffset (obj, i)},
                        ty = valueType elt,
                        var = SOME var})
                in
                   var :: ac
                end
           | SOME obj => flattenValues (object, obj, ac))
      fun flattenArgs (xs: Var.t vector, outer: Object.t, ac): Var.t list =
         Vector.foldr
         (xs, ac, fn (x, ac) =>
          let
             val v = varValue x
          in
             case Value.deFlat {inner = v, outer = outer} of
                NONE => x :: ac
              | SOME obj =>
                   (case ! (varInfo x) of
                       Flattenable {components, ...} =>
                          flattenArgs (components, obj, ac)
                     | Unflattenable => flattenValues (x, obj, ac))
          end)
      val flattenArgs =
         Trace.trace3 ("RefFlatten.flattenArgs",
                       Vector.layout Var.layout,
                       Object.layout,
                       List.layout Var.layout,
                       List.layout Var.layout)
         flattenArgs
      fun transformBind {exp, ty, var}: Statement.t vector =
         let
            fun make e =
               Vector.new1
               (Bind {exp = e,
                      ty = (case var of
                               NONE => ty
                             | SOME var => valueType (varValue var)),
                      var = var})
            fun none () = Vector.new0 ()
         in
            case exp of
               Exp.Object {args, con} =>
                  (case var of
                      NONE => none ()
                    | SOME var =>
                         (case varObject var of
                             NONE => make exp
                           | SOME (z as Obj {flat, ...}) =>
                                case ! flat of
                                   Flat.Offset _ => none ()
                                 | _ =>
                                      let
                                         val args =
                                            Vector.fromList
                                            (flattenArgs (args, z, []))
                                         val extra = !extraSelects
                                         val () = extraSelects := []
                                      in
                                         Vector.concat
                                         [Vector.fromList extra,
                                          make (Exp.Object
                                                {args = args, con = con})]
                                      end))
             | PrimApp {args, prim} =>
                  make (PrimApp {args = args, prim = prim})
             | Select {base, offset} =>
                  (case var of
                      NONE => none ()
                    | SOME var =>
                         (case base of
                             Base.Object object =>
                                (case varObject object of
                                    NONE => make exp
                                  | SOME obj => 
                                       make
                                       (if isSome (Value.deFlat
                                                   {inner = varValue var,
                                                    outer = obj})
                                           then Var object
                                        else (Select
                                              {base = base,
                                               offset = (objectOffset
                                                         (obj, offset))})))
                           | Base.VectorSub _ => make exp))
             | _ => make exp
         end
      fun transformStatement (s: Statement.t): Statement.t vector =
         case s of
            Bind b => transformBind b
          | Profile _ => Vector.new1 s
          | Update {base, offset, value} =>
               Vector.new1
               (case base of
                   Base.Object object =>
                      (case varObject object of
                          NONE => s
                        | SOME obj =>
                             let
                                val base =
                                   case ! (varInfo object) of
                                      Flattenable {useStatus, ...} =>
                                         (case ! useStatus of
                                             InTuple {objectVar, ...} =>
                                                Base.Object objectVar
                                           | _ => base)
                                    | Unflattenable => base
                             in
                                Update {base = base,
                                        offset = objectOffset (obj, offset),
                                        value = value}
                             end)
                 | Base.VectorSub _ => s)
      val transformStatement =
         Trace.trace ("RefFlatten.transformStatement",
                      Statement.layout,
                      Vector.layout Statement.layout)
         transformStatement
      fun transformStatements ss =
         Vector.concatV (Vector.map (ss, transformStatement))
      fun transformBlock (Block.T {args, label, statements, transfer}) =
            Block.T {args = transformFormals args,
                     label = label,
                     statements = transformStatements statements,
                     transfer = transfer}
      fun valuesTypes vs = Vector.map (vs, valueType)
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
                             case Type.dest (valueType v) of
                                Type.Object {args, ...} => args
                              | _ => Error.bug "RefFlatten.datatypes: strange con"
                 in
                    {args = args, con = con}
                 end)
          in
             Datatype.T {cons = cons, tycon = tycon}
          end)
      fun transformFunction (f: Function.t): Function.t =
          let
             val {args, blocks, mayInline, name, start, ...} = Function.dest f
             val {raises, returns, ...} = func name
             val raises = Option.map (raises, valuesTypes)
             val returns = Option.map (returns, valuesTypes)
          in
             Function.new {args = transformFormals args,
                           blocks = Vector.map (blocks, transformBlock),
                           mayInline = mayInline,
                           name = name,
                           raises = raises,
                           returns = returns,
                           start = start}
          end
      val program =
         Program.T {datatypes = datatypes,
                    functions = List.revMap (functions, transformFunction),
                    globals = transformStatements globals,
                    main = main}
      val () = Program.clear program
   in
      shrink program
   end

end
