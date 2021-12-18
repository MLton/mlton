(* Copyright (C) 2017,2019-2021 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(*
 * Invariant: Created globals only refer to other globals.
 *            Hence, the newly created globals may appear at the
 *            beginning of the program.
 *
 * Circular abstract values can arise as a result of programs like:
 *   datatype t = T of t
 *   fun f () = T (f ())
 *   val _ = f ()
 * There is special code in printing abstract values and in determining whether
 * they are global in order to avoid infinite loops.
 *)

functor ConstantPropagation (S: SSA_TRANSFORM_STRUCTS) : SSA_TRANSFORM = 
struct

open S

structure Multi = Multi (S)
structure Global = Global (S)

structure UniqueId = UniqueId ()

structure Graph = DirectedGraph
structure Node = Graph.Node
structure Size = TwoPointLattice (val bottom = "small"
                                  val top = "large")

open Exp Transfer

structure Value =
   struct
      datatype global =
         NotComputed
       | No
       | Yes of Var.t

      structure Const =
         FlatLatticeMono(structure Point = S.Const
                         val bottom = "undefined constant"
                         val top = "unknown constant")
      structure Const =
         struct
            open Const
            val undefined = Const.newBottom
            val const = Const.newPoint
            val getConst = Const.getPoint
            val unknown = Const.newTop
            val makeUnknown = Const.makeTop

            val coerce =
               Trace.trace
               ("ConstantPropagation.Value.Const.coerce",
                fn {from, to} => Layout.record [("from", layout from),
                                                ("to", layout to)],
                Unit.layout)
               coerce

            val unify =
               Trace.trace2
               ("ConstantPropagation.Value.Const.unify",
                layout, layout, Unit.layout)
               unify
         end

      structure ConApp =
         struct
            datatype 'a t = T of {args: 'a vector,
                                  con: Con.t,
                                  unique: UniqueId.t}

            fun new {args, con} = T {args = args,
                                     con = con,
                                     unique = UniqueId.new ()}

            fun equals (T {args = args1,
                           con = con1,
                           unique = unique1},
                        T {con = con2,
                           unique = unique2, ...}) =
               UniqueId.equals (unique1, unique2)
               orelse (Con.equals (con1, con2)
                       andalso Vector.isEmpty args1)

            fun layout layoutA (T {args, con, ...}) =
               let
                  open Layout
               in
                  record [("con", Con.layout con),
                          ("args", Vector.layout layoutA args)]
               end
         end
      structure Data =
         FlatLatticeParam(structure Point = ConApp
                          val bottom = "undefined datatype"
                          val top = "unknown datatype")
      structure Data =
         struct
            open Data

            val undefined = Data.newBottom
            fun conApp {args, con} =
               Data.newPoint (ConApp.new {args = args, con = con})
            val getConApp = Data.getPoint
            val unknown = Data.newTop
            val makeUnknown = Data.makeTop
         end

      structure One =
         struct
            datatype 'a t = T of {extra: 'a,
                                  global: Var.t option ref}

            local
               fun make f (T r) = f r
            in
               val global = fn z => make #global z
            end

            fun layout layoutA =
               let
                  val optionVarLayout = Option.layout Var.layout
                  open Layout
               in
                  fn T {extra, global} =>
                  seq [str "One ",
                       record [("extra", layoutA extra),
                               ("global", optionVarLayout (!global))]]
               end

            fun new (a: 'a): 'a t = T {extra = a,
                                       global = ref NONE}

            val equals: 'a t * 'a t -> bool = 
               fn (n, n') => global n = global n'
         end
      structure Birth = FlatLatticeParam(structure Point = One
                                         val bottom = "undefined birth"
                                         val top = "unknown birth")
      structure Birth =
         struct
            open Birth

            val undefined = Birth.newBottom
            fun here (a: 'a): 'a t = Birth.newPoint (One.new a)
            val getOne = Birth.getPoint
            val unknown = Birth.newTop
         end

      structure ArrayInit =
         struct
            datatype 'a t =
               Alloc of {raw: bool}
             | Array of {args: 'a vector}

            fun layout layoutA ai =
               let
                  open Layout
               in
                  case ai of
                     Alloc {raw} =>
                        seq [str "Alloc ",
                             record [("raw", Bool.layout raw)]]
                   | Array {args} =>
                        seq [str "Array ",
                             record [("args",
                                      Vector.layout layoutA args)]]
               end
         end
      structure ArrayBirth =
         struct
            open Birth
            type 'a t = 'a ArrayInit.t Birth.t
            val layout = fn layoutA => Birth.layout (ArrayInit.layout layoutA)
         end
      structure RefInit =
         struct
            type 'a t = {arg: 'a}

            fun layout layoutA {arg} =
               let
                  open Layout
               in
                  record [("arg", layoutA arg)]
               end
         end
      structure RefBirth =
         struct
            open Birth
            type 'a t = 'a RefInit.t Birth.t
            val layout = fn layoutA => Birth.layout (RefInit.layout layoutA)
         end

      structure Sequence =
         struct
            structure Elts =
               FlatLatticeRec(structure Point =
                                 struct
                                    type 'a t = 'a vector
                                    fun equals equalsA (xs, ys) =
                                       Vector.equals (xs, ys, equalsA)
                                    fun layout layoutA xs =
                                       Vector.layout layoutA xs
                                    fun clone {clone = cloneA, equals = _} xs =
                                       Vector.map (xs, cloneA)
                                    fun coerce {clone = _, coerce = coerceA, equals = _} {from, to} =
                                       if Vector.length from = Vector.length to
                                          then (Vector.foreach2 (from, to, fn (from, to) =>
                                                                 coerceA {from = from, to = to})
                                                ; true)
                                          else false
                                    fun unify {equals = _, unify = unifyA} (xs1, xs2) =
                                       if Vector.length xs1 = Vector.length xs2
                                          then (Vector.foreach2 (xs1, xs2, unifyA)
                                                ; true)
                                          else false
                                 end
                              val bottom = "undefined elts"
                              val top = "unknown elts")
            structure Elts =
               struct
                  open Elts

                  val undefined = Elts.newBottom
                  val elts = Elts.newPoint
                  val getElts = Elts.getPoint
                  val unknown = Elts.newTop
                  val makeUnknown = Elts.makeTop
               end
            datatype 'a t = T of {eltLb: 'a,
                                  elts: 'a Elts.t,
                                  eltUb: 'a,
                                  length: 'a}

            local
               fun make sel (T fs) = sel fs
            in
               fun eltLb s = make #eltLb s
               fun elts s = make #elts s
               fun eltUb s = make #eltUb s
               fun length s = make #length s
            end

            fun layout layoutA (T {eltLb, elts, eltUb, length}) =
               Layout.record [("eltLb", layoutA eltLb),
                              ("elts", Elts.layout layoutA elts),
                              ("eltUb", layoutA eltUb),
                              ("length", layoutA length)]

            fun unknown {unknown = unknownA: Type.t -> 'a} =
               fn eltTy =>
               let
                  val length = unknownA (Type.word (WordSize.seqIndex ()))
                  val eltLb = unknownA eltTy
                  val elts = Elts.unknown ()
                  val eltUb = unknownA eltTy
               in
                  T {eltLb = eltLb,
                     elts = elts,
                     eltUb = eltUb,
                     length = length}
               end

            fun undefined {clone = cloneA: 'a -> 'a,
                           coerce = coerceA: {from: 'a, to: 'a} -> unit,
                           deConst: 'a -> Const.t option,
                           equals = equalsA: 'a * 'a -> bool,
                           unify = unifyA: 'a * 'a -> unit,
                           undefined = undefinedA: Type.t -> 'a} =
               let
                  val eltsUnify =
                     Elts.unify
                     {clone = cloneA,
                      coerce = coerceA,
                      equals = equalsA,
                      unify = unifyA}
               in
                  fn eltTy =>
                  let
                     val length = undefinedA (Type.word (WordSize.seqIndex ()))
                     val lengthConst =
                        case deConst length of
                           NONE => Error.bug "ConstantPropagation.Value.Sequence.new: deConst length"
                         | SOME lengthConst => lengthConst

                     val eltLb = undefinedA eltTy
                     val elts = Elts.undefined ()
                     val eltUb = undefinedA eltTy

                     val () = coerceA {from = eltLb, to = eltUb}
                     fun doit v =
                        case v of
                           Const.Value.Bottom => ()
                         | Const.Value.Point c =>
                              Exn.withEscape
                              (fn escape =>
                               let
                                  val length =
                                     case S.Const.deWordOpt c of
                                        NONE => Error.bug "ConstantPropagation.Value.Sequence.new: S.Const.deWordOpt c"
                                      | SOME w =>
                                           let
                                              val length = WordX.toIntInf w
                                           in
                                              if length <= 0x1000
                                                 then IntInf.toInt length
                                                 else (Elts.makeUnknown elts
                                                       ; escape ())
                                           end
                                  val elts' =
                                     Vector.tabulate
                                     (length, fn _ =>
                                      let
                                         val elt = undefinedA eltTy
                                         val _ = coerceA {from = eltLb, to = elt}
                                         val _ = coerceA {from = elt, to = eltUb}
                                      in
                                         elt
                                      end)
                                  val () = eltsUnify (elts, Elts.elts elts')
                               in
                                  ()
                               end)
                         | Const.Value.Top => Elts.makeUnknown elts
                     val doit =
                        Trace.trace
                        ("ConstantPropagation.Value.Sequence.new.doit",
                         Const.Value.layout, Unit.layout)
                        doit
                     val () = Const.addHandler' (lengthConst, doit)
                  in
                     T {eltLb = eltLb,
                        elts = elts,
                        eltUb = eltUb,
                        length = length}
                  end
               end

            fun makeUnknown (makeUnknownA: 'a -> unit) (T {eltLb, length, ...}) =
               (makeUnknownA length; makeUnknownA eltLb)

            fun coerceElts {clone = cloneA, coerce = coerceA, equals = equalsA} =
               fn {from = T {eltLb = eltLbFrom, elts = eltsFrom, eltUb = eltUbFrom, ...},
                   to = T {eltLb = eltLbTo, elts = eltsTo, eltUb = eltUbTo, ...}} =>
               let
                  val eltsCoerce = Elts.coerce {clone = cloneA,
                                                coerce = coerceA,
                                                equals = equalsA}
               in
                  coerceA {from = eltLbFrom, to = eltLbTo}
                  ; eltsCoerce {from = eltsFrom, to = eltsTo}
                  ; coerceA {from = eltUbFrom, to = eltUbTo}
               end

            fun coerce {clone = cloneA, coerce = coerceA, equals = equalsA} =
               fn {from = from as T {length = lengthFrom, ...},
                   to = to as T {length = lengthTo, ...}} =>
               let
                  val coerceElts = coerceElts {clone = cloneA,
                                               coerce = coerceA,
                                               equals = equalsA}
               in
                  (coerceA {from = lengthFrom, to = lengthTo}
                   ; coerceElts {from = from, to = to})
               end

            fun unifyElts {clone = cloneA, coerce = coerceA, equals = equalsA, unify = unifyA} =
               fn (T {eltLb = eltLb1, elts = elts1, eltUb = eltUb1, ...},
                   T {eltLb = eltLb2, elts = elts2, eltUb = eltUb2, ...}) =>
               let
                  val eltsUnify = Elts.unify {clone = cloneA,
                                              coerce = coerceA,
                                              equals = equalsA,
                                              unify = unifyA}
               in
                  unifyA (eltLb1, eltLb2)
                  ; eltsUnify (elts1, elts2)
                  ; unifyA (eltUb1, eltUb2)
               end

            fun coerceLengthUnifyElts {clone = cloneA, coerce = coerceA, equals = equalsA, unify = unifyA} =
               fn {from = from as T {length = lengthFrom, ...},
                   to = to as T {length = lengthTo, ...}} =>
               let
                  val unifyElts = unifyElts {clone = cloneA,
                                             coerce = coerceA,
                                             equals = equalsA,
                                             unify = unifyA}
               in
                  (coerceA {from = lengthFrom, to = lengthTo}
                   ; unifyElts (from, to))
               end

            fun unify {clone = cloneA, coerce = coerceA, equals = equalsA, unify = unifyA} =
               fn (seq1 as T {length = length1, ...},
                   seq2 as T {length = length2, ...}) =>
               let
                  val unifyElts = unifyElts {clone = cloneA,
                                             coerce = coerceA,
                                             equals = equalsA,
                                             unify = unifyA}
               in
                  (unifyA (length1, length2)
                   ; unifyElts (seq1, seq2))
               end
         end

      structure Set = DisjointSet

      datatype t =
         T of {global: global ref,
               ty: Type.t,
               value: value} Set.t
      and value =
         Array of {birth: t ArrayBirth.t,
                   sequence: t Sequence.t}
       | Const of Const.t
       | Datatype of t Data.t
       | Ref of {arg: t,
                 birth: t RefBirth.t}
       | Tuple of t vector
       | Vector of {sequence: t Sequence.t}
       | Weak of t

      local
         fun make sel (T s) = sel (Set.! s)
      in
         val value = make #value
         val ty = make #ty
      end

      fun equals (T s, T s') = Set.equals (s, s')

      local
         open Layout
         fun layout (seen, v) =
            if List.contains (seen, v, equals)
               then str "$$$"
            else if List.length seen > !Control.constPropAbsValLayoutDepth
               then str "..."
               else let
                       val seen' = v::seen
                       val layout = fn v' => layout (seen', v')
                    in
                       case value v of
                          Array {birth, sequence, ...} =>
                             seq [str "array ",
                                  tuple [ArrayBirth.layout layout birth,
                                         Sequence.layout layout sequence]]
                        | Const c => Const.layout c
                        | Datatype d => Data.layout layout d
                        | Ref {arg, birth, ...} =>
                             seq [str "ref ",
                                  tuple [layout arg,
                                         RefBirth.layout layout birth]]
                        | Tuple vs => Vector.layout layout vs
                        | Vector {sequence, ...} =>
                             seq [str "vector ",
                                  tuple [Sequence.layout layout sequence]]
                        | Weak v => seq [str "weak ", layout v]
                    end
      in
         val layout = fn v => layout ([], v)
      end

      val equals =
         Trace.trace2 
         ("ConstantPropagation.Value.equals", 
          layout, layout, Bool.layout) 
         equals

      local
         structure Value =
            struct
               type t = t
               val layout = layout
            end
         structure AbsValue = Value
      in
         structure Data =
            struct
               open Data
               type t = AbsValue.t Data.t
               val layout = Data.layout AbsValue.layout
               val coerce: {from: t, to: t} -> unit =
                  Trace.trace
                  ("ConstantPropagation.Value.Data.coerce",
                   fn {from, to} =>
                   Layout.record [("from", layout from),
                                  ("to", layout to)],
                   Unit.layout)
                  Data.coerce
               val unify: t * t -> unit =
                  Trace.trace2
                  ("ConstantPropagation.Value.Data.unify",
                   layout, layout, Unit.layout)
                  Data.unify
            end
         structure ArrayBirth =
            struct
               open ArrayBirth
               type t = AbsValue.t ArrayBirth.t
               val layout = ArrayBirth.layout AbsValue.layout
               val coerce: {from: t, to: t} -> unit =
                  Trace.trace
                  ("ConstantPropagation.Value.ArrayBirth.coerce",
                   fn {from, to} =>
                   Layout.record [("from", layout from),
                                  ("to", layout to)],
                   Unit.layout)
                  Birth.coerce
               val unify: t * t -> unit =
                  Trace.trace2
                  ("ConstantPropagation.Value.ArrayBirth.unify",
                   layout, layout, Unit.layout)
                  Birth.unify
            end
         structure RefBirth =
            struct
               open RefBirth
               type t = AbsValue.t RefBirth.t
               val layout = RefBirth.layout AbsValue.layout
               val coerce: {from: t, to: t} -> unit =
                  Trace.trace
                  ("ConstantPropagation.Value.RefBirth.coerce",
                   fn {from, to} =>
                   Layout.record [("from", layout from),
                                  ("to", layout to)],
                   Unit.layout)
                  Birth.coerce
               val unify: t * t -> unit =
                  Trace.trace2
                  ("ConstantPropagation.Value.RefBirth.unify",
                   layout, layout, Unit.layout)
                  Birth.unify
            end
         structure Sequence =
            struct
               open Sequence
               type t = Value.t Sequence.t
               structure Elts =
                  struct
                     open Sequence.Elts
                     val layout = Sequence.Elts.layout AbsValue.layout
                  end
               val layout = Sequence.layout Value.layout
            end
      end


      fun new (value: value, ty: Type.t): t =
         T (Set.singleton {global = ref NotComputed,
                           ty = ty,
                           value = value})

      fun const c = new (Const (Const.const c), Type.ofConst c)
      fun deConst v =
         case value v of
            Const const => SOME const
          | _ => NONE
      fun deConst' v =
         case deConst v of
            NONE => NONE
          | SOME const => Const.getConst const


      local
         (* The extra birth is because of let-style polymorphism.
          * arrayBirth is really the same as refBirth.
          *)
         fun make {arrayBirth, const, data, refBirth, sequence} =
            let
               fun loop (t: Type.t): t =
                  new
                  (case Type.dest t of
                      Type.Array t => Array {birth = arrayBirth (), sequence = sequence loop t}
                    | Type.Datatype _ => Datatype (data ())
                    | Type.Ref t => Ref {arg = loop t, birth = refBirth ()}
                    | Type.Tuple ts => Tuple (Vector.map (ts, loop))
                    | Type.Vector t => Vector {sequence = sequence loop t}
                    | Type.Weak t => Weak (loop t)
                    | _ => Const (const ()),
                   t)
            in loop
            end
      in
         val mkFromType =
            fn {clone, coerce, unify} =>
            make {arrayBirth = ArrayBirth.undefined,
                  const = Const.undefined,
                  data = Data.undefined,
                  refBirth = RefBirth.undefined,
                  sequence = fn undefined => Sequence.undefined {clone = clone,
                                                                 coerce = coerce,
                                                                 deConst = deConst,
                                                                 equals = equals,
                                                                 undefined = undefined,
                                                                 unify = unify}}
         val unknown =
            make {arrayBirth = ArrayBirth.unknown,
                  const = Const.unknown,
                  data = Data.unknown,
                  refBirth = RefBirth.unknown,
                  sequence = fn unknown => Sequence.unknown {unknown = unknown}}
      end

      local
      val traceFromType =
         Trace.trace ("ConstantPropagation.Value.fromType",
                      Type.layout,
                      layout)
      val traceClone =
         Trace.trace
         ("ConstantPropagation.Value.clone",
          layout, layout)
      val traceCoerce =
         Trace.trace ("ConstantPropagation.Value.coerce",
                      fn {from, to} => Layout.record [("from", layout from),
                                                      ("to", layout to)],
                      Unit.layout)
      val traceCoerces =
         Trace.trace ("ConstantPropagation.Value.coerces",
                      fn {froms, tos} => Layout.record [("from", Vector.layout layout froms),
                                                        ("to", Vector.layout layout tos)],
                      Unit.layout)
      val traceSequenceCoerce =
         Trace.trace
         ("ConstantPropagation.Value.Sequence.coerce",
          fn {from, to} =>
          Layout.record [("from", Sequence.layout from),
                         ("to", Sequence.layout to)],
          Unit.layout)
      val traceSequenceCoerceLengthUnifyElts =
         Trace.trace
         ("ConstantPropagation.Value.Sequence.coerceLengthUnifyElts",
          fn {from, to} =>
          Layout.record [("from", Sequence.layout from),
                         ("to", Sequence.layout to)],
          Unit.layout)
      val traceUnify =
         Trace.trace2 ("ConstantPropagation.Value.unify",
                       layout, layout,
                       Unit.layout)
      val traceSequenceUnify =
         Trace.trace
         ("ConstantPropagation.Value.sequenceUnify",
          fn (seq1, seq2) =>
          let
             open Layout
          in
             tuple [Sequence.layout seq1,
                    Sequence.layout seq2]
          end,
          Unit.layout)

      fun fromType ty =
         traceFromType
         (mkFromType {clone = clone, coerce = coerce, unify = unify})
         ty
      and clone v =
         traceClone
         (fromType o ty)
         v
      and coerce arg =
         traceCoerce
         (fn {from, to} =>
          if equals (from, to)
             then ()
             else
                let
                   fun error () =
                      Error.bug
                      (concat ["ConstantPropagation.Value.coerce: strange: from: ",
                               Layout.toString (layout from),
                               " to: ", Layout.toString (layout to)])
                in
                   case (value from, value to) of
                      (Array {birth = birthFrom, sequence = sequenceFrom},
                       Array {birth = birthTo, sequence = sequenceTo}) =>
                         (ArrayBirth.coerce {from = birthFrom, to = birthTo}
                          ; sequenceCoerceLengthUnifyElts {from = sequenceFrom, to = sequenceTo})
                    | (Const from, Const to) =>
                         Const.coerce {from = from, to = to}
                    | (Datatype from, Datatype to) =>
                         Data.coerce {from = from, to = to}
                    | (Ref {birth = birthFrom, arg = argFrom},
                       Ref {birth = birthTo, arg = argTo}) =>
                         (RefBirth.coerce {from = birthFrom, to = birthTo}
                          ; unify (argFrom, argTo))
                    | (Tuple froms, Tuple tos) =>
                         coerces {froms = froms, tos = tos}
                    | (Vector {sequence = sequenceFrom},
                       Vector {sequence = sequenceTo}) =>
                         sequenceCoerce {from = sequenceFrom, to = sequenceTo}
                    | (Weak from, Weak to) => unify (from, to)
                    | (_, _) => error ()
                end) arg
      and coerces arg =
         traceCoerces
         (fn {froms: t vector, tos: t vector} =>
          Vector.foreach2 (froms, tos, fn (from, to) =>
                           coerce {from = from, to = to}))
         arg
      and sequenceCoerce arg =
         traceSequenceCoerce
         (fn {from, to} =>
          Sequence.coerce
          {clone = clone,
           coerce = coerce,
           equals = equals}
          {from = from, to = to})
         arg
      and sequenceCoerceLengthUnifyElts arg =
         traceSequenceCoerceLengthUnifyElts
         (fn {from, to} =>
          Sequence.coerceLengthUnifyElts
          {clone = clone,
           coerce = coerce,
           equals = equals,
           unify = unify}
          {from = from, to = to})
         arg
      and unify arg: unit =
         traceUnify
         (fn (v1, v2) =>
          if equals (v1, v2)
             then ()
             else
                let
                   val (T s1, T s2) = (v1, v2)
                   val {global, ty, value = value1, ...} = Set.! s1
                   val {value = value2, ...} = Set.! s2
                   fun error () =
                      Error.bug
                      (concat ["ConstantPropagation.Value.unify: strange: value1: ",
                               Layout.toString (layout v1),
                               " value2: ", Layout.toString (layout v2)])
                   val _ = Set.union (s1, s2)
                   val _ = Set.:= (s1, {global = global,
                                        ty = ty,
                                        value = value1})
                in
                   case (value1, value2) of
                      (Array {birth = birth1, sequence = sequence1},
                       Array {birth = birth2, sequence = sequence2}) =>
                         (ArrayBirth.unify (birth1, birth2)
                          ; sequenceUnify (sequence1, sequence2))
                    | (Const c1, Const c2) => Const.unify (c1, c2)
                    | (Datatype d1, Datatype d2) => Data.unify (d1, d2)
                    | (Ref {birth = birth1, arg = arg1},
                       Ref {birth = birth2, arg = arg2}) =>
                         (RefBirth.unify (birth1, birth2)
                          ; unify (arg1, arg2))
                    | (Tuple vs1, Tuple vs2) => Vector.foreach2 (vs1, vs2, unify)
                    | (Vector {sequence = sequence1},
                       Vector {sequence = sequence2}) =>
                         sequenceUnify (sequence1, sequence2)
                    | (Weak v1, Weak v2) => unify (v1, v2)
                    | _ => error ()
                end)
         arg
      and sequenceUnify args =
         traceSequenceUnify
         (fn (seq1, seq2) =>
          Sequence.unify
          {clone = clone,
           coerce = coerce,
           equals = equals,
           unify = unify}
          (seq1, seq2))
         args
      in
         val clone = clone
         val coerce = coerce
         val coerces = coerces
         val fromType = fromType
         val unify = unify
      end

      fun makeUnknown (v: t): unit =
         case value v of
            Array {sequence, ...} => Sequence.makeUnknown makeUnknown sequence
          | Const c => Const.makeUnknown c
          | Datatype d => Data.makeUnknown d
          | Ref {arg, ...} => makeUnknown arg
          | Tuple vs => Vector.foreach (vs, makeUnknown)
          | Vector {sequence} => Sequence.makeUnknown makeUnknown sequence
          | Weak v => makeUnknown v

      fun sideEffect (v: t): unit =
         case value v of
            Array {sequence, ...} => makeUnknown (Sequence.eltLb sequence)
          | Const _ => ()
          | Datatype _ => ()
          | Ref {arg, ...} => makeUnknown arg
          | Vector _ => ()
          | Tuple vs => Vector.foreach (vs, sideEffect)
          | Weak v => makeUnknown v

      local
         structure Value =
            struct
               val clone = clone
               val coerce = coerce
               val const = const
               val deConst = deConst
               val equals = equals
               val fromType = fromType
               val undefined = fromType
               val unify = unify
            end
      in
         structure Sequence =
            struct
               open Sequence
               val undefined: Type.t -> t =
                  Sequence.undefined {clone = Value.clone,
                                      coerce = Value.coerce,
                                      deConst = Value.deConst,
                                      equals = Value.equals,
                                      undefined = Value.undefined,
                                      unify = Value.unify}
               fun make (args, eltTy) =
                  let
                     val length =
                        Value.const (S.Const.word (WordX.fromInt (Vector.length args, WordSize.seqIndex ())))
                     val lengthConst =
                        case deConst length of
                           NONE => Error.bug "ConstantPropagation.Value.Sequence.elts: deConst length"
                         | SOME lengthConst => lengthConst

                     val eltLb = Value.undefined eltTy
                     val eltUb = Value.undefined eltTy
                     val () = Value.coerce {from = eltLb, to = eltUb}
                     val elts =
                        (Elts.elts o Vector.map)
                        (args, fn arg =>
                         let
                            val elt = Value.clone arg
                            val () = Value.coerce {from = arg, to = elt}
                            val () = Value.coerce {from = eltLb, to = elt}
                            val () = Value.coerce {from = elt, to = eltUb}
                         in
                            elt
                         end)
                     val () =
                        Const.addHandler'
                        (lengthConst, fn v =>
                         case v of
                            Const.Value.Top => Elts.makeUnknown elts
                          | _ => ())
                  in
                     T {eltLb = eltLb,
                        elts = elts,
                        eltUb = eltUb,
                        length = length}
                  end
            end
      end

      local
         fun make (err, sel) v =
            case value v of
               Array fs => sel fs
             | _ => Error.bug err
      in
         val arrayLength = make ("ConstantPropagation.Value.arrayLength", Sequence.length o #sequence)
         val arraySequence = make ("ConstantPropagation.Value.arraySequence", #sequence)
      end

      fun bool b =
         new (Datatype (Data.conApp {args = Vector.new0 (),
                                     con = Con.fromBool b}),
              Type.bool)

      val const = fn c =>
         case c of
            S.Const.WordVector v =>
               let
                  val eltTy = Type.word (WordXVector.elementSize v)
                  val vecTy = Type.vector eltTy
                  val args = WordXVector.toVectorMap (v, const o S.Const.word)
                  val seq = Sequence.make (args, eltTy)
               in
                  new (Vector {sequence = seq}, vecTy)
               end
          | _ => const c

      local
         fun make (err, sel) v =
            case value v of
               Ref fs => sel fs
             | _ => Error.bug err
      in
         val refArg = make ("ConstantPropagation.Value.refArg", #arg)
         val refBirth = make ("ConstantPropagation.Value.refBirth", #birth)
      end

      fun select {tuple, offset, resultType = _} =
         case value tuple of
            Tuple vs => Vector.sub (vs, offset)
          | _ => Error.bug "ConstantPropagation.Value.select: non-tuple"

      fun tuple vs = new (Tuple vs, Type.tuple (Vector.map (vs, ty)))

      fun unit () = tuple (Vector.new0 ())

      local
         fun make (err, sel) v =
            case value v of
               Vector fs => sel fs
             | _ => Error.bug err
      in
         val vectorLength = make ("ConstantPropagation.Value.vectorLength", Sequence.length o #sequence)
         val vectorSequence = make ("ConstantPropagation.Value.vectorSequence", #sequence)
      end

      fun weakArg v =
         case value v of
            Weak v => v
          | _ => Error.bug "ConstantPropagation.Value.weakArg"


      fun globals arg: (Var.t * Type.t) vector option =
         Trace.trace
         ("ConstantPropagation.Value.globals",
          (Vector.layout layout) o #1,
          Option.layout (Vector.layout (Var.layout o #1)))
         (fn (vs: t vector, isSmallType, newGlobal) =>
          Exn.withEscape
          (fn escape =>
           SOME (Vector.map
                 (vs, fn v =>
                  case global (v, isSmallType, newGlobal) of
                     NONE => escape NONE
                   | SOME g => g)))) arg
      and global arg: (Var.t * Type.t) option =
         Trace.trace
         ("ConstantPropagation.Value.global",
          layout o #1,
          Option.layout (Var.layout o #1))
         (fn (v as T s, isSmallType, newGlobal) =>
          let val {global = r, ty, value} = Set.! s
          in case !r of
                No => NONE
              | Yes g => SOME (g, ty)
              | NotComputed =>
                   let
                      val global = fn v =>
                         global (v, isSmallType, newGlobal)
                      val globals = fn vs =>
                         globals (vs, isSmallType, newGlobal)
                      (* avoid globalizing circular abstract values *)
                      val _ = r := No
                      fun yes e = Yes (newGlobal (ty, e))
                      fun once (birth, make) =
                         case Birth.getOne birth of
                            SOME (One.T {extra, global = r, ...}) =>
                               (case make extra of
                                   SOME exp =>
                                      Yes (case !r of
                                              NONE => let
                                                         val g = newGlobal (ty, exp)
                                                      in
                                                         r := SOME g; g
                                                      end
                                            | SOME g => g)
                                 | NONE => No)
                          | _ => No
                      fun arrayOnce (birth, length) =
                         let
                            val eltTy = Type.deArray ty
                         in
                            once
                            (birth, fn ab =>
                             if isSmallType ty
                                then (case ab of
                                         ArrayInit.Alloc {raw} =>
                                            (case global length of
                                                NONE => NONE
                                              | SOME (length, _) =>
                                                   SOME (Exp.PrimApp
                                                         {args = Vector.new1 length,
                                                          prim = Prim.Array_alloc {raw = raw},
                                                          targs = Vector.new1 eltTy}))
                                       | ArrayInit.Array {args} =>
                                            (case globals args of
                                                NONE => NONE
                                              | SOME args =>
                                                   SOME (Exp.PrimApp
                                                         {args = Vector.map (args, #1),
                                                          prim = Prim.Array_array,
                                                          targs = Vector.new1 eltTy})))
                                else NONE)
                         end
                      fun refOnce birth =
                         let
                            val argTy = Type.deRef ty
                         in
                            once (birth, fn {arg} =>
                                  if isSmallType ty
                                     then (case global arg of
                                              SOME (arg, _) =>
                                                 SOME (Exp.PrimApp
                                                       {args = Vector.new1 arg,
                                                        prim = Prim.Ref_ref,
                                                        targs = Vector.new1 argTy})
                                            | _ => NONE)
                                     else NONE)
                         end
                      val g =
                         case value of
                            Array {birth, sequence} =>
                               if !Control.globalizeArrays then
                               arrayOnce (birth, Sequence.length sequence)
                               else No
                          | Const const =>
                               (case Const.getConst const of
                                   SOME c => yes (Exp.Const c)
                                 | NONE => No)
                          | Datatype d =>
                               (case Data.getConApp d of
                                   SOME (ConApp.T {args, con, ...}) =>
                                      (case globals args of
                                          NONE => No
                                        | SOME args =>
                                             yes (Exp.ConApp
                                                  {con = con,
                                                   args = Vector.map (args, #1)}))
                                 | _ => No)
                          | Ref {birth, ...} =>
                               if !Control.globalizeRefs then
                               refOnce birth
                               else No
                          | Tuple vs =>
                               (case globals vs of
                                   NONE => No
                                 | SOME xts =>
                                      yes (Exp.Tuple (Vector.map (xts, #1))))
                          | Vector {sequence} =>
                               (case Sequence.Elts.getElts (Sequence.elts sequence) of
                                   NONE => No
                                 | SOME elts =>
                                      let
                                         val eltTy = Type.deVector ty
                                         fun vector () =
                                            case globals elts of
                                               NONE => No
                                             | SOME args =>
                                                  yes (Exp.PrimApp
                                                       {args = Vector.map (args, #1),
                                                        prim = Prim.Vector_vector,
                                                        targs = Vector.new1 eltTy})
                                         fun wordxvector elementSize =
                                            Exn.withEscape
                                            (fn escape =>
                                             let
                                                val ws =
                                                   Vector.map
                                                   (elts, fn elt =>
                                                    case deConst' elt of
                                                       SOME (S.Const.Word w) => w
                                                     | _ => escape No)
                                             in
                                                yes (Exp.Const
                                                     (S.Const.wordVector
                                                      (WordXVector.fromVector
                                                       ({elementSize = elementSize}, ws))))
                                             end)
                                      in
                                         case Type.deWordOpt eltTy of
                                            NONE => vector ()
                                          | SOME ws => wordxvector ws
                                      end)
                          | Weak _ => No
                      val _ = r := g
                   in
                      global v
                   end
          end) arg

      fun arrayToArray (v: t): t =
         case value v of
            Array {sequence, ...} =>
               new (Array {birth = Birth.unknown (), sequence = sequence},
                    ty v)
          | _ => Error.bug "ConstantPropagation.Value.arrayToArray"

      fun arrayToVector (v: t): t =
         case value v of
            Array {sequence, ...} =>
               new (Vector {sequence = sequence},
                    Type.vector (Type.deArray (ty v)))
          | _ => Error.bug "ConstantPropagation.Value.arrayToVector"
   end

(* ------------------------------------------------- *)
(*                     simplify                      *)
(* ------------------------------------------------- *)

fun transform (program: Program.t): Program.t =
   let
      val program as Program.T {datatypes, globals, functions, main} =
         eliminateDeadBlocks program
      val {varIsMultiDefed, ...} = Multi.multi program
      val once = not o varIsMultiDefed
      val {get = conInfo: Con.t -> {result: Type.t,
                                    types: Type.t vector,
                                    values: Value.t vector},
           set = setConInfo, ...} =
         Property.getSetOnce
         (Con.plist, Property.initRaise ("conInfo", Con.layout))
      val conValues = #values o conInfo
      val _ =
         Vector.foreach
         (datatypes, fn Datatype.T {tycon, cons} =>
          let
             val result = Type.datatypee tycon
          in
             Vector.foreach
             (cons, fn {con, args} =>
              setConInfo (con,
                          {result = result,
                           types = args,
                           values = Vector.map (args, Value.fromType)}))
          end)
      local
         open Value
      in
         fun conApp {con: Con.t, args: t vector}: t =
            let
               val {values = tos, result, ...} = conInfo con
            in
               coerces {froms = args, tos = tos}
               ; new (Datatype (Data.conApp {args = args, con = con}), result)
            end
         val conApp =
            Trace.trace
            ("ConstantPropagation.Value.conApp",
             fn {con, args} =>
             Layout.record [("con", Con.layout con),
                            ("args", Vector.layout layout args)],
             layout)
            conApp
         fun filter (variant, con, args) =
            case value variant of
               Datatype d =>
                  Data.addHandler'
                  (d, fn v =>
                   case v of
                      Data.Value.Bottom => ()
                    | Data.Value.Point (ConApp.T {con = con', args = args', ...}) =>
                         if Con.equals (con, con')
                            then coerces {froms = args', tos = args}
                            else ()
                    | Data.Value.Top =>
                         coerces {froms = conValues con, tos = args})
             | _ => Error.bug "ConstantPropagation.Value.filter: non-datatype"
         val filter =
            Trace.trace
            ("ConstantPropagation.Value.filter",
             fn (variant, con, args) =>
             Layout.record [("variant", layout variant),
                            ("con", Con.layout con),
                            ("args", Vector.layout layout args)],
             Unit.layout)
            filter
         fun primApp {prim,
                      targs = _,
                      args: t vector,
                      resultVar,
                      resultType}: t =
            let
               fun bear z =
                  case resultVar of
                     SOME resultVar => if once resultVar 
                                          then Birth.here z
                                       else Birth.unknown ()
                   | _ => Error.bug "ConstantPropagation.Value.primApp.bear"
               fun arg i = Vector.sub (args, i)
               fun sequenceSub deSeq =
                  let
                     val res = fromType resultType
                     val seq = deSeq (arg 0)
                     val elts = Sequence.elts seq
                     val eltUb = Sequence.eltUb seq
                     val idx =
                        case deConst (arg 1) of
                           SOME idx => idx
                         | _ => Error.bug "ConstantPropagation.Value.primApp: sequenceSub, idx"
                     fun doit () =
                        case (Const.value idx, Sequence.Elts.value elts) of
                           (Const.Value.Bottom, _) => ()
                         | (_, Sequence.Elts.Value.Bottom) => ()
                         | (Const.Value.Point idx, Sequence.Elts.Value.Point elts) =>
                              let
                                 val idx = WordX.toIntInf (S.Const.deWord idx)
                              in
                                 if idx < IntInf.fromInt (Vector.length elts)
                                    then coerce {from = Vector.sub (elts, IntInf.toInt idx),
                                                 to = res}
                                    else ()
                              end
                         | _ => coerce {from = eltUb, to = res}
                     val doit =
                        Trace.trace
                        ("ConstantPropagation.Value.primApp.sequenceSub.doit",
                         fn () => Layout.tuple [Const.layout idx,
                                                Sequence.Elts.layout elts,
                                                layout eltUb],
                         Unit.layout)
                        doit
                     val _ = Const.addHandler (idx, doit)
                     val _ = Sequence.Elts.addHandler (elts, doit)
                  in
                     res
                  end
               fun sequenceUpd deSeq =
                  let
                     val seq = deSeq (arg 0)
                     val elts = Sequence.elts seq
                     val eltLb = Sequence.eltLb seq
                     val idx =
                        case deConst (arg 1) of
                           SOME idx => idx
                         | _ => Error.bug "ConstantPropagation.Value.primApp: sequenceUpd, idx"
                     val new = arg 2
                     fun doit () =
                        case (Const.value idx, Sequence.Elts.value elts) of
                           (Const.Value.Bottom, _) => ()
                         | (_, Sequence.Elts.Value.Bottom) => ()
                         | (Const.Value.Point idx, Sequence.Elts.Value.Point elts) =>
                              let
                                 val idx = WordX.toIntInf (S.Const.deWord idx)
                              in
                                 if idx < IntInf.fromInt (Vector.length elts)
                                    then coerce {from = new,
                                                 to = Vector.sub (elts, IntInf.toInt idx)}
                                    else ()
                              end
                         | _ => coerce {from = new, to = eltLb}
                     val doit =
                        Trace.trace
                        ("ConstantPropagation.Value.primApp.sequenceUpd.doit",
                         fn () => Layout.tuple [Const.layout idx,
                                                Sequence.Elts.layout elts,
                                                layout eltLb],
                         Unit.layout)
                        doit
                     val _ = Const.addHandler (idx, doit)
                     val _ = Sequence.Elts.addHandler (elts, doit)
                  in
                     unit ()
                  end
               fun arrayCopy deSeq =
                  let
                     val eltUb = Sequence.eltUb (deSeq (arg 2))
                     val eltLb = Sequence.eltLb (arraySequence (arg 0))
                     val _ = coerce {from = eltUb, to = eltLb}
                  in
                     unit ()
                  end
               fun constFold () =
                  Exn.withEscape
                  (fn escape =>
                   let
                      val (getArgs, addHandlers) =
                         (Vector.unzip o Vector.map)
                         (args, fn arg =>
                          case value arg of
                             Const c =>
                                (fn () =>
                                 case Const.value c of
                                    Const.Value.Bottom => NONE
                                  | Const.Value.Point c =>
                                       SOME (Prim.ApplyArg.Const c)
                                  | Const.Value.Top =>
                                       SOME (Prim.ApplyArg.Var arg),
                                 fn h => Const.addHandler (c, h))
                          | Datatype d =>
                               (fn () =>
                                case Data.value d of
                                   Data.Value.Bottom => NONE
                                 | Data.Value.Point (ConApp.T {con, args, ...}) =>
                                      SOME (Prim.ApplyArg.Con
                                            {con = con,
                                             hasArg = not (Vector.isEmpty args)})
                                 | Data.Value.Top =>
                                      SOME (Prim.ApplyArg.Var arg),
                                fn h => Data.addHandler (d, h))
                          | _ => escape (unknown resultType))
                      val res = fromType resultType
                      fun apply () =
                         Exn.withEscape
                         (fn escape =>
                          let
                             val args =
                                Vector.toListMap
                                (getArgs, fn getArg =>
                                 case getArg () of
                                    NONE => escape ()
                                  | SOME arg => arg)
                             val apply =
                                Trace.trace3
                                ("ConstantPropagation.Value.primApp.apply",
                                 Prim.layout,
                                 List.layout (Prim.ApplyArg.layout layout),
                                 Layout.ignore,
                                 Prim.ApplyResult.layout layout)
                                Prim.apply
                          in
                             case apply (prim, args, fn _ => false) of
                                Prim.ApplyResult.Const c =>
                                   coerce {from = const c, to = res}
                              | Prim.ApplyResult.Bool b =>
                                   coerce {from = bool b, to = res}
                              | Prim.ApplyResult.Var v =>
                                   coerce {from = v, to = res}
                              | _ => makeUnknown res
                          end)
                      val () =
                         Vector.foreach
                         (addHandlers, fn addHandler =>
                          addHandler apply)
                   in
                      res
                   end)
            in
               case prim of
                  Prim.Array_alloc {raw} =>
                     let
                        val birth = bear (ArrayInit.Alloc {raw = raw})
                        val sequence = Sequence.undefined (Type.deArray resultType)
                        val _ = coerce {from = arg 0, to = Sequence.length sequence}
                     in
                        new (Array {birth = birth, sequence = sequence}, resultType)
                     end
                | Prim.Array_array =>
                     let
                        val birth = bear (ArrayInit.Array {args = args})
                        val sequence = Sequence.make (args, Type.deArray resultType)
                     in
                        new (Array {birth = birth, sequence = sequence}, resultType)
                     end
                | Prim.Array_copyArray => arrayCopy arraySequence
                | Prim.Array_copyVector => arrayCopy vectorSequence
                | Prim.Array_length => arrayLength (arg 0)
                | Prim.Array_sub => sequenceSub arraySequence
                | Prim.Array_toArray => arrayToArray (arg 0)
                | Prim.Array_toVector => arrayToVector (arg 0)
                | Prim.Array_update => sequenceUpd arraySequence
                | Prim.Ref_assign => (coerce {from = arg 1, to = refArg (arg 0)}; unit ())
                | Prim.Ref_deref => refArg (arg 0)
                | Prim.Ref_ref =>
                     let
                        val v = arg 0
                        val r = fromType resultType
                        val _ = coerce {from = v, to = refArg r}
                        val _ = RefBirth.coerce {from = bear {arg = v},
                                                 to = refBirth r}
                     in
                        r
                     end
                | Prim.Vector_length => vectorLength (arg 0)
                | Prim.Vector_sub => sequenceSub vectorSequence
                | Prim.Vector_vector =>
                     let
                        val sequence = Sequence.make (args, Type.deVector resultType)
                     in
                        new (Vector {sequence = sequence}, resultType)
                     end
                | Prim.Weak_get => weakArg (arg 0)
                | Prim.Weak_new =>
                     let
                        val w = fromType resultType
                        val _ = coerce {from = arg 0, to = weakArg w}
                     in
                        w
                     end
                | _ => if Prim.isFunctional prim
                          then constFold ()
                          else (if Prim.maySideEffect prim
                                   then Vector.foreach (args, sideEffect)
                                   else ()
                                ; unknown resultType)
            end
         val primApp =
            Trace.trace
            ("ConstantPropagation.Value.primApp",
             fn {prim, targs, args, resultVar, resultType} =>
             Layout.record [("prim", Prim.layout prim),
                            ("targs", Vector.layout Type.layout targs),
                            ("args", Vector.layout layout args),
                            ("resultVar", Option.layout Var.layout resultVar),
                            ("resultType", Type.layout resultType)],
             layout)
            primApp
      end
      fun filterIgnore _ = ()
      val {value, ...} =
         Control.trace (Control.Detail, "fixed point")
         analyze {
                  coerce = Value.coerce,
                  conApp = conApp,
                  const = Value.const,
                  filter = filter,
                  filterWord = filterIgnore,
                  fromType = Value.fromType,
                  layout = Value.layout,
                  primApp = primApp,
                  program = program,
                  select = Value.select,
                  tuple = Value.tuple,
                  useFromTypeOnBinds = false
                  }
      val _ =
         Control.diagnostics
         (fn display =>
          let open Layout
          in 
             display (str "\n\nConstructors:")
             ; (Vector.foreach
                (datatypes, fn Datatype.T {tycon, cons} =>
                 (display (seq [Tycon.layout tycon, str ": "])
                  ; Vector.foreach
                    (cons, fn {con, ...} =>
                     display
                     (seq [Con.layout con, str ": ",
                           Vector.layout Value.layout (conValues con)])))))
             ; display (str "\n\nVariables:")
             ; (Program.foreachVar
                (program, fn (x, _) => display (seq [Var.layout x,
                                                     str " ",
                                                     Value.layout (value x)])))
          end)

      fun mkIsSmallType n =
         let
            datatype t = datatype Type.dest
         in
            case n of
               0 => {isSmallType = fn _ => false,
                     destroyIsSmallType = fn () => ()}
             | 1 => let
                       val {get: Type.t -> bool,
                            destroy} =
                          Property.destGet
                          (Type.plist,
                           Property.initRec
                           (fn (t, get) =>
                            case Type.dest t of
                               Array _ => false
                             | CPointer => true
                             | Datatype _ => false
                             | IntInf => !Control.globalizeSmallIntInf
                             | Real _ => true
                             | Ref t => get t
                             | Thread => false
                             | Tuple ts => Vector.forall (ts, get)
                             | Vector _ => false
                             | Weak _ => true
                             | Word _ => true))
                    in
                       {isSmallType = get,
                        destroyIsSmallType = destroy}
                    end
             | 2 => let
                       val {get = getTycon: Tycon.t -> bool,
                            set = setTycon, ...} =
                          Property.getSetOnce
                          (Tycon.plist,
                           Property.initRaise
                           ("ConstantPropagation.mkIsSmallType(2).getTycon",
                            Tycon.layout))
                       val () =
                          Vector.foreach
                          (datatypes, fn Datatype.T {tycon, cons} =>
                           setTycon
                           (tycon,
                            Vector.forall
                            (cons, fn {args, ...} =>
                             Vector.isEmpty args)))
                       val {get: Type.t -> bool,
                            destroy} =
                          Property.destGet
                          (Type.plist,
                           Property.initRec
                           (fn (t, get) =>
                            case Type.dest t of
                               Array _ => false
                             | CPointer => true
                             | Datatype tc => getTycon tc
                             | IntInf => !Control.globalizeSmallIntInf
                             | Real _ => true
                             | Ref t => get t
                             | Thread => false
                             | Tuple ts => Vector.forall (ts, get)
                             | Vector _ => false
                             | Weak _ => true
                             | Word _ => true))
                    in
                       {isSmallType = get,
                        destroyIsSmallType = destroy}
                    end
             | 3 => let
                       val {isSmallType, destroyIsSmallType} =
                          mkIsSmallType 1
                       val {get = getTycon: Tycon.t -> bool,
                            set = setTycon, ...} =
                          Property.getSetOnce
                          (Tycon.plist,
                           Property.initRaise
                           ("ConstantPropagation.mkIsSmallType(3).getTycon",
                            Tycon.layout))
                       val () =
                          Vector.foreach
                          (datatypes, fn Datatype.T {tycon, cons} =>
                           setTycon
                           (tycon,
                            Vector.forall
                            (cons, fn {args, ...} =>
                             Vector.forall
                             (args, isSmallType))))
                       val () = destroyIsSmallType ()
                       val {get: Type.t -> bool,
                            destroy} =
                          Property.destGet
                          (Type.plist,
                           Property.initRec
                           (fn (t, get) =>
                            case Type.dest t of
                               Array _ => false
                             | CPointer => true
                             | Datatype tc => getTycon tc
                             | IntInf => !Control.globalizeSmallIntInf
                             | Real _ => true
                             | Ref t => get t
                             | Thread => false
                             | Tuple ts => Vector.forall (ts, get)
                             | Vector _ => false
                             | Weak _ => true
                             | Word _ => true))
                    in
                       {isSmallType = get,
                        destroyIsSmallType = destroy}
                    end
             | 4 => let
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
                              val {get = dependsOn, destroy = destroyDependsOn} =
                                 Property.destGet
                                 (Type.plist,
                                  Property.initRec
                                  (fn (t, dependsOn) =>
                                   case Type.dest t of
                                      Array t => dependsOn t
                                    | Datatype tc =>
                                         (ignore o Graph.addEdge)
                                         (graph, {from = n, to = tyconNode tc})
                                    | Ref t => dependsOn t
                                    | Tuple ts => Vector.foreach (ts, dependsOn)
                                    | Vector t => dependsOn t
                                    | _ => ()))
                              val () =
                                 Vector.foreach
                                 (cons, fn {args, ...} =>
                                  Vector.foreach (args, dependsOn))
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
                       val {get = typeSize: Type.t -> Size.t,
                            destroy = destroyTypeSize, ...} =
                          Property.destGet
                          (Type.plist,
                           Property.initRec
                           (fn (t, typeSize) =>
                            let
                               val s = Size.new ()
                               fun dependsOn (t: Type.t): unit =
                                  Size.<= (typeSize t, s)
                               val () =
                                  case Type.dest t of
                                     Array _ => Size.makeTop s
                                   | CPointer => ()
                                   | Datatype tc => Size.<= (tyconSize tc, s)
                                   | IntInf => if !Control.globalizeSmallIntInf
                                                  then ()
                                                  else Size.makeTop s
                                   | Real _ => ()
                                   | Ref t => dependsOn t
                                   | Thread => Size.makeTop s
                                   | Tuple ts => Vector.foreach (ts, dependsOn)
                                   | Vector _ => Size.makeTop s
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
                              val () =
                                 Vector.foreach
                                 (cons, fn {args, ...} =>
                                  Vector.foreach (args, dependsOn))
                           in
                              ()
                           end)
                    in
                       {isSmallType = not o Size.isTop o typeSize,
                        destroyIsSmallType = destroyTypeSize}
                    end
             | 9 => {isSmallType = fn _ => true,
                     destroyIsSmallType = fn () => ()}
             | _ => Error.bug "ConstantPropagation.mkIsSmallType"
         end

      val {isSmallType: Type.t -> bool,
           destroyIsSmallType: unit -> unit} =
         mkIsSmallType (!Control.globalizeSmallType)

      (* Walk through the program
       *  - removing declarations whose rhs is constant
       *  - replacing variables whose value is constant with globals
       *  - building up the global decs
       *)
      val {new = newGlobal, all = allGlobals} = Global.make ()
      fun maybeGlobal x = Value.global (value x, isSmallType, newGlobal)
      val maybeGlobal =
         Trace.trace
         ("ConstantPropagation.maybeGlobal",
          Var.layout,
          Option.layout (Var.layout o #1))
         maybeGlobal
      val isConst = Option.isSome o maybeGlobal

      val _ =
         Control.diagnostics
         (fn display =>
          let
             open Layout
             val numVars = ref 0
             val numConstVars = ref 0
             val _ =
                Program.foreachVar
                (program, fn (x, _) =>
                 (Int.inc numVars
                  ; if isConst x
                       then Int.inc numConstVars
                       else ()))
          in
             display (seq [str "\n\nConstant Variables: ",
                           Int.layout (!numConstVars),
                           str " / ",
                           Int.layout (!numVars)])
             ; (Program.foreachVar
                (program, fn (x, _) =>
                 if isConst x
                    then display (Var.layout x)
                    else ()))
          end)

      fun replaceVar x =
         case maybeGlobal x of
            NONE => x
          | SOME (g, _) => g
      fun doitStatement (Statement.T {var, ty, exp}) =
         let
            fun keep () =
               SOME (Statement.T {var = var,
                                  ty = ty,
                                  exp = Exp.replaceVar (exp, replaceVar)})
         in
            case var of
               NONE => keep ()
             | SOME var => 
                  (case (maybeGlobal var, exp) of
                      (NONE, _) => keep ()
                    | (SOME _, PrimApp {prim, ...}) =>
                         if Prim.maySideEffect prim
                            then keep ()
                         else NONE
                    | _ => NONE)
         end
      fun doitTransfer transfer =
         Transfer.replaceVar (transfer, replaceVar)
      fun doitBlock (Block.T {label, args, statements, transfer}) =
         Block.T {label = label,
                  args = args,
                  statements = Vector.keepAllMap (statements, doitStatement),
                  transfer = doitTransfer transfer}
      fun doitFunction f =
         let
            val {args, blocks, mayInline, name, raises, returns, start} =
               Function.dest f
         in
            Function.new {args = args,
                          blocks = Vector.map (blocks, doitBlock),
                          mayInline = mayInline,
                          name = name,
                          raises = raises,
                          returns = returns,
                          start = start}
         end
      val functions = List.revMap (functions, doitFunction)
      val globals = Vector.keepAllMap (globals, doitStatement)
      val newGlobals = allGlobals ()
      val _ =
         Control.diagnostics
         (fn display =>
          let open Layout
          in
             display (seq [str "\n\nNew Globals (",
                           Int.layout (Vector.length newGlobals),
                           str "):"])
             ; (Vector.foreach
                (newGlobals, display o Statement.layout))
          end)

      val globals = Vector.concat [newGlobals, globals]
      val shrink = shrinkFunction {globals = globals}
      val program = Program.T {datatypes = datatypes,
                               globals = globals,
                               functions = List.revMap (functions, shrink),
                               main = main}
      val _ = destroyIsSmallType ()
      val _ = Program.clearTop program
   in
      program
   end

end
