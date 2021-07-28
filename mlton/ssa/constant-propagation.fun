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

      structure Set = DisjointSet

      datatype t =
         T of {global: global ref,
               ty: Type.t,
               value: value} Set.t
      and value =
         Array of {birth: t ArrayBirth.t,
                   elt: t,
                   length: t}
       | Const of Const.t
       | Datatype of t Data.t
       | Ref of {arg: t,
                 birth: t RefBirth.t}
       | Tuple of t vector
       | Vector of {elt: t,
                    length: t}
       | Weak of t

      local
         fun make sel (T s) = sel (Set.! s)
      in
         val value = make #value
         val ty = make #ty
      end

      fun deConst' v =
         case value v of
            Const const => SOME const
          | _ => NONE
      fun deConst v =
         case deConst' v of
            NONE => NONE
          | SOME const => Const.getConst const

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
                          Array {birth, elt, length, ...} =>
                             seq [str "array ",
                                  tuple [ArrayBirth.layout layout birth,
                                         layout length,
                                         layout elt]]
                        | Const c => Const.layout c
                        | Datatype d => Data.layout layout d
                        | Ref {arg, birth, ...} =>
                             seq [str "ref ",
                                  tuple [layout arg,
                                         RefBirth.layout layout birth]]
                        | Tuple vs => Vector.layout layout vs
                        | Vector {elt, length, ...} =>
                             seq [str "vector ", tuple [layout elt,
                                                        layout length]]
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
      end

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
                            Array {birth, length, ...} =>
                               if !Control.globalizeArrays then
                               arrayOnce (birth, length)
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
                          | Vector {elt, length} =>
                               (case Option.map (deConst length, S.Const.deWord) of
                                   NONE => No
                                 | SOME length =>
                                      let
                                         val length = WordX.toInt length
                                         val eltTy = Type.deVector ty
                                         fun mkVec args =
                                            yes (Exp.PrimApp
                                                 {args = args,
                                                  prim = Prim.Vector_vector,
                                                  targs = Vector.new1 eltTy})
                                         fun mkConst (ws, elts) =
                                            yes (Exp.Const
                                                 (S.Const.wordVector
                                                  (WordXVector.fromList
                                                   ({elementSize = ws}, elts))))
                                      in
                                         case (Option.map (deConst elt, S.Const.deWordOpt),
                                               global elt) of
                                            (SOME (SOME w), _) =>
                                               mkConst (Type.deWord eltTy,
                                                        List.new (length, w))
                                          | (_, SOME (x, _)) =>
                                               mkVec (Vector.new (length, x))
                                          | _ =>
                                               if length = 0
                                                  then case Type.deWordOpt eltTy of
                                                          SOME ws => mkConst (ws, [])
                                                        | NONE => mkVec (Vector.new0 ())
                                                  else No
                                      end)
                          | Weak _ => No
                      val _ = r := g
                   in
                      global v
                   end
          end) arg

      fun new (v: value, ty: Type.t): t =
         T (Set.singleton {value = v,
                           ty = ty,
                           global = ref NotComputed})

      fun tuple vs =
         new (Tuple vs, Type.tuple (Vector.map (vs, ty)))

      fun const' (c, ty) = new (Const c, ty)

      fun const c = let val c' = Const.const c
                    in new (Const c', Type.ofConst c)
                    end

      fun constToVector (c, eltSz, err) =
         let
            val eltConst = Const.undefined ()
            val lengthConst = Const.undefined ()
            val () =
               Const.addHandler'
               (c, fn v =>
                case v of
                   Const.Value.Bottom => ()
                 | Const.Value.Point (S.Const.WordVector v) =>
                      (WordXVector.foreach
                       (v, fn w =>
                        Const.lowerBound (eltConst, Const.Value.Point (S.Const.word w)))
                       ; Const.lowerBound (lengthConst, Const.Value.Point (S.Const.word (WordX.fromInt (WordXVector.length v, WordSize.seqIndex ())))))
                 | Const.Value.Point _ => err ()
                 | Const.Value.Top =>
                      (Const.makeUnknown eltConst
                       ; Const.makeUnknown lengthConst))
            val elt = const' (eltConst, Type.word eltSz)
            val length = const' (lengthConst, Type.word (WordSize.seqIndex ()))
         in
            {elt = elt, length = length}
         end

      local
         fun make (err, sel) v =
            case value v of
               Vector fs => sel fs
             | Const c =>
                  sel (constToVector
                       (c, Type.deWord (Type.deVector (ty v)),
                        fn () => Error.bug err))
             | _ => Error.bug err
      in
         val vectorElt = make ("ConstantPropagation.Value.vectorElt", #elt)
         val vectorLength = make ("ConstantPropagation.Value.vectorLength", #length)
      end

      local
         fun make (err, sel) v =
            case value v of
               Array fs => sel fs
             | _ => Error.bug err
      in
         val arrayElt = make ("ConstantPropagation.Value.arrayElt", #elt)
         val arrayLength = make ("ConstantPropagation.Value.arrayLength", #length)
         val arrayBirth = make ("ConstantPropagation.Value.arrayBirth", #birth)
      end

      fun arrayFromArray (T s: t): t =
         let
            val {value, ty, ...} = Set.! s
         in case value of
            Array {elt, length, ...} =>
               new (Array {birth = ArrayBirth.unknown (), elt = elt, length = length}, ty)
          | _ => Error.bug "ConstantPropagation.Value.arrayFromArray"
         end

      fun vectorFromArray (T s: t): t =
         let
            val {value, ty, ...} = Set.! s
         in case value of
            Array {elt, length, ...} =>
               new (Vector {elt = elt, length = length}, Type.vector (Type.deArray ty))
          | _ => Error.bug "ConstantPropagation.Value.vectorFromArray"
         end

      local
         fun make (err, sel) v =
            case value v of
               Ref fs => sel fs
             | _ => Error.bug err
      in
         val deref = make ("ConstantPropagation.Value.deref", #arg)
         val refBirth = make ("ConstantPropagation.Value.refBirth", #birth)
      end

      fun deweak v =
         case value v of
            Weak v => v
          | _ => Error.bug "ConstantPropagation.Value.deweak"

      local
         (* The extra birth is because of let-style polymorphism.
          * arrayBirth is really the same as refBirth.
          *)
         fun make (const, data, refBirth, arrayBirth) =
            let
               fun loop (t: Type.t): t =
                  new
                  (case Type.dest t of
                      Type.Array t => 
                         Array {birth = arrayBirth (),
                                elt = loop t,
                                length = loop (Type.word (WordSize.seqIndex ()))}
                    | Type.Datatype _ => Datatype (data ())
                    | Type.Ref t => Ref {arg = loop t,
                                         birth = refBirth ()}
                    | Type.Tuple ts => Tuple (Vector.map (ts, loop))
                    | Type.Vector t =>
                         Vector {elt = loop t,
                                 length = loop (Type.word (WordSize.seqIndex ()))}
                    | Type.Weak t => Weak (loop t)
                    | _ => Const (const ()), 
                   t)
            in loop
            end
      in
         val fromType =
            make (Const.undefined,
                  Data.undefined,
                  ArrayBirth.undefined,
                  RefBirth.undefined)
         val unknown =
            make (Const.unknown,
                  Data.unknown,
                  ArrayBirth.unknown,
                  RefBirth.unknown)
      end

      fun select {tuple, offset, resultType = _} =
         case value tuple of
            Tuple vs => Vector.sub (vs, offset)
          | _ => Error.bug "ConstantPropagation.Value.select: non-tuple" 

      fun unit () = tuple (Vector.new0 ())
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
         val traceCoerce =
            Trace.trace ("ConstantPropagation.Value.coerce",
                         fn {from, to} => Layout.record [("from", layout from),
                                                         ("to", layout to)],
                         Unit.layout)
         fun coerces {froms: Value.t vector, tos: Value.t vector} =
            Vector.foreach2 (froms, tos, fn (from, to) =>
                             coerce {from = from, to = to})
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
                               Layout.toString (Value.layout from),
                               " to: ", Layout.toString (Value.layout to)])
                in
                  case (value from, value to) of
                     (Const from, Const to) =>
                        Const.coerce {from = from, to = to}
                   | (Datatype from, Datatype to) =>
                        Data.coerce {from = from, to = to}
                   | (Ref {birth, arg}, Ref {birth = b', arg = a'}) =>
                        (RefBirth.coerce {from = birth, to = b'}
                         ; unify (arg, a'))
                   | (Array {birth = b, length = n, elt = x},
                      Array {birth = b', length = n', elt = x'}) =>
                        (ArrayBirth.coerce {from = b, to = b'}
                         ; coerce {from = n, to = n'}
                         ; unify (x, x'))
                   | (Vector {length = n, elt = x},
                      Vector {length = n', elt = x'}) =>
                        (coerce {from = n, to = n'}
                         ; coerce {from = x, to = x'})
                   | (Tuple vs, Tuple vs') => coerces {froms = vs, tos = vs'}
                   | (Weak v, Weak v') => unify (v, v')
                   | (Const c, Vector {elt = elt', length = length'}) =>
                        let
                           val {elt, length} =
                              constToVector
                              (c, Type.deWord (Value.ty elt'),
                               error)
                        in
                           coerce {from = elt, to = elt'}
                           ; coerce {from = length, to = length'}
                        end
                   | (_, _) => error ()
                end) arg
         and unify (T s: t, T s': t): unit =
            if Set.equals (s, s')
               then ()
            else
               let 
                  val {value, ...} = Set.! s
                  val {value = value', ...} = Set.! s'
                  fun error () =
                     Error.bug
                     (concat ["ConstantPropagation.Value.unify: strange: value: ",
                              Layout.toString (Value.layout (T s)),
                              " value': ", Layout.toString (Value.layout (T s'))])
               in Set.union (s, s')
                  ; case (value, value') of
                       (Const c, Const c') => Const.unify (c, c')
                     | (Datatype d, Datatype d') => Data.unify (d, d')
                     | (Ref {birth, arg}, Ref {birth = b', arg = a'}) =>
                          (RefBirth.unify (birth, b')
                           ; unify (arg, a'))
                     | (Array {birth = b, length = n, elt = x},
                        Array {birth = b', length = n', elt = x'}) =>
                          (ArrayBirth.unify (b, b')
                           ; unify (n, n')
                           ; unify (x, x'))
                     | (Vector {length = n, elt = x},
                        Vector {length = n', elt = x'}) =>
                          (unify (n, n')
                           ; unify (x, x'))
                     | (Tuple vs, Tuple vs') => Vector.foreach2 (vs, vs', unify)
                     | (Weak v, Weak v') => unify (v, v')
                     | _ => error ()
               end
         fun conApp {con: Con.t, args: t vector}: t =
            let
               val {values = tos, result, ...} = conInfo con
            in
               coerces {froms = args, tos = tos}
               ; new (Datatype (Data.conApp {args = args, con = con}), result)
            end
         fun makeUnknown (v: t): unit =
            case value v of
               Array {length, elt, ...} => (makeUnknown length
                                            ; makeUnknown elt)
             | Const c => Const.makeUnknown c
             | Datatype d => Data.makeUnknown d
             | Ref {arg, ...} => makeUnknown arg
             | Tuple vs => Vector.foreach (vs, makeUnknown)
             | Vector {length, elt} => (makeUnknown length
                                        ; makeUnknown elt)
             | Weak v => makeUnknown v
         fun sideEffect (v: t): unit =
            case value v of
               Array {elt, ...} => makeUnknown elt
             | Const _ => ()
             | Datatype _ => ()
             | Ref {arg, ...} => makeUnknown arg
             | Vector _ => ()
             | Tuple vs => Vector.foreach (vs, sideEffect)
             | Weak v => makeUnknown v
         fun primApp {prim,
                      targs = _,
                      args: Value.t vector,
                      resultVar,
                      resultType}: t =
            let
               fun bear z =
                  case resultVar of
                     SOME resultVar => if once resultVar 
                                          then Birth.here z
                                       else Birth.unknown ()
                   | _ => Error.bug "ConstantPropagation.Value.primApp.bear"
               fun update (a, v) =
                  (coerce {from = v, to = arrayElt a}
                   ; unit ())
               fun arg i = Vector.sub (args, i)
               fun array (birth, length) =
                  let
                     val a = fromType resultType
                     val _ = ArrayBirth.coerce {from = birth, to = arrayBirth a}
                     val _ = coerce {from = length, to = arrayLength a}
                  in
                     (a, arrayElt a)
                  end
               fun vector length =
                  let
                     val v = fromType resultType
                     val _ = coerce {from = length, to = vectorLength v}
                  in
                     (v, vectorElt v)
                  end
               fun sequence mk =
                  let
                     val l =
                        (const o S.Const.word o WordX.fromInt)
                        (Vector.length args, WordSize.seqIndex ())
                     val (seq, elt) = mk l
                     val _ =
                        Vector.foreach
                        (args, fn arg =>
                         coerce {from = arg, to = elt})
                  in
                     seq
                  end
            in
               case prim of
                  Prim.Array_alloc {raw} =>
                     let
                        val birth = bear (ArrayInit.Alloc {raw = raw})
                     in
                        #1 (array (birth, arg 0))
                     end
                | Prim.Array_array =>
                     let
                        val birth = bear (ArrayInit.Array {args = args})
                     in
                        sequence (fn l => array (birth, l))
                     end
                | Prim.Array_copyArray =>
                     update (arg 0, arrayElt (arg 2))
                | Prim.Array_copyVector =>
                     update (arg 0, vectorElt (arg 2))
                | Prim.Array_length => arrayLength (arg 0)
                | Prim.Array_sub => arrayElt (arg 0)
                | Prim.Array_toArray => arrayFromArray (arg 0)
                | Prim.Array_toVector => vectorFromArray (arg 0)
                | Prim.Array_update => update (arg 0, arg 2)
                | Prim.Ref_assign =>
                     (coerce {from = arg 1, to = deref (arg 0)}; unit ())
                | Prim.Ref_deref => deref (arg 0)
                | Prim.Ref_ref =>
                     let
                        val v = arg 0
                        val r = fromType resultType
                        val _ = coerce {from = v, to = deref r}
                        val _ = RefBirth.coerce {from = bear {arg = v},
                                                 to = refBirth r}
                     in
                        r
                     end
                | Prim.Vector_length => vectorLength (arg 0)
                | Prim.Vector_sub => vectorElt (arg 0)
                | Prim.Vector_vector => sequence vector
                | Prim.Weak_get => deweak (arg 0)
                | Prim.Weak_new =>
                     let
                        val w = fromType resultType
                        val _ = coerce {from = arg 0, to = deweak w}
                     in
                        w
                     end
                | _ => (if Prim.maySideEffect prim
                           then Vector.foreach (args, sideEffect)
                        else ()
                        ; unknown resultType)
            end
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
      end
      fun filterIgnore _ = ()
      val {value, ...} =
         Control.trace (Control.Detail, "fixed point")
         analyze {
                  coerce = coerce,
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
