(* Copyright (C) 2017 Matthew Fluet.
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

structure Type =
   struct
      open Type

      fun isSmall t =
         case dest t of
            Array _ => false
          | Datatype _ => false
          | Ref t => isSmall t
          | Tuple ts => Vector.forall (ts, isSmall)
          | Vector _ => false
          | _ => true
   end

structure Sconst = Const
open Exp Transfer

structure Value =
   struct
      datatype global =
         NotComputed
       | No
       | Yes of Var.t

      structure Const =
         struct
            datatype t = T of {const: const ref,
                               coercedTo: t list ref}
            and const =
               Const of Const.t
              | Undefined (* no possible value *)
              | Unknown (* many possible values *)

            fun layout (T {const, ...}) = layoutConst (!const)
            and layoutConst c =
               let
                  open Layout
               in
                  case c of
                     Const c => Const.layout c
                   | Undefined => str "undefined constant"
                   | Unknown => str "unknown constant"
               end

            fun new c = T {const = ref c,
                           coercedTo = ref []}

            fun equals (T {const = r, ...}, T {const = r', ...}) = r = r'

            val equals =
               Trace.trace2 
               ("ConstantPropagation.Value.Const.equals", 
                layout, layout, Bool.layout) 
               equals

            val const = new o Const

            fun undefined () = new Undefined

            fun unknown () = new Unknown

            fun makeUnknown (T {const, coercedTo}): unit =
               case !const of
                  Unknown => ()
                | _ => (const := Unknown
                        ; List.foreach (!coercedTo, makeUnknown)
                        ; coercedTo := [])

            val makeUnknown =
               Trace.trace 
               ("ConstantPropagation.Value.Const.makeUnknown", 
                layout, Unit.layout) 
               makeUnknown

            fun send (c: t, c': const): unit =
               let
                  fun loop (c as T {const, coercedTo}) =
                     case (c', !const) of
                        (_, Unknown) => ()
                      | (_, Undefined) => (const := c'
                                           ; List.foreach (!coercedTo, loop))
                      | (Const c', Const c'') =>
                           if Const.equals (c', c'')
                              then ()
                           else makeUnknown c
                      | _ => makeUnknown c
               in
                  loop c
               end

            val send =
               Trace.trace2 
               ("ConstantPropagation.Value.Const.send", 
                layout, layoutConst, Unit.layout) 
               send

            fun coerce {from = from as T {const, coercedTo}, to: t}: unit =
               if equals (from, to)
                  then ()
               else
                  let
                     fun push () = List.push (coercedTo, to)
                  in
                     case !const of
                        c as Const _ => (push (); send (to, c))
                      | Undefined => push ()
                      | Unknown => makeUnknown to
                  end

            val coerce =
               Trace.trace
               ("ConstantPropagation.Value.Const.coerce",
                fn {from, to} => Layout.record [("from", layout from),
                                                ("to", layout to)],
                Unit.layout)
               coerce

            fun unify (c, c') =
               (coerce {from = c, to = c'}
                ; coerce {from = c', to = c})

            val unify =
               Trace.trace2 
               ("ConstantPropagation.Value.Const.unify", 
                layout, layout, Unit.layout) 
               unify
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

            fun layout (one: 'a t): Layout.t =
               Layout.record
               [("global", Option.layout Var.layout (! (global one)))]

            fun new (a: 'a): 'a t = T {extra = a,
                                       global = ref NONE}

            val equals: 'a t * 'a t -> bool = 
               fn (n, n') => global n = global n'
         end

      structure Place =
         struct
            datatype 'a t =
               One of 'a One.t
             | Undefined
             | Unknown

            val toString =
               fn One _ => "One"
                | Undefined => "Undefined"
                | Unknown => "Unknown"

            fun layout b = Layout.str (toString b)
         end

      structure Birth =
         struct
            datatype 'a t = T of {coercedTo: 'a t list ref,
                                  place: 'a Place.t ref}

            fun layout (T {place, ...}) = Place.layout (!place)

            fun equals (T {place = r, ...}, T {place = r', ...}) = r = r'

            fun new p = T {place = ref p,
                           coercedTo = ref []}

            fun undefined (): 'a t = new Place.Undefined
            fun unknown (): 'a t = new Place.Unknown
            fun here (a: 'a): 'a t = new (Place.One (One.new a))

            val traceMakeUnknown = 
               Trace.info 
               "ConstantPropagation.Value.Birth.makeUnknown"

            fun makeUnknown arg =
               Trace.traceInfo'
               (traceMakeUnknown, layout, Unit.layout)
               (fn T {place, coercedTo, ...} =>
                case !place of
                   Place.Unknown => ()
                 | _ => (place := Place.Unknown
                         ; List.foreach (!coercedTo, makeUnknown)
                         ; coercedTo := [])) arg

            val traceSend = 
               Trace.info 
               "ConstantPropagation.Value.Birth.send"

            fun send arg =
               Trace.traceInfo'
               (traceSend, Layout.tuple2 (layout, One.layout), Unit.layout)
               (fn (b, one) =>
                let
                   fun loop (b as T {place, coercedTo, ...}) =
                      case !place of
                         Place.Undefined => (place := Place.One one
                                             ; List.foreach (!coercedTo, loop))
                       | Place.One one' => if One.equals (one, one')
                                              then ()
                                           else makeUnknown b
                       | Place.Unknown => ()
                in
                   loop b
                end) arg

            val traceCoerce = 
               Trace.info 
               "ConstantPropagation.Value.Birth.coerce"
            fun coerce arg =
               Trace.traceInfo'
               (traceCoerce,
                fn {from, to} => Layout.record [("from", layout from),
                                                ("to", layout to)],
                Unit.layout)
               (fn {from = from as T {place, coercedTo, ...}, to} =>
                if equals (from, to)
                   then ()
                else
                   let
                      fun push () = List.push (coercedTo, to)
                   in
                      case !place of
                         Place.Unknown => makeUnknown to
                       | Place.One one => (push (); send (to, one))
                       | Place.Undefined => push ()
                   end) arg

            val traceUnify = 
               Trace.info 
               "ConstantPropagation.Value.Birth.unify"

            fun unify arg =
               Trace.traceInfo'
               (traceUnify, Layout.tuple2 (layout, layout), Unit.layout)
               (fn (c, c') =>
                (coerce {from = c, to = c'}
                 ; coerce {from = c', to = c})) arg
         end

      structure Set = DisjointSet
      structure Unique = UniqueId ()

      datatype t =
         T of {global: global ref,
               ty: Type.t,
               value: value} Set.t
      and value =
         Array of {birth: unit Birth.t,
                   elt: t,
                   length: t,
                   raw: bool option ref}
       | Const of Const.t
       | Datatype of data
       | Ref of {arg: t,
                 birth: {init: t} Birth.t}
       | Tuple of t vector
       | Vector of {elt: t,
                    length: t}
       | Weak of t
      and data =
         Data of {coercedTo: data list ref,
                  filters: {args: t vector,
                            con: Con.t} list ref,
                  value: dataVal ref}
      and dataVal =
         ConApp of {args: t vector,
                    con: Con.t,
                    uniq: Unique.t}
       | Undefined
       | Unknown

      local
         fun make sel (T s) = sel (Set.! s)
      in
         val value = make #value
         val ty = make #ty
      end
      fun deConst v =
         case value v of
            Const (Const.T {const, ...}) =>
               (case !const of
                   Const.Const c => SOME c
                 | _ => NONE)
          | _ => NONE

      local
         open Layout
      in
         fun layout v =
            case value v of
               Array {birth, elt, length, raw, ...} =>
                  seq [str "array", tuple [Birth.layout birth,
                                           layout length,
                                           layout elt,
                                           Option.layout Bool.layout (!raw)]]
             | Const c => Const.layout c
             | Datatype d => layoutData d
             | Ref {arg, birth, ...} =>
                  seq [str "ref ", tuple [layout arg, Birth.layout birth]]
             | Tuple vs => Vector.layout layout vs
             | Vector {elt, length, ...} => seq [str "vector ",
                                                 tuple [layout elt,
                                                        layout length]]
             | Weak v => seq [str "weak ", layout v]
         and layoutData (Data {value, ...}) =
            case !value of
               Undefined => str "undefined datatype"
             | ConApp {con, uniq, ...} =>
                  record [("con", Con.layout con),
                          ("uniq", Unique.layout uniq)]
                  (* Can't layout the args because there may be a circularity *)
             | Unknown => str "unknown datatype"
      end

      fun equals (T s, T s') = Set.equals (s, s')

      val equals =
         Trace.trace2 
         ("ConstantPropagation.Value.equals", 
          layout, layout, Bool.layout) 
         equals

      val globalsInfo = Trace.info "ConstantPropagation.Value.globals"
      val globalInfo = Trace.info "ConstantPropagation.Value.global"

      fun globals arg: (Var.t * Type.t) vector option =
         Trace.traceInfo
         (globalsInfo,
          (Vector.layout layout) o #1,
          Option.layout (Vector.layout
                         (Layout.tuple2 (Var.layout, Type.layout))),
          Trace.assertTrue)
         (fn (vs: t vector, newGlobal) =>
          Exn.withEscape
          (fn escape =>
           SOME (Vector.map
                 (vs, fn v =>
                  case global (v, newGlobal) of
                     NONE => escape NONE
                   | SOME g => g)))) arg
      and global arg: (Var.t * Type.t) option =
         Trace.traceInfo (globalInfo,
                          layout o #1,
                          Option.layout (Var.layout o #1),
                          Trace.assertTrue)
         (fn (v as T s, newGlobal) =>
          let val {global = r, ty, value} = Set.! s
          in case !r of
                No => NONE
              | Yes g => SOME (g, ty)
              | NotComputed =>
                   let
                      (* avoid globalizing circular abstract values *)
                      val _ = r := No
                      fun yes e = Yes (newGlobal (ty, e))
                      fun unary (Birth.T {place, ...},
                                 makeInit: 'a -> t,
                                 primApp: {targs: Type.t vector,
                                           args: Var.t vector} -> Exp.t,
                                 targ: Type.t) =
                         case !place of
                            Place.One (One.T {global = glob, extra, ...}) =>
                               let
                                  val init = makeInit extra
                               in
                                  case global (init, newGlobal) of
                                     SOME (x, _) =>
                                        Yes
                                        (case !glob of
                                            NONE => 
                                               let
                                                  val exp =
                                                     primApp
                                                     {targs = Vector.new1 targ,
                                                      args = Vector.new1 x}
                                                  val g = newGlobal (ty, exp)
                                               in
                                                  glob := SOME g; g
                                               end
                                              | SOME g => g)
                                   | _ => No
                               end
                          | _ => No
                      val g =
                         case value of
                            Array {birth, length, raw, ...} =>
                               unary (birth, fn _ => length,
                                      fn {args, targs} =>
                                      Exp.PrimApp {args = args,
                                                   prim = Prim.arrayAlloc
                                                          {raw = valOf (!raw)},
                                                   targs = targs},
                                      Type.deArray ty)
                          | Const (Const.T {const, ...}) =>
                               (case !const of
                                   Const.Const c => yes (Exp.Const c)
                                 | _ => No)
                          | Datatype (Data {value, ...}) =>
                               (case !value of
                                   ConApp {args, con, ...} =>
                                      (case globals (args, newGlobal) of
                                          NONE => No
                                        | SOME args =>
                                             yes (Exp.ConApp
                                                  {con = con,
                                                   args = Vector.map (args, #1)}))
                                 | _ => No)
                          | Ref {birth, ...} =>
                               unary (birth, fn {init} => init,
                                      fn {args, targs} =>
                                      Exp.PrimApp {args = args,
                                                   prim = Prim.reff,
                                                   targs = targs},
                                      Type.deRef ty)
                          | Tuple vs =>
                               (case globals (vs, newGlobal) of
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
                                                  prim = Prim.vector,
                                                  targs = Vector.new1 eltTy})
                                         fun mkConst (ws, elts) =
                                            yes (Exp.Const
                                                 (S.Const.wordVector
                                                  (WordXVector.fromList
                                                   ({elementSize = ws}, elts))))
                                      in
                                         case (Option.map (deConst elt, S.Const.deWordOpt),
                                               global (elt, newGlobal)) of
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
                      global (v, newGlobal)
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

      fun constToEltLength (c, err) =
         let
            val v =
               case c of
                  Sconst.WordVector v => v
                | _ => Error.bug err 
            val length = WordXVector.length v
            val eltTy = Type.word (WordXVector.elementSize v)
            val elt =
               if 0 = length
                  then const' (Const.unknown (), eltTy)
               else let
                       val w = WordXVector.sub (v, 0)
                    in
                       if WordXVector.forall (v, fn w' =>
                                              WordX.equals (w, w'))
                          then const (Sconst.word w)
                       else const' (Const.unknown (), eltTy)
                    end
            val length =
               const (Sconst.Word (WordX.fromIntInf (IntInf.fromInt length,
                                                     WordSize.seqIndex ())))
         in
            {elt = elt, length = length}
         end

      local
         fun make (err, sel) v =
            case value v of
               Vector fs => sel fs
             | Const (Const.T {const = ref (Const.Const c), ...}) =>
                  sel (constToEltLength (c, err))
             | _ => Error.bug err
      in
         val devector = make ("ConstantPropagation.Value.devector", #elt)
         val vectorLength = make ("ConstantPropagation.Value.vectorLength", #length)
      end

      local
         fun make (err, sel) v =
            case value v of
               Array fs => sel fs
             | _ => Error.bug err
      in val dearray = make ("ConstantPropagation.Value.dearray", #elt)
         val arrayLength = make ("ConstantPropagation.Value.arrayLength", #length)
         val arrayBirth = make ("ConstantPropagation.Value.arrayBirth", #birth)
         val arrayRaw = make ("ConstantPropagation.Value.arrayRaw", #raw)
      end

      fun arrayFromArray (T s: t): t =
         let
            val {value, ty, ...} = Set.! s
         in case value of
            Array {elt, length, ...} =>
               new (Array {birth = Birth.unknown (), elt = elt, length = length, raw = ref (SOME false)}, ty)
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

      structure Data =
         struct
            datatype t = datatype data

            val layout = layoutData

            local
               fun make v () = Data {value = ref v,
                                     coercedTo = ref [],
                                     filters = ref []}
            in
               val undefined = make Undefined
               val unknown = make Unknown
            end
         end

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
                                length = loop (Type.word (WordSize.seqIndex ())),
                                raw = ref NONE}
                    | Type.Datatype _ => Datatype (data ())
                    | Type.Ref t => Ref {arg = loop t,
                                         birth = refBirth ()}
                    | Type.Tuple ts => Tuple (Vector.map (ts, loop))
                    | Type.Vector t => Vector 
                         {elt = loop t,
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
                  Birth.undefined,
                  Birth.undefined)
         val unknown =
            make (Const.unknown,
                  Data.unknown,
                  Birth.unknown,
                  Birth.unknown)
      end

      fun select {tuple, offset, resultType = _} =
         case value tuple of
            Tuple vs => Vector.sub (vs, offset)
          | _ => Error.bug "ConstantPropagation.Value.select: non-tuple" 

      fun unit () = tuple (Vector.new0 ())
   end

val traceSendConApp =
   Trace.trace2
   ("ConstantPropagation.sendConApp", Value.Data.layout,
    fn {con, args, uniq} =>
    Layout.record [("con", Con.layout con),
                   ("args", Vector.layout Value.layout args),
                   ("uniq", Value.Unique.layout uniq)],
    Unit.layout)

val traceSendConAppLoop =
   Trace.trace 
   ("ConstantPropagation.sendConAppLoop", 
    Value.Data.layout, Unit.layout)

val traceMakeDataUnknown =
   Trace.trace 
   ("ConstantPropagation.makeDataUnknown", 
    Value.Data.layout, Unit.layout)

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
         fun makeDataUnknown arg: unit =
            traceMakeDataUnknown
            (fn Data {value, coercedTo, filters, ...} =>
             let
                fun doit () =
                   (value := Unknown
                    ; List.foreach (!coercedTo, makeDataUnknown)
                    ; coercedTo := []
                    ; (List.foreach
                       (!filters, fn {con, args} =>
                        coerces {froms = conValues con,
                                 tos = args})))
             in
                case !value of
                   ConApp _ => doit ()
                 | Undefined => doit ()
                 | Unknown => ()
             end) arg
         and sendConApp arg: unit =
            traceSendConApp
            (fn (d: data, ca as {con, args, uniq}) =>
             let
                val v = ConApp ca
                fun loop arg: unit =
                   traceSendConAppLoop
                   (fn Data {value, coercedTo, filters, ...} =>
                    case !value of
                       Unknown => ()
                     | Undefined =>
                          (value := v
                           ; List.foreach (!coercedTo, loop)
                           ; (List.foreach
                              (!filters, fn {con = con', args = args'} =>
                               if Con.equals (con, con')
                                  then coerces {froms = args, tos = args'}
                               else ())))
                     | ConApp {con = con', uniq = uniq', ...} =>
                          if Unique.equals (uniq, uniq')
                             orelse (Con.equals (con, con')
                                     andalso Vector.isEmpty args)
                             then ()
                          else makeDataUnknown d) arg
             in loop d
             end) arg
         and coerces {froms: Value.t vector, tos: Value.t vector} =
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
                        coerceData {from = from, to = to}
                   | (Ref {birth, arg}, Ref {birth = b', arg = a'}) =>
                        (Birth.coerce {from = birth, to = b'}
                         ; unify (arg, a'))
                   | (Array {birth = b, length = n, elt = x, raw = r},
                      Array {birth = b', length = n', elt = x', raw = r'}) =>
                        (Birth.coerce {from = b, to = b'}
                         ; coerce {from = n, to = n'}
                         ; unify (x, x')
                         ; (case (!r, !r') of
                               (NONE, r') => r := r'
                             | (r, NONE) => r' := r
                             | (SOME b, SOME b') =>
                                  (if b = b'
                                      then ()
                                      else error ())))
                   | (Vector {length = n, elt = x},
                      Vector {length = n', elt = x'}) =>
                        (coerce {from = n, to = n'}
                         ; coerce {from = x, to = x'})
                   | (Tuple vs, Tuple vs') => coerces {froms = vs, tos = vs'}
                   | (Weak v, Weak v') => unify (v, v')
                   | (Const (Const.T {const = ref (Const.Const c), ...}),
                      Vector {elt, length}) =>
                        let
                           val {elt = elt', length = length'} =
                              Value.constToEltLength (c, "coerce")
                        in
                           coerce {from = elt', to = elt}
                           ; coerce {from = length', to = length}
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
                     | (Datatype d, Datatype d') => unifyData (d, d')
                     | (Ref {birth, arg}, Ref {birth = b', arg = a'}) =>
                          (Birth.unify (birth, b')
                           ; unify (arg, a'))
                     | (Array {birth = b, length = n, elt = x, raw = r},
                        Array {birth = b', length = n', elt = x', raw = r'}) =>
                          (Birth.unify (b, b')
                           ; unify (n, n')
                           ; unify (x, x')
                           ; (case (!r, !r') of
                                 (NONE, r') => r := r'
                               | (r, NONE) => r' := r
                               | (SOME b, SOME b') =>
                                    (if b = b'
                                        then ()
                                        else error ())))
                     | (Vector {length = n, elt = x},
                        Vector {length = n', elt = x'}) =>
                          (unify (n, n')
                           ; unify (x, x'))
                     | (Tuple vs, Tuple vs') => Vector.foreach2 (vs, vs', unify)
                     | (Weak v, Weak v') => unify (v, v')
                     | _ => error ()
               end
         and unifyData (d, d') =
            (coerceData {from = d, to = d'}
             ; coerceData {from = d', to = d})
         and coerceData {from = Data {value, coercedTo, ...}, to} =
            case !value of
               ConApp ca => (List.push (coercedTo, to)
                             ; sendConApp (to, ca))
             | Undefined => List.push (coercedTo, to)
             | Unknown => makeDataUnknown to
         fun conApp {con: Con.t, args: t vector}: t =
            let
               val {values = tos, result, ...} = conInfo con
            in
               coerces {froms = args, tos = tos}
               ; new (Datatype
                      (Data {value = ref (ConApp {con = con, args = args,
                                                  uniq = Unique.new ()}),
                             coercedTo = ref [],
                             filters = ref []}),
                      result)
            end
         fun makeUnknown (v: t): unit =
            case value v of
               Array {length, elt, ...} => (makeUnknown length
                                            ; makeUnknown elt)
             | Const c => Const.makeUnknown c
             | Datatype d => makeDataUnknown d
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
             | Vector {elt, ...} => makeUnknown elt
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
                                          andalso 
                                          Type.isSmall resultType
                                          then Birth.here z
                                       else Birth.unknown ()
                   | _ => Error.bug "ConstantPropagation.Value.primApp.bear"
               fun update (a, v) =
                  (coerce {from = v, to = dearray a}
                   ; unit ())
               fun arg i = Vector.sub (args, i)
               datatype z = datatype Prim.Name.t
               fun array (raw, length, birth) =
                  let
                     val a = fromType resultType
                     val _ = coerce {from = length, to = arrayLength a}
                     val _ = Birth.coerce {from = birth, to = arrayBirth a}
                     val _ = arrayRaw a := SOME raw
                  in
                     a
                  end
               fun vector () =
                  let
                     val v = fromType resultType
                     val l =
                        (const o S.Const.word o WordX.fromIntInf)
                        (IntInf.fromInt (Vector.length args),
                         WordSize.seqIndex ())
                     val _ = coerce {from = l, to = vectorLength v}
                     val _ =
                        Vector.foreach
                        (args, fn arg =>
                         coerce {from = arg, to = devector v})
                  in
                     v
                  end
            in
               case Prim.name prim of
                  Array_alloc {raw} => array (raw, arg 0, bear ())
                | Array_copyArray =>
                     update (arg 0, dearray (arg 2))
                | Array_copyVector =>
                     update (arg 0, devector (arg 2))
                | Array_length => arrayLength (arg 0)
                | Array_sub => dearray (arg 0)
                | Array_toArray => arrayFromArray (arg 0)
                | Array_toVector => vectorFromArray (arg 0)
                | Array_update => update (arg 0, arg 2)
                | Ref_assign =>
                     (coerce {from = arg 1, to = deref (arg 0)}; unit ())
                | Ref_deref => deref (arg 0)
                | Ref_ref =>
                     let
                        val v = arg 0
                        val r = fromType resultType
                        val _ = coerce {from = v, to = deref r}
                        val _ = Birth.coerce {from = bear {init = v},
                                              to = refBirth r}
                     in
                        r
                     end
                | Vector_length => vectorLength (arg 0)
                | Vector_sub => devector (arg 0)
                | Vector_vector => vector ()
                | Weak_get => deweak (arg 0)
                | Weak_new =>
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
               Datatype (Data {value, filters, ...}) =>
                  let
                     fun save () = List.push (filters, {con = con, args = args})
                  in case !value of
                     Undefined => save ()
                   | Unknown => coerces {froms = conValues con, tos = args}
                   | ConApp {con = con', args = args', ...} =>
                        ((* The save () has to happen before the coerces because
                          * they may loop back and change the variant, which
                          * would need to then change this value.
                          *)
                         save ()
                         ; if Con.equals (con, con')
                              then coerces {froms = args', tos = args}
                           else ())
                  end
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
             ; display (str "\n\nConstants:")
             ; (Program.foreachVar
                (program, fn (x, _) => display (seq [Var.layout x,
                                                     str " ",
                                                     Value.layout (value x)])))
          end)
      (* Walk through the program
       *  - removing declarations whose rhs is constant
       *  - replacing variables whose value is constant with globals
       *  - building up the global decs
       *)
      val {new = newGlobal, all = allGlobals} = Global.make ()
      fun replaceVar x =
         case Value.global (value x, newGlobal) of
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
                  (case (Value.global (value var, newGlobal), exp) of
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
      val globals = Vector.concat [allGlobals (), globals]
      val shrink = shrinkFunction {globals = globals}
      val program = Program.T {datatypes = datatypes,
                               globals = globals,
                               functions = List.revMap (functions, shrink),
                               main = main}
      val _ = Program.clearTop program
   in
      program
   end

end
