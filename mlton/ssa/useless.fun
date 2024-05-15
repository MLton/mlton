(* Copyright (C) 2009,2017-2021,2024 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Useless (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S
(* useless thing elimination
 *  build some kind of dependence graph where 
 *    - a value of ground type is useful if it is an arg to a primitive
 *    - a tuple is useful if it contains a useful component
 *    - a conapp is useful if it contains a useful component
 *                            or is used in a case
 *
 * If a useful tuple is coerced to another useful tuple,
 *   then all of their components must agree (exactly).
 * It is trivial to convert a useful value to a useless one.
 *
 * It is also trivial to convert a useful tuple to one of its
 *  useful components -- but this seems hard
 *)

(* Suppose that you have a ref/array/vector that is useful, but the
 * components aren't -- then the components are converted to type unit, and
 * any primapp args must be as well.
 *)

structure Value =
   struct
      structure Set = DisjointSet

      structure Exists :
         sig
            type t
            val <= : t * t -> unit
            val == : t * t -> unit
            val doesExist: t -> bool
            val layout: t -> Layout.t
            val mustExist: t -> unit
            val new: unit -> t
            val whenExists: t * (unit -> unit) -> unit
         end =
         struct
            structure L = TwoPointLattice (val bottom = "not exists"
                                           val top = "exists")
            open L
            val mustExist = makeTop
            val doesExist = isTop
            val whenExists = addHandler
         end

      structure Useful :
         sig
            type t
            val <= : t * t -> unit
            val == : t * t -> unit
            val isUseful: t -> bool
            val layout: t -> Layout.t
            val makeUseful: t -> unit
            val makeWanted: t -> unit
            val new: unit -> t
            val whenUseful: t * (unit -> unit) -> unit
         end =
         struct
            structure U = TwoPointLattice (val bottom = "useless"
                                           val top = "useful")
            structure W = TwoPointLattice (val bottom = "unwanted"
                                           val top = "wanted")

            datatype t = T of U.t * W.t

            fun layout (T (u, w)) =
               let open Layout
               in seq [U.layout u, str "/", W.layout w]
               end

            fun new () =
               T (U.new (), W.new ())
            fun == (T (u, w), T (u', w')) =
               (U.== (u, u'); W.== (w, w'))
            fun (T (uf, wf)) <= (T (ut, wt)) =
               (U.<= (uf, ut);
                W.<= (wf, wt);
                W.addHandler (wf, fn () => U.== (uf, ut)))

            fun makeUseful (T (u, _)) = U.makeTop u
            fun isUseful (T (u, _)) = U.isTop u
            fun whenUseful (T (u, _), h) = U.addHandler (u, h)

            fun makeWanted (T (_, w)) = W.makeTop w
         end

      datatype t =
         T of {new: (Type.t * bool) option ref,
               ty: Type.t,
               value: value} Set.t
      and value =
         Array of {elt: slot,
                   length: t,
                   useful: Useful.t}
       | Ground of Useful.t
       | Ref of {arg: slot,
                 useful: Useful.t}
       | Tuple of slot vector
       | Vector of {elt: slot,
                    length: t}
       | Weak of {arg: slot,
                  useful: Useful.t}
      withtype slot = t * Exists.t

      local
         fun make sel (T s) = sel (Set.! s)
      in
         val value = make #value
         val ty = make #ty
      end

      local
         open Layout
      in
         fun layout (T s) =
            let
               val {value, ...} = Set.! s
            in
               layoutValue value
            end
         and layoutValue value =
            case value of
               Array {elt, length, ...} =>
                  seq [str "array", tuple [layout length, layoutSlot elt]]
             | Ground g => seq [str "ground ", Useful.layout g]
             | Ref {arg, useful, ...} =>
                  seq [str "ref ",
                       record [("useful", Useful.layout useful),
                               ("slot", layoutSlot arg)]]
             | Tuple vs => Vector.layout layoutSlot vs
             | Vector {elt, length} =>
                  seq [str "vector", tuple [layout length, layoutSlot elt]]
             | Weak {arg, useful} =>
                  seq [str "weak ",
                       record [("useful", Useful.layout useful),
                               ("slot", layoutSlot arg)]]
         and layoutSlot (v, e) =
            tuple [Exists.layout e, layout v]
      end

      fun unify (T s, T s') =
         if Set.equals (s, s')
            then ()
         else
            let
               val {value = v, ...} = Set.! s
               val {value = v', ...} = Set.! s'
               val _ = Set.union (s, s')
            in
               case (v, v') of
                  (Array {useful = u, length = n, elt = e},
                   Array {useful = u', length = n', elt = e'}) =>
                     (Useful.== (u, u'); unify (n, n'); unifySlot (e, e'))
                | (Ground g, Ground g') => Useful.== (g, g')
                | (Ref {useful = u, arg = a},
                   Ref {useful = u', arg = a'}) =>
                     (Useful.== (u, u'); unifySlot (a, a'))
                | (Tuple vs, Tuple vs') =>
                     Vector.foreach2 (vs, vs', unifySlot)
                | (Vector {length = n, elt = e},
                   Vector {length = n', elt = e'}) =>
                     (unify (n, n'); unifySlot (e, e'))
                | (Weak {useful = u, arg = a}, Weak {useful = u', arg = a'}) =>
                     (Useful.== (u, u'); unifySlot (a, a'))
                | _ => Error.bug "Useless.Value.unify: strange"
            end
      and unifySlot ((v, e), (v', e')) = (unify (v, v'); Exists.== (e, e'))

      val unify =
         Trace.trace ("Useless.Value.unify",
                      Layout.tuple2 (layout, layout),
                      Unit.layout)
         unify

      fun coerce {from = from as T sfrom, to = to as T sto}: unit =
         if Set.equals (sfrom, sto)
            then ()
         else
            let
               fun coerceSlot ((v, e), (v', e')) =
                  (coerce {from = v, to = v'}
                   ; Exists.== (e, e'))
            in
               case (value from, value to) of
                  (Array _, Array _) => unify (from, to)
                | (Ground from, Ground to) => Useful.<= (to, from)
                | (Ref _, Ref _) => unify (from, to)
                | (Tuple vs, Tuple vs') =>
                     Vector.foreach2 (vs, vs', coerceSlot)
                | (Vector {length = n, elt = e},
                   Vector {length = n', elt = e'}) =>
                     (coerce {from = n, to = n'}
                      ; coerceSlot (e, e'))
                | (Weak _, Weak _) => unify (from, to)
                | _ => Error.bug "Useless.Value.coerce: strange"
            end

      val coerce =
         Trace.trace ("Useless.Value.coerce",
                      fn {from, to} => let open Layout
                                       in record [("from", layout from),
                                                  ("to", layout to)]
                                       end,
                      Unit.layout) 
         coerce

      fun coerces {from, to} =
         Vector.foreach2 (from, to, fn (from, to) =>
                          coerce {from = from, to = to})

      fun foreach (v: t, f: Useful.t -> unit): unit  =
         let
            fun loop (v: t): unit =
               case value v of
                  Array {length, elt, useful} =>
                     (f useful; loop length; slot elt)
                | Ground u => f u
                | Tuple vs => Vector.foreach (vs, slot)
                | Ref {arg, useful} => (f useful; slot arg)
                | Vector {length, elt} => (loop length; slot elt)
                | Weak {arg, useful} => (f useful; slot arg)
            and slot (v, _) = loop v
         in
            loop v
         end

      (* Coerce every ground value in v to u. *)
      fun deepCoerce (v: t, u: Useful.t): unit =
         foreach (v, fn u' => Useful.<= (u', u))

      val deepCoerce =
         Trace.trace2 ("Useless.deepCoerce", layout, Useful.layout, Unit.layout)
         deepCoerce

      fun deground (v: t): Useful.t =
         case value v of
            Ground g => g
          | _ => Error.bug "Useless.deground"

      fun someUseful (v: t): Useful.t option =
         case value v of
            Array {useful = u, ...} => SOME u
          | Ground u => SOME u
          | Ref {useful = u, ...} => SOME u
          | Tuple slots => Vector.peekMap (slots, someUseful o #1)
          | Vector {length, ...} => SOME (deground length)
          | Weak {useful = u, ...} => SOME u

      fun allOrNothing (v: t): Useful.t option =
         case someUseful v of
            NONE => NONE
          | SOME u => (foreach (v, fn u' => Useful.== (u, u'))
                       ; SOME u)

      fun fromType (t: Type.t): t =
         let
            fun loop (t: Type.t, es: Exists.t list): t =
               let
                  fun useful () =
                     let val u = Useful.new ()
                     in Useful.whenUseful
                        (u, fn () => List.foreach (es, Exists.mustExist))
                        ; u
                     end
                  fun slot t =
                     let val e = Exists.new ()
                     in (loop (t, e :: es), e)
                     end
                  val loop = fn t => loop (t, es)
                  val value =
                     case Type.dest t of
                        Type.Array t =>
                           let val elt as (_, e) = slot t
                               val length = loop (Type.word (WordSize.seqIndex ()))
                           in Exists.whenExists
                              (e, fn () => Useful.makeUseful (deground length))
                              ; Array {useful = useful (),
                                       length = length,
                                       elt = elt}
                           end
                      | Type.Ref t => Ref {arg = slot t,
                                           useful = useful ()}
                      | Type.Tuple ts => Tuple (Vector.map (ts, slot))
                      | Type.Vector t => 
                           Vector {length = loop (Type.word (WordSize.seqIndex ())),
                                   elt = slot t}
                      | Type.Weak t => Weak {arg = slot t,
                                             useful = useful ()}
                      | _ => Ground (useful ())
               in
                  T (Set.singleton {ty = t,
                                    new = ref NONE,
                                    value = value})
               end
         in
            loop (t, [])
         end

      fun const (c: Const.t): t =
         let
            val v = fromType (Type.ofConst c)
            (* allOrNothing v because constants are not transformed and their
             * type cannot change.  So they must either be completely eliminated
             * or completely kept.
             *)
            val _ = allOrNothing v
         in
            v
         end

      fun detupleSlots (v: t): slot vector =
         case value v of
            Tuple ss => ss
          | _ => Error.bug "Useless.detupleSlots"
      fun detuple v = Vector.map (detupleSlots v, #1)
      fun tuple (vs: t vector): t =
         let
            val t = Type.tuple (Vector.map (vs, ty))
            val v = fromType t
            val _ =
               Vector.foreach2 (vs, detuple v, fn (v, v') =>
                                coerce {from = v, to = v'})
         in
            v
         end
      fun select {tuple, offset, resultType} =
         let
            val v = fromType resultType
            val _ = coerce {from = Vector.sub (detuple tuple, offset), to = v}
         in
            v
         end
      local
         fun make (err, sel) v =
            case value v of
               Vector fs => sel fs
             | _ => Error.bug err
      in
         val devector = make ("Useless.devector", #1 o #elt)
         val vectorLength = make ("Useless.vectorLength", #length)
      end
      local
         fun make (err, sel) v =
            case value v of
               Array fs => sel fs
             | _ => Error.bug err
      in
         val dearray: t -> t = make ("Useless.dearray", #1 o #elt)
         val arrayLength = make ("Useless.arrayLength", #length)
         val arrayUseful = make ("Useless.arrayUseful", #useful)
      end
      local
         fun make (err, sel) v =
            case value v of
               Ref fs => sel fs
             | _ => Error.bug err
      in
         val deref: t -> t = make ("Useless.deref", #1 o #arg)
      end
      local
         fun make (err, sel) v =
            case value v of
               Weak fs => sel fs
             | _ => Error.bug err
      in
         val deweak: t -> t = make ("Useless.deweak", #1 o #arg)
         val weakUseful = make ("Useless.weakUseful", #useful)
      end

      fun newType (v: t): Type.t = #1 (getNew v)
      and isUseful (v: t): bool = #2 (getNew v)
      and getNew (T s): Type.t * bool =
         let
            val {value, ty, new, ...} = Set.! s
         in
            Ref.memoize
            (new, fn () =>
             let 
                fun slot (arg: t, e: Exists.t) =
                   let val (t, b) = getNew arg
                   in (if Exists.doesExist e then t else Type.unit, b)
                   end
                fun wrap ((t, b), f) = (f t, b)
                fun or ((t, b), b') = (t, b orelse b')
                fun maybe (u: Useful.t, s: slot, make: Type.t -> Type.t) =
                   wrap (or (slot s, Useful.isUseful u), make)
             in
                case value of
                   Array {useful, elt, length, ...} =>
                      or (wrap (slot elt, Type.array),
                          Useful.isUseful useful orelse isUseful length)
                 | Ground u => (ty, Useful.isUseful u)
                 | Ref {arg, useful, ...} =>
                      maybe (useful, arg, Type.reff)
                 | Tuple vs =>
                      let
                         val (v, b) =
                            Vector.mapAndFold
                            (vs, false, fn ((v, e), useful) =>
                             let
                                val (t, u) = getNew v
                                val t =
                                   if Exists.doesExist e
                                      then SOME t
                                   else NONE
                             in (t, u orelse useful)
                             end)
                         val v = Vector.keepAllMap (v, fn t => t)
                      in
                         (Type.tuple v, b)
                      end
                 | Vector {elt, length, ...} =>
                      or (wrap (slot elt, Type.vector), isUseful length)
                 | Weak {arg, useful} =>
                      maybe (useful, arg, Type.weak)
             end)
         end

      val getNew =
         Trace.trace ("Useless.getNew", layout, Layout.tuple2 (Type.layout, Bool.layout))
         getNew

      val isUseful = Trace.trace ("Useless.isUseful", layout, Bool.layout) isUseful

      val newType = Trace.trace ("Useless.newType", layout, Type.layout) newType

      fun newTypes (vs: t vector): Type.t vector =
         Vector.keepAllMap (vs, fn v =>
                            let val (t, b) = getNew v
                            in if b then SOME t else NONE
                            end)
   end

structure Exists = Value.Exists

fun transform (program: Program.t): Program.t =
   let
      val program as Program.T {datatypes, globals, functions, main} =
         eliminateDeadBlocks program
      val {get = conInfo: Con.t -> {args: Value.t vector,
                                    value: unit -> Value.t},
           set = setConInfo, ...} =
         Property.getSetOnce 
         (Con.plist, Property.initRaise ("Useless.conInfo", Con.layout))
      val {get = tyconInfo: Tycon.t -> {cons: Con.t vector,
                                        visitedDeepMakeUseful: bool ref,
                                        visitedShallowMakeUseful: bool ref},
           set = setTyconInfo, ...} =
         Property.getSetOnce 
         (Tycon.plist, Property.initRaise ("Useless.tyconInfo", Tycon.layout))
      local open Value
      in
         val _ =
            Vector.foreach
            (datatypes, fn Datatype.T {tycon, cons} =>
             let
                val _ =
                   setTyconInfo (tycon, {cons = Vector.map (cons, #con),
                                         visitedDeepMakeUseful = ref false,
                                         visitedShallowMakeUseful = ref false})
                fun value () = fromType (Type.datatypee tycon)
             in Vector.foreach
                (cons, fn {con, args} =>
                 setConInfo (con, {value = value,
                                   args = Vector.map (args, fromType)}))
             end)
         val conArgs = #args o conInfo
         fun conApp {con: Con.t,
                     args: Value.t vector} =
            let val {args = args', value, ...} = conInfo con
            in coerces {from = args, to = args'}
               ; value ()
            end
         fun filter (v: Value.t, con: Con.t, to: Value.t vector): unit =
            case value v of
               Ground g =>
                  (Useful.makeUseful g
                   ; coerces {from = conArgs con, to = to})
             | _ => Error.bug "Useless.filter: non ground"
         val filter =
            Trace.trace3 ("Useless.filter",
                          Value.layout,
                          Con.layout,
                          Vector.layout Value.layout,
                          Unit.layout)
            filter
         fun filterGround (v: Value.t): unit =
            case value v of
               Ground g => Useful.makeUseful g
             | _ => Error.bug "Useless.filterGround: non ground"

         (* This is for primitive args, which may inspect any component.
          *)
         fun mkDeepMakeUseful deepMakeUseful v =
            let
               val slot = deepMakeUseful o #1
            in
               case value v of
                  Array {useful = u, length = n, elt = e} =>
                     (Useful.makeUseful u
                      ; deepMakeUseful n
                      ; slot e)
                | Ground u =>
                     (Useful.makeUseful u
                      (* Make all constructor args of this tycon useful *)
                      ; (case Type.dest (ty v) of
                            Type.Datatype tycon =>
                               let
                                  val {cons, visitedDeepMakeUseful, visitedShallowMakeUseful} =
                                     tyconInfo tycon
                               in
                                  if !visitedDeepMakeUseful
                                     then ()
                                     else (visitedDeepMakeUseful := true
                                           ; visitedShallowMakeUseful := true
                                           ; Vector.foreach
                                             (cons, fn con =>
                                              Vector.foreach
                                              (#args (conInfo con),
                                               deepMakeUseful)))
                               end
                          | _ => ()))
                | Ref {useful = u, arg = a} => (Useful.makeUseful u; slot a)
                | Tuple vs => Vector.foreach (vs, slot)
                | Vector {length = n, elt = e} => (deepMakeUseful n; slot e)
                | Weak {useful = u, arg = a} => (Useful.makeUseful u; slot a)
            end
         val deepMakeUseful =
            Trace.traceRec
            ("Useless.deepMakeUseful",
             Value.layout,
             Unit.layout)
            mkDeepMakeUseful

         (* This is used for MLton_equal and MLton_hash, which will not inspect
          * contents of Array, Ref, or Weak.
          *)
         fun mkShallowMakeUseful shallowMakeUseful v =
            let
               val slot = shallowMakeUseful o #1
            in
               case value v of
                  Array {useful = u, ...} => Useful.makeUseful u
                | Ground u =>
                     (Useful.makeUseful u
                      (* Make all constructor args of this tycon useful *)
                      ; (case Type.dest (ty v) of
                            Type.Datatype tycon =>
                               let
                                  val {cons, visitedShallowMakeUseful, ...} =
                                     tyconInfo tycon
                               in
                                  if !visitedShallowMakeUseful
                                     then ()
                                     else (visitedShallowMakeUseful := true
                                           ; Vector.foreach
                                             (cons, fn con =>
                                              Vector.foreach
                                              (#args (conInfo con),
                                               shallowMakeUseful)))
                               end
                          | _ => ()))
                | Ref {useful = u, ...} => Useful.makeUseful u
                | Tuple vs => Vector.foreach (vs, slot)
                | Vector {length = n, elt = e} => (shallowMakeUseful n; slot e)
                | Weak {useful = u, ...} => Useful.makeUseful u
            end
         val shallowMakeUseful =
            Trace.traceRec
            ("Useless.shallowMakeUseful",
             Value.layout,
             Unit.layout)
            mkShallowMakeUseful

         (* This is used for MLton_eq, MLton_share, and MLton_size,
          * which should only make use of the components that
          * are used elsewhere in the computation.
          *)
         fun mkMakeWanted makeWanted v =
            let
               val slot = makeWanted o #1
            in
               case value v of
                  Array {useful = u, ...} => Useful.makeWanted u
                | Ground u => Useful.makeWanted u
                | Ref {useful = u, ...} => Useful.makeWanted u
                | Tuple vs => Vector.foreach (vs, slot)
                | Vector {length = n, elt = e} => (makeWanted n; slot e)
                | Weak {useful = u, ...} => Useful.makeWanted u
            end
         val makeWanted =
            Trace.traceRec
            ("Useless.makeWanted",
             Value.layout,
             Unit.layout)
            mkMakeWanted

         fun primApp {args: t vector, prim, resultVar = _, resultType,
                      targs = _} =
            let
               val result = fromType resultType
               fun return v = coerce {from = v, to = result}
               infix dependsOn
               fun v1 dependsOn v2 = deepCoerce (v2, deground v1)
               fun arg i = Vector.sub (args, i)
               fun sub () =
                  (arg 1 dependsOn result
                   ; return (dearray (arg 0)))
               fun update () =
                  let
                     val a = dearray (arg 0)
                  in arg 1 dependsOn a
                     ; coerce {from = arg 2, to = a}
                  end
               val _ =
                  case prim of
                     Prim.Array_alloc _ =>
                        coerce {from = arg 0, to = arrayLength result}
                   | Prim.Array_array =>
                        let
                           val l =
                              (const o S.Const.word o WordX.fromInt)
                              (Vector.length args,
                               WordSize.seqIndex ())
                        in
                           (coerce {from = l, to = arrayLength result}
                            ; Vector.foreach
                              (args, fn arg =>
                               coerce {from = arg, to = dearray result}))
                        end
                   | Prim.Array_copyArray =>
                        let
                           val a = dearray (arg 0)
                        in
                           arg 1 dependsOn a
                           ; arg 3 dependsOn a
                           ; arg 4 dependsOn a
                           ; case (value (arg 0), value (arg 2)) of
                                (Array {elt = e, ...}, Array {elt = e', ...}) =>
                                   unifySlot (e, e')
                              | _ => Error.bug "Useless.primApp: Array_copyArray"
                         end
                   | Prim.Array_copyVector =>
                        let
                           val a = dearray (arg 0)
                        in
                           arg 1 dependsOn a
                           ; arg 3 dependsOn a
                           ; arg 4 dependsOn a
                           ; case (value (arg 0), value (arg 2)) of
                                (Array {elt = e, ...}, Vector {elt = e', ...}) =>
                                   unifySlot (e, e')
                              | _ => Error.bug "Useless.primApp: Array_copyVector"
                         end
                   | Prim.Array_length => return (arrayLength (arg 0))
                   | Prim.Array_sub => sub ()
                   | Prim.Array_toArray =>
                        (case (value (arg 0), value result) of
                            (Array {length = l, elt = e, ...},
                             Array {length = l', elt = e', ...}) =>
                               (unify (l, l'); unifySlot (e, e'))
                           | _ => Error.bug "Useless.primApp: Array_toArray")
                   | Prim.Array_toVector =>
                        (case (value (arg 0), value result) of
                            (Array {length = l, elt = e, ...},
                             Vector {length = l', elt = e', ...}) =>
                               (unify (l, l'); unifySlot (e, e'))
                           | _ => Error.bug "Useless.primApp: Array_toVector")
                   | Prim.Array_uninit =>
                        let
                           val a = dearray (arg 0)
                        in
                           arg 1 dependsOn a
                        end
                   | Prim.Array_uninitIsNop =>
                        Useful.whenUseful
                        (deground result, fn () =>
                         Useful.makeUseful (arrayUseful (arg 0)))
                   | Prim.Array_update => update ()
                   | Prim.CFunction _ =>
                        (Vector.foreach (args, deepMakeUseful);
                         deepMakeUseful result)
                   | Prim.MLton_eq =>
                        Useful.whenUseful
                        (deground result, fn () =>
                         (unify (arg 0, arg 1)
                          ; makeWanted (arg 1)))
                   | Prim.MLton_equal =>
                        Useful.whenUseful
                        (deground result, fn () =>
                         (shallowMakeUseful (arg 0)
                          ; shallowMakeUseful (arg 1)))
                   | Prim.MLton_hash =>
                        Useful.whenUseful
                        (deground result, fn () =>
                         shallowMakeUseful (arg 0))
                   | Prim.MLton_share => makeWanted (arg 0)
                   | Prim.MLton_size =>
                        Useful.whenUseful
                        (deground result, fn () =>
                         makeWanted (arg 0))
                   | Prim.MLton_touch => shallowMakeUseful (arg 0)
                   | Prim.Ref_assign => coerce {from = arg 1, to = deref (arg 0)}
                   | Prim.Ref_deref => return (deref (arg 0))
                   | Prim.Ref_ref => coerce {from = arg 0, to = deref result}
                   | Prim.Vector_length => return (vectorLength (arg 0))
                   | Prim.Vector_sub => (arg 1 dependsOn result
                                         ; return (devector (arg 0)))
                   | Prim.Vector_vector =>
                        let
                           val l =
                              (const o S.Const.word o WordX.fromInt)
                              (Vector.length args, WordSize.seqIndex ())
                        in
                           (coerce {from = l, to = vectorLength result}
                            ; Vector.foreach
                              (args, fn arg =>
                               coerce {from = arg, to = devector result}))
                        end
                   | Prim.Weak_canGet =>
                        Useful.whenUseful
                        (deground result, fn () =>
                         Useful.makeUseful (weakUseful (arg 0)))
                   | Prim.Weak_get => return (deweak (arg 0))
                   | Prim.Weak_new => coerce {from = arg 0, to = deweak result}
                   | Prim.WordArray_subWord _ => sub ()
                   | Prim.WordArray_updateWord _ => update ()
                   | _ =>
                        let
                           (* allOrNothing so the type doesn't change *)
                           val res = allOrNothing result
                        in
                           if Prim.maySideEffect prim
                              then Vector.foreach (args, deepMakeUseful)
                              else Vector.foreach (args, fn a =>
                                                   case (allOrNothing a, res) of
                                                      (SOME u, SOME u') =>
                                                         Useful.<= (u', u)
                                                    | _ => ())
                        end
            in
               result
            end
         val primApp =
            Trace.trace
            ("Useless.primApp",
             fn {prim, args, ...} =>
             Layout.seq [Prim.layout prim,
                         Vector.layout layout args],
             layout)
            primApp
      end
      val {value, func, label, ...} =
         analyze {
                  coerce = Value.coerce,
                  conApp = conApp,
                  const = Value.const,
                  filter = filter,
                  filterWord = filterGround o #1,
                  fromType = Value.fromType,
                  layout = Value.layout,
                  primApp = primApp,
                  program = program,
                  select = Value.select,
                  tuple = Value.tuple,
                  useFromTypeOnBinds = true
                  }
      open Exp Transfer
      val _ =
         Control.diagnostics
         (fn display =>
          let
             open Layout
             val _ =
                Vector.foreach
                (datatypes, fn Datatype.T {tycon, cons} =>
                 display
                 (align
                  [Tycon.layout tycon,
                   indent (Vector.layout
                           (fn {con, ...} =>
                            seq [Con.layout con, str " ",
                                 Vector.layout Value.layout (conArgs con)])
                           cons,
                           2)]))
             fun diagVar x =
                display (seq [Var.layout x,
                              str " ", Value.layout (value x)])
             val _ =
                Vector.foreach
                (globals, fn Statement.T {var, ...} =>
                 Option.app (var, diagVar))
             val _ =
                List.foreach
                (functions, fn f =>
                 let
                    val {name, ...} = Function.dest f
                    val _ = display (seq [str "Useless info for ",
                                          Func.layout name])
                    val {args, returns, raises} = func name
                    val _ =
                       display
                       (record [("args", Vector.layout Value.layout args),
                                ("returns",
                                 Option.layout (Vector.layout Value.layout)
                                 returns),
                                ("raises", 
                                 Option.layout (Vector.layout Value.layout)
                                 raises)])
                    val _ =
                       Function.foreachVar
                       (f, fn (x, _) => diagVar x)
                 in
                    ()
                 end)
          in
             ()
          end)
      val varExists = Value.isUseful o value
      val unitVar = Var.newString "unit"
      val bogusGlobals: Statement.t list ref = ref []
      val {get = bogus, destroy, ...} =
         Property.destGet
         (Type.plist,
          Property.initFun
          (fn ty =>
           let val var = Var.newString "bogus"
           in List.push (bogusGlobals,
                         Statement.T
                         {var = SOME var, 
                          ty = ty,
                          exp = PrimApp {prim = Prim.MLton_bogus,
                                         targs = Vector.new1 ty,
                                         args = Vector.new0 ()}})
              ; var
           end))
      fun keepUseful (xs: Var.t vector, vs: Value.t vector): Var.t vector =
         Vector.keepAllMap2
         (xs, vs, fn (x, v) =>
          let val (t, b) = Value.getNew v
          in if b
                then SOME (if varExists x then x else bogus t)
             else NONE
          end)
      fun keepUsefulArgs (xts: (Var.t * Type.t) vector) =
         Vector.keepAllMap
         (xts, fn (x, _) =>
          let val (t, b) = Value.getNew (value x)
          in if b
                then SOME (x, t)
             else NONE
          end)
      val keepUsefulArgs =
         Trace.trace ("Useless.keepUsefulArgs",
                      Vector.layout (Layout.tuple2 (Var.layout, Type.layout)),
                      Vector.layout (Layout.tuple2 (Var.layout, Type.layout)))
         keepUsefulArgs
      fun dropUseless (vs: Value.t vector,
                       vs': Value.t vector,
                       makeTrans: Var.t vector -> Transfer.t): Label.t * Block.t =
         let
            val l = Label.newNoname ()
            val (formals, actuals) =
               Vector.unzip
               (Vector.map2
                (vs, vs', fn (v, v') =>
                 if Value.isUseful v
                   then let val x = Var.newNoname ()
                        in (SOME (x, Value.newType v),
                            if Value.isUseful v'
                               then SOME x
                            else NONE)
                        end
                 else (NONE, NONE)))
         in (l, Block.T {label = l,
                         args = Vector.keepAllSome formals,
                         statements = Vector.new0 (),
                         transfer = makeTrans (Vector.keepAllSome actuals)})
         end
      (* Returns true if the component is the only component of the tuple
       * that exists.
       *)
      fun newOffset (bs: bool vector, n: int): int * bool =
         let
            val len = Vector.length bs
            fun loop (pos, n, i) =
               let val b = Vector.sub (bs, pos)
               in if n = 0
                     then (i, (i = 0
                               andalso not (Int.exists (pos + 1, len, fn i =>
                                                        Vector.sub (bs, i)))))
                  else loop (pos + 1, n - 1, if b then i + 1 else i)
               end
         in loop (0, n, 0)
         end

      fun doitExp (e: Exp.t, resultType: Type.t, resultValue: Value.t option) =
         case e of
            ConApp {con, args} =>
               ConApp {con = con,
                       args = keepUseful (args, conArgs con)}
          | Const _ => e
          | PrimApp {prim, args, ...} => 
               let
                  fun arg i = Vector.sub (args, i)
                  fun doit () =
                     let
                        val (args, argTypes) =
                           Vector.unzip
                           (Vector.map (args, fn x =>
                                        let
                                           val (t, b) = Value.getNew (value x)
                                        in
                                           if b
                                              then (x, t)
                                              else (unitVar, Type.unit)
                                        end))
                     in
                        PrimApp
                        {prim = prim,
                         args = args,
                         targs = (Prim.extractTargs
                                  (prim,
                                   {args = argTypes,
                                    result = resultType,
                                    typeOps = {deArray = Type.deArray,
                                               deArrow = fn _ => Error.bug "Useless.doitExp: deArrow",
                                               deRef = Type.deRef,
                                               deVector = Type.deVector,
                                               deWeak = Type.deWeak}}))}
                     end
                  fun makePtr dePtr =
                     if Type.isUnit (dePtr resultType)
                        then PrimApp {prim = prim,
                                      targs = Vector.new1 Type.unit,
                                      args = Vector.new1 unitVar}
                        else doit ()
                  fun makeSeq deSeq =
                     if Type.isUnit (deSeq resultType)
                        then PrimApp {prim = prim,
                                      targs = Vector.new1 Type.unit,
                                      args = Vector.map (args, fn _ => unitVar)}
                        else doit ()
               in
                  case prim of
                     Prim.Array_uninitIsNop =>
                        if varExists (Vector.sub (args, 0))
                           then doit ()
                           else ConApp {args = Vector.new0 (),
                                        con = Con.truee}
                   | Prim.Array_array => makeSeq Type.deArray
                   | Prim.MLton_equal =>
                        let
                           val (t0, _) = Value.getNew (value (arg 0))
                           val (t1, _) = Value.getNew (value (arg 1))
                        in
                           if Type.equals (t0, t1)
                              then PrimApp {prim = prim,
                                            targs = Vector.new1 t0,
                                            args = args}
                              else (* The arguments differ in the usefulness of
                                    * contents of an Array, Ref, or Weak and
                                    * the corresponding Array, Ref, or Weak
                                    * objects must be distinct and, therefore,
                                    * not equal.
                                    *)
                                   ConApp {args = Vector.new0 (),
                                           con = Con.falsee}
                        end
                   | Prim.Ref_ref => makePtr Type.deRef
                   | Prim.Vector_vector => makeSeq Type.deVector
                   | Prim.Weak_new => makePtr Type.deWeak
                   | _ => doit ()
               end
          | Select {tuple, offset} =>
               let
                  val (offset, isOne) =
                     newOffset (Vector.map (Value.detupleSlots (value tuple),
                                            Exists.doesExist o #2),
                                offset)
               in if isOne
                     then Var tuple
                  else Select {tuple = tuple,
                               offset = offset}
               end
          | Tuple xs =>
               let
                  val slots = Value.detupleSlots (valOf resultValue)
                  val xs =
                     Vector.keepAllMap2
                     (xs, slots, fn (x, (v, e)) =>
                      if Exists.doesExist e
                         then SOME (if varExists x then x
                                    else bogus (Value.newType v))
                      else NONE)
               in
                  if 1 = Vector.length xs
                     then Var (Vector.first xs)
                  else Tuple xs
               end
          | Var _ => e
          | _ => e
      val doitExp =
         Trace.trace3 ("Useless.doitExp",
                       Exp.layout, Layout.ignore, Layout.ignore,
                       Exp.layout) 
         doitExp
      fun doitStatement (Statement.T {var, exp, ty}) =
         let
            val v = Option.map (var, value)
            val (ty, b) =
               case v of
                  NONE => (ty, false)
                | SOME v => Value.getNew v
            fun yes ty =
               SOME (Statement.T 
                     {var = var, 
                      ty = ty, 
                      exp = doitExp (exp, ty, v)})
         in
            if b
               then yes ty
            else
               case exp of
                  PrimApp {prim, args, ...} =>
                     if Prim.maySideEffect prim
                        andalso let
                                   fun arg i = Vector.sub (args, i)
                                   fun array () =
                                      Value.isUseful
                                      (Value.dearray (value (arg 0)))
                                in
                                   case prim of
                                      Prim.Array_copyArray => array ()
                                    | Prim.Array_copyVector => array ()
                                    | Prim.Array_uninit => array ()
                                    | Prim.Array_update => array ()
                                    | Prim.Ref_assign =>
                                         Value.isUseful
                                         (Value.deref (value (arg 0)))
                                    | Prim.WordArray_updateWord _ => array ()
                                    | _ => true
                                end
                        then yes ty
                     else NONE
                | Profile _ => yes ty
                | _ => NONE
         end
      val doitStatement =
         Trace.trace ("Useless.doitStatement", 
                      Statement.layout, Option.layout Statement.layout)
         doitStatement
      fun agree (v: Value.t, v': Value.t): bool =
         Value.isUseful v = Value.isUseful v'
      fun agrees (vs, vs') = Vector.forall2 (vs, vs', agree)
      val agrees =
         Trace.trace2 ("Useless.agrees",
                       Vector.layout Value.layout,
                       Vector.layout Value.layout,
                       Bool.layout)
         agrees
      fun doitTransfer (t: Transfer.t, 
                        returns: Value.t vector option,
                        raises: Value.t vector option)
         : Block.t list * Transfer.t =
         case t of
            Bug => ([], Bug)
          | Call {func = f, args, return} =>
               let
                  val {args = fargs, returns = freturns, raises = fraises} = func f
                  fun bug () =
                     let
                        val l = Label.newNoname ()
                     in
                        (l,
                         Block.T {label = l,
                                  args = Vector.new0 (),
                                  statements = Vector.new0 (),
                                  transfer = Bug})
                     end
                  fun wrap (froms, tos, mkTrans) =
                     case (froms, tos) of
                        (NONE, NONE) => (true, bug)
                      | (NONE, SOME _) => (true, bug)
                      | (SOME _, NONE) => Error.bug "Useless.doitTransfer: Call mismatch"
                      | (SOME froms, SOME tos) =>
                           (agrees (froms, tos), fn () =>
                            dropUseless (froms, tos, mkTrans))
                  val (blocks, return) =
                     case return of
                        Return.Dead => ([], return)
                      | Return.Tail =>
                           (case (wrap (freturns, returns, Return), wrap (fraises, raises, Raise)) of
                               ((true, _), (true, _)) =>
                                  ([], Return.Tail)
                             | ((false, mkc), (true, _)) =>
                                  let
                                     val (lc, bc) = mkc ()
                                  in
                                     ([bc],
                                      Return.NonTail {cont = lc, handler = Handler.Caller})
                                  end
                             | ((_, mkc), (false, mkh)) =>
                                  let
                                     val (lc, bc) = mkc ()
                                     val (lh, bh) = mkh ()
                                  in
                                     ([bc, bh],
                                      Return.NonTail {cont = lc,
                                                      handler = Handler.Handle lh})
                                  end)
                      | Return.NonTail {cont, handler} =>
                           let
                              val returns = SOME (label cont)
                              val mkct = fn args => Goto {dst = cont, args = args}
                              val (raises, mkht) =
                                 case handler of
                                    Handler.Dead => (NONE, fn _ => Bug)
                                  | Handler.Caller => (raises, Raise)
                                  | Handler.Handle hand =>
                                       (SOME (label hand),
                                        fn args => Goto {dst = hand, args = args})
                           in
                              case (wrap (freturns, returns, mkct), wrap (fraises, raises, mkht)) of
                                 ((true, _), (true, _)) =>
                                    ([],
                                     Return.NonTail {cont = cont,
                                                     handler = handler})
                               | ((false, mkc), (true, _)) =>
                                    let
                                       val (lc, bc) = mkc ()
                                    in
                                       ([bc],
                                        Return.NonTail {cont = lc,
                                                        handler = handler})
                                    end
                               | ((true, _), (false, mkh)) =>
                                    let
                                       val (lh, bh) = mkh ()
                                    in
                                       ([bh],
                                        Return.NonTail {cont = cont,
                                                        handler = Handler.Handle lh})
                                    end
                               | ((false, mkc), (false, mkh)) =>
                                    let
                                       val (lc, bc) = mkc ()
                                       val (lh, bh) = mkh ()
                                    in
                                       ([bc, bh],
                                        Return.NonTail {cont = lc,
                                                        handler = Handler.Handle lh})
                                    end
                           end
               in (blocks,
                   Call {func = f, 
                         args = keepUseful (args, fargs), 
                         return = return})
               end
          | Case {test, cases, default} => 
               let
                  datatype z = datatype Cases.t
               in
                  case cases of
                     Con cases =>
                        (case (Vector.length cases, default) of
                            (0, NONE) => ([], Bug)
                          | _ => 
                               let
                                  val (cases, blocks) =
                                     Vector.mapAndFold
                                     (cases, [], fn ((c, l), blocks) =>
                                      let
                                         val args = label l
                                      in if Vector.forall (args, Value.isUseful)
                                            then ((c, l), blocks)
                                         else
                                            let
                                               val (l', b) =
                                                  dropUseless
                                                  (conArgs c, args, fn args =>
                                                   Goto {dst = l, args = args})
                                            in ((c, l'), b :: blocks)
                                            end
                                      end)
                               in (blocks, 
                                   Case {test = test, 
                                         cases = Cases.Con cases,
                                         default = default})
                               end)
                   | Word (_, cs) =>
                        (* The test may be useless if there are no cases or
                         * default, thus we must eliminate the case.
                         *)
                        case (Vector.length cs, default) of
                           (0, NONE) => ([], Bug)
                         | _ => ([], t)
               end
          | Goto {dst, args} =>
               ([], Goto {dst = dst, args = keepUseful (args, label dst)})
          | Raise xs => ([], Raise (keepUseful (xs, valOf raises)))
          | Return xs => ([], Return (keepUseful (xs, valOf returns)))
          | Runtime {prim, args, return} =>
               ([], Runtime {prim = prim, args = args, return = return})
      val doitTransfer =
         Trace.trace3 ("Useless.doitTransfer",
                       Transfer.layout,
                       Option.layout (Vector.layout Value.layout),
                       Option.layout (Vector.layout Value.layout),
                       Layout.tuple2 (List.layout (Label.layout o Block.label), 
                                      Transfer.layout))
         doitTransfer
      fun doitBlock (Block.T {label, args, statements, transfer},
                     returns: Value.t vector option,
                     raises: Value.t vector option)
         : Block.t list * Block.t =
         let
            val args = keepUsefulArgs args
            val statements = Vector.keepAllMap (statements, doitStatement)
            val (blocks, transfer) = doitTransfer (transfer, returns, raises)
         in
           (blocks, Block.T {label = label,
                             args = args,
                             statements = statements,
                             transfer = transfer})
         end
      val doitBlock =
         Trace.trace3 ("Useless.doitBlock",
                       Label.layout o Block.label,
                       Option.layout (Vector.layout Value.layout),
                       Option.layout (Vector.layout Value.layout),
                       Layout.tuple2 (List.layout (Label.layout o Block.label), 
                                      (Label.layout o Block.label)))
         doitBlock
      fun doitFunction f =
         let
            val {args, blocks, mayInline, name, start, ...} = Function.dest f
            val {returns = returnvs, raises = raisevs, ...} = func name
            val args = keepUsefulArgs args
            val (blocks, blocks') =
               Vector.mapAndFold
               (blocks, [], fn (block, blocks') =>
                let val (blocks'', block) = doitBlock (block, returnvs, raisevs)
                in (block, blocks''::blocks')
                end)
            val blocks =
               Vector.concat (blocks :: List.map (blocks', Vector.fromList))
            val returns = Option.map (returnvs, Value.newTypes)
            val raises = Option.map (raisevs, Value.newTypes)
         in
            Function.new {args = args,
                          blocks = blocks,
                          mayInline = mayInline,
                          name = name,
                          raises = raises,
                          returns = returns,
                          start = start}
         end
      val datatypes =
         Vector.map
         (datatypes, fn Datatype.T {tycon, cons} =>
          Datatype.T {tycon = tycon,
                      cons = Vector.map (cons, fn {con, ...} =>
                                         {con = con,
                                          args = Value.newTypes (conArgs con)})})
      val globals =
         Vector.concat
         [Vector.new1 (Statement.T {var = SOME unitVar,
                                    ty = Type.unit,
                                    exp = Exp.unit}),
          Vector.keepAllMap (globals, doitStatement)]
      val shrink = shrinkFunction {globals = globals}
      val functions = List.map (functions, shrink o doitFunction)
      val globals = Vector.concat [Vector.fromList (!bogusGlobals),
                                   globals]
      val program = Program.T {datatypes = datatypes,
                               globals = globals,
                               functions = functions,
                               main = main}
      val _ = destroy ()
      val _ = Program.clearTop program
   in
      program
   end
end
