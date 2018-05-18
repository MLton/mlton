(* Copyright (C) 2009,2017 Matthew Fluet.
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
 *  remove components of tuples that are constants (use unification)
 *  remove function arguments that are constants
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

(* Weirdness with raise/handle.
 * There must be a uniform "calling convention" for raise and handle.
 * Hence, just because some of a handlers args are useless, that doesn't mean
 * that it can drop them, since they may be useful to another handler, and
 * hence every raise will pass them along.  The problem is that it is not
 * possible to tell solely from looking at a function declaration whether it is
 * a handler or not, and in fact, there is nothing preventing a jump being used
 * in both ways.  So, maybe the right thing is for the handler wrapper to
 * do
 * Another solution would be to unify all handler args.
 *)

structure Value =
   struct
      structure Set = DisjointSet

      structure Exists =
         struct
            structure L = TwoPointLattice (val bottom = "not exists"
                                           val top = "exists")
            open L
            val mustExist = makeTop
            val doesExist = isTop
         end

      structure Useful =
         struct
            structure L = TwoPointLattice (val bottom = "useless"
                                           val top = "useful")
            open L
            val makeUseful = makeTop
            val isUseful = isTop
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
            end
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
                  (Array {length = n, elt = e, ...},
                   Array {length = n', elt = e', ...}) =>
                     (unify (n, n'); unifySlot (e, e'))
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
                     in Useful.addHandler
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
                           in Exists.addHandler
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
      end

      fun deref (r: t): t =
         case value r of
            Ref {arg, ...} => #1 arg
          | _ => Error.bug "Useless.deref"

      fun deweak (v: t): t =
         case value v of
            Weak {arg, ...} => #1 arg
          | _ => Error.bug "Useless.deweak"

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
                                    argTypes: Type.t vector,
                                    value: unit -> Value.t},
           set = setConInfo, ...} =
         Property.getSetOnce 
         (Con.plist, Property.initRaise ("conInfo", Con.layout))
      val {get = tyconInfo: Tycon.t -> {useful: bool ref,
                                        cons: Con.t vector},
           set = setTyconInfo, ...} =
         Property.getSetOnce 
         (Tycon.plist, Property.initRaise ("tyconInfo", Tycon.layout))
      local open Value
      in
         val _ =
            Vector.foreach
            (datatypes, fn Datatype.T {tycon, cons} =>
             let
                val _ =
                   setTyconInfo (tycon, {useful = ref false,
                                         cons = Vector.map (cons, #con)})
                fun value () = fromType (Type.datatypee tycon)
             in Vector.foreach
                (cons, fn {con, args} =>
                 setConInfo (con, {value = value,
                                   argTypes = args,
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
         fun filterGround (v: Value.t): unit =
            case value v of
               Ground g => Useful.makeUseful g
             | _ => Error.bug "Useless.filterGround: non ground"
         val filter =
            Trace.trace3 ("Useless.filter",
                          Value.layout,
                          Con.layout,
                          Vector.layout Value.layout,
                          Unit.layout)
            filter
         (* This is used for primitive args, since we have no idea what
          * components of its args that a primitive will look at.
          *)
         fun deepMakeUseful v =
            let
               val slot = deepMakeUseful o #1
            in
               case value v of
                  Array {useful, length, elt} =>
                     (Useful.makeUseful useful
                      ; deepMakeUseful length
                      ; slot elt)
                | Ground u =>
                     (Useful.makeUseful u
                      (* Make all constructor args of this tycon useful *)
                      ; (case Type.dest (ty v) of
                            Type.Datatype tycon =>
                               let val {useful, cons} = tyconInfo tycon
                               in if !useful
                                     then ()
                                  else (useful := true
                                        ; Vector.foreach (cons, fn con =>
                                                          Vector.foreach
                                                          (#args (conInfo con),
                                                           deepMakeUseful)))
                               end
                          | _ => ()))
                | Ref {arg, useful} => (Useful.makeUseful useful; slot arg)
                | Tuple vs => Vector.foreach (vs, slot)
                | Vector {length, elt} => (deepMakeUseful length; slot elt)
                | Weak {arg, useful} => (Useful.makeUseful useful; slot arg)
            end

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
               datatype z = datatype Prim.Name.t
               val _ =
                  case Prim.name prim of
                     Array_alloc _ =>
                        coerce {from = arg 0, to = arrayLength result}
                   | Array_copyArray =>
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
                   | Array_copyVector =>
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
                   | Array_length => return (arrayLength (arg 0))
                   | Array_sub => sub ()
                   | Array_toArray =>
                        (case (value (arg 0), value result) of
                            (Array {length = l, elt = e, ...},
                             Array {length = l', elt = e', ...}) =>
                               (unify (l, l'); unifySlot (e, e'))
                           | _ => Error.bug "Useless.primApp: Array_toArray")
                   | Array_toVector =>
                        (case (value (arg 0), value result) of
                            (Array {length = l, elt = e, ...},
                             Vector {length = l', elt = e', ...}) =>
                               (unify (l, l'); unifySlot (e, e'))
                           | _ => Error.bug "Useless.primApp: Array_toVector")
                   | Array_uninit =>
                        let
                           val a = dearray (arg 0)
                        in
                           arg 1 dependsOn a
                        end
                   | Array_uninitIsNop =>
                        (* Array_uninitIsNop is Functional, but
                         * performing Useless.<= (allOrNothing result,
                         * allOrNothing (arg 0)) would effectively
                         * make the whole array useful, inhibiting the
                         * Useless optimization.
                         *)
                        ()
                   | Array_update => update ()
                   | FFI _ =>
                        (Vector.foreach (args, deepMakeUseful);
                         deepMakeUseful result)
                   | MLton_equal => Vector.foreach (args, deepMakeUseful)
                   | MLton_hash => Vector.foreach (args, deepMakeUseful)
                   | Ref_assign => coerce {from = arg 1, to = deref (arg 0)}
                   | Ref_deref => return (deref (arg 0))
                   | Ref_ref => coerce {from = arg 0, to = deref result}
                   | Vector_length => return (vectorLength (arg 0))
                   | Vector_sub => (arg 1 dependsOn result
                                    ; return (devector (arg 0)))
                   | Vector_vector =>
                        let
                           val l =
                              (const o S.Const.word o WordX.fromIntInf)
                              (IntInf.fromInt (Vector.length args),
                               WordSize.seqIndex ())
                        in
                           (coerce {from = l, to = vectorLength result}
                            ; Vector.foreach
                              (args, fn arg =>
                               coerce {from = arg, to = devector result}))
                        end
                   | Weak_get => return (deweak (arg 0))
                   | Weak_new => coerce {from = arg 0, to = deweak result}
                   | WordArray_subWord _ => sub ()
                   | WordArray_updateWord _ => update ()
                   | _ =>
                        let (* allOrNothing so the type doesn't change *)
                           val res = allOrNothing result
                        in if Prim.maySideEffect prim
                              then Vector.foreach (args, deepMakeUseful)
                           else
                              Vector.foreach (args, fn a =>
                                              case (allOrNothing a, res) of
                                                 (NONE, _) => ()
                                               | (SOME u, SOME u') =>
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
      (* Unify all handler args so that raise/handle has a consistent calling
       * convention.
       *)
      val _ =
         List.foreach
         (functions, fn f =>
          let
             val {raises = fraisevs, ...} = func (Function.name f)
             fun coerce (x, y) = Value.coerce {from = x, to = y}
          in
             Vector.foreach
             (Function.blocks f, fn Block.T {transfer, ...} =>
              case transfer of
                 Call {func = g, return, ...} =>
                    let
                       val {raises = graisevs, ...} = func g
                       fun coerceRaise () =
                          case (graisevs, fraisevs) of
                             (NONE, NONE) => ()
                           | (NONE, SOME _) => ()
                           | (SOME _, NONE) =>
                                Error.bug "Useless.useless: raise mismatch at Caller"
                           | (SOME vs, SOME vs') =>
                                Vector.foreach2 (vs', vs, coerce)
                    in
                      case return of
                         Return.Dead => ()
                       | Return.NonTail {handler, ...} =>
                            (case handler of
                                Handler.Caller => coerceRaise ()
                              | Handler.Dead => ()
                              | Handler.Handle h =>
                                   Option.app
                                   (graisevs, fn graisevs =>
                                    Vector.foreach2 
                                    (label h, graisevs, coerce)))
                       | Return.Tail => coerceRaise ()
                    end
               | _ => ())
          end)
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
                       (f, fn (x, _) => 
                        display (seq [Var.layout x,
                                      str " ", Value.layout (value x)]))
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
                          exp = PrimApp {prim = Prim.bogus,
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
                  datatype z = datatype Prim.Name.t
               in
                  case Prim.name prim of
                     Array_uninitIsNop =>
                        if varExists (Vector.sub (args, 0))
                           then doit ()
                           else ConApp {args = Vector.new0 (),
                                        con = Con.falsee}
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
                                   datatype z = datatype Prim.Name.t
                                in
                                   case Prim.name prim of
                                      Array_copyArray => array ()
                                    | Array_copyVector => array ()
                                    | Array_uninit => array ()
                                    | Array_update => array ()
                                    | Ref_assign =>
                                         Value.isUseful
                                         (Value.deref (value (arg 0)))
                                    | WordArray_updateWord _ => array ()
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
            Arith {prim, args, overflow, success, ty} =>
               let
                  val v = Value.fromType ty
                  val _ = Value.Useful.makeUseful (Value.deground v)
                  val res = Vector.new1 v
                  val sargs = label success
               in
                  if agree (v, Vector.first sargs)
                     then ([], t)
                  else let
                          val (l, b) = dropUseless
                                       (res, sargs, fn args =>
                                        Goto {dst = success, args = args})
                       in
                          ([b],
                           Arith {prim = prim,
                                  args = args,
                                  overflow = overflow,
                                  success = l,
                                  ty = ty})
                       end
               end
          | Bug => ([], Bug)
          | Call {func = f, args, return} =>
               let
                  val {args = fargs, returns = freturns, ...} = func f
                  val (blocks, return) =
                     case return of
                        Return.Dead => ([], return)
                      | Return.Tail =>
                           (case (returns, freturns) of
                               (NONE, NONE) => ([], Return.Tail)
                             | (NONE, SOME _) => Error.bug "Useless.doitTransfer: return mismatch"
                             | (SOME _, NONE) => ([], Return.Tail)
                             | (SOME returns, SOME freturns) =>
                                  if agrees (freturns, returns)
                                     then ([], Return.Tail)
                                  else
                                     let
                                        val (l, b) =
                                           dropUseless
                                           (freturns, returns, Return)
                                     in ([b],
                                         Return.NonTail
                                         {cont = l,
                                          handler = Handler.Caller})
                                     end)
                      | Return.NonTail {cont, handler} =>
                           (case freturns of
                               NONE => ([], return)
                             | SOME freturns => 
                                  let val returns = label cont
                                  in if agrees (freturns, returns)
                                        then ([], return)
                                     else let
                                             val (l, b) =
                                                dropUseless
                                                (freturns, returns, fn args =>
                                                 Goto {dst = cont, args = args})
                                          in ([b],
                                              Return.NonTail
                                              {cont = l, handler = handler})
                                          end
                                  end)
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
