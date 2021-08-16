(* Copyright (C) 2021 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor FlatLattice (S: FLAT_LATTICE_STRUCTS): FLAT_LATTICE =
struct

open S

structure Elt =
   struct
      datatype t =
         Bottom
       | Point of Point.t
       | Top

      local
         open Layout
      in
         val layout =
            fn Bottom => str "Bottom"
             | Point p => Point.layout p
             | Top => str "Top"
      end
   end
datatype z = datatype Elt.t

datatype t = T of {lessThan: t list ref,
                   upperBound: Point.t option ref,
                   value: Elt.t ref}

fun layout (T {value, ...}) = Elt.layout (!value)

fun new () = T {lessThan = ref [],
                upperBound = ref NONE,
                value = ref Bottom}

val isBottom =
   fn (T {value = ref Bottom, ...}) => true
    | _ => false
val isPoint =
   fn (T {value = ref (Point _), ...}) => true
    | _ => false
val isPointEq = 
   fn (T {value = ref (Point p), ...}, p') => Point.equals (p, p')
    | _ => false
val getPoint =
   fn (T {value = ref (Point p), ...}) => SOME p
    | _ => NONE
val isTop =
   fn (T {value = ref Top, ...}) => true
    | _ => false

fun forceTop (T {upperBound, value, ...}): bool =
   if isSome (!upperBound)
      then false
   else (value := Top; true)

fun up (T {lessThan, upperBound, value, ...}, e: Elt.t): bool =
   let
      fun continue e = List.forall (!lessThan, fn z => up (z, e))
      fun setTop () =
         not (isSome (!upperBound))
         andalso (value := Top
                  ; continue Top)
   in
      case (!value, e) of
         (_, Bottom) => true
       | (Top, _) => true
       | (_, Top) => setTop ()
       | (Bottom, Point p) =>
            (value := Point p
             ; (case !upperBound of
                   NONE => continue (Point p)
                 | SOME p' =>
                      Point.equals (p, p') andalso continue (Point p)))
       | (Point p, Point p') => Point.equals (p, p') orelse setTop ()
   end

val op <= : t * t -> bool =
   fn (T {lessThan, value, ...}, e) =>
   (List.push (lessThan, e)
    ; up (e, !value))

val op <= =
   Trace.trace2 ("FlatLattice.<=", layout, layout, Bool.layout)
   (op <=)

fun lowerBound (e, p): bool = up (e, Point p)

val lowerBound =
   Trace.trace2 ("FlatLattice.lowerBound", layout, Point.layout, Bool.layout)
   lowerBound

fun upperBound (T {upperBound = r, value, ...}, p): bool =
   case !r of
      NONE => (r := SOME p
               ; (case !value of
                     Bottom => true
                   | Point p' => Point.equals (p, p')
                   | Top => false))
    | SOME p' => Point.equals (p, p')

val upperBound =
   Trace.trace2 ("FlatLattice.upperBound", layout, Point.layout, Bool.layout)
   upperBound

fun forcePoint (e, p) =
   lowerBound (e, p) andalso upperBound (e, p)

val forcePoint =
   Trace.trace2 ("FlatLattice.forcePoint", layout, Point.layout, Bool.layout)
   forcePoint

fun point p =
   let
      val e = new ()
      val _ = forcePoint (e, p)
   in
      e
   end

val point = Trace.trace ("FlatLattice.point", Point.layout, layout) point

end



functor FlatLatticeRec (S: FLAT_LATTICE_REC_STRUCTS): FLAT_LATTICE_REC =
struct

open S

structure Handlers =
   struct
      datatype t = T of unit -> unit
      fun run (T hs) = hs ()
      val empty = T (fn () => ())
      fun cons (h: unit -> unit, hs) = T (fn () => (h () ; run hs))
   end

structure Value =
   struct
      datatype 'a t =
         Bottom
       | Point of 'a Point.t
       | Top

      local
         open Layout
      in
         fun layout layoutA v =
            case v of
               Bottom => str bottom
             | Point p => Point.layout layoutA p
             | Top => str top
      end

      fun isBottom v =
         case v of Bottom => true | _ => false
      fun isPoint v =
         case v of Point _ => true | _ => false
      fun isPointEq equalsA (v, p') =
         case v of Point p => Point.equals equalsA (p, p') | _ => false
      fun isTop v =
         case v of Top => true | _ => false

      fun coerce {clone = cloneA, coerce = coerceA, equals = equalsA}
                 {from, to}: 'a t option =
         let
            val pointClone = Point.clone {clone = cloneA, equals = equalsA}
            val pointCoerce = Point.coerce {clone = cloneA, coerce = coerceA, equals = equalsA}
         in
            case (from, to) of
               (_, Top) => NONE
             | (Top, _) => SOME Top
             | (Point from, Bottom) =>
                  let
                     val to = pointClone from
                  in
                     if pointCoerce {from = from, to = to}
                        then SOME (Point to)
                        else SOME Top
                  end
             | (Point from, Point to) =>
                  if pointCoerce {from = from, to = to}
                     then NONE
                     else SOME Top
             | (Bottom, _) => NONE
         end

      fun unify {equals = equalsA, unify = unifyA}
                (v1, v2): (bool * bool * 'a t) =
         let
            val pointUnify = Point.unify {equals = equalsA, unify = unifyA}
         in
            case (v1, v2) of
               (Bottom, Bottom) => (false, false, Bottom)
             | (Bottom, _) => (true, false, v2)
             | (_, Bottom) => (false, true, v1)
             | (Point p1, Point p2) =>
                  if pointUnify (p1, p2)
                     then (false, false, Point p1)
                     else (true, true, Top)
             | (Top, Top) => (false, false, Top)
             | (Top, _) => (false, true, Top)
             | (_, Top) => (true, false, Top)
         end
   end

datatype 'a t = T of {handlers: Handlers.t Ref.t,
                      lessThan: 'a t List.t Ref.t,
                      value: 'a Value.t Ref.t}

local
   fun mk' sel (T s) = sel s
   fun mk sel e = ! (mk' sel e)
in
   fun handlersRef e = mk' #handlers e
   fun handlers e = mk #handlers e
   (* fun lessThanRef e = mk' #lessThan e *)
   (* fun lessThan e = mk #lessThan e *)
   fun valueRef e = mk' #value e
   fun value e = mk #value e
end

(*
fun setValue (e, value) =
   valueRef e := value
*)

fun equals (e1, e2) = Ref.equals (valueRef e1, valueRef e2)

fun getPoint e =
   case value e of
      Value.Point p => SOME p
    | _ => NONE
fun isBottom e = Value.isBottom (value e)
fun isPoint e = Value.isPoint (value e)
fun isPointEq equalsA (e, p') = Value.isPointEq equalsA (value e, p')
fun isTop e = Value.isTop (value e)

fun layout layoutA e =
   Value.layout layoutA (value e)

fun new value = T {handlers = Ref.new Handlers.empty,
                   lessThan = Ref.new List.empty,
                   value = Ref.new value}

fun newBottom () = new Value.Bottom
fun newPoint p = new (Value.Point p)
fun newTop () = new Value.Top

(*
fun setLessThan (e, lessThan) =
   lessThanRef e := lessThan
*)

fun setHandlers (e, handlers) =
   handlersRef e := handlers
fun addHandler (e, h) =
   (setHandlers (e, Handlers.cons (h, handlers e))
    ; h ())
fun addHandler' (e, h) = addHandler (e, fn () => h (value e))

fun makeTop (T {handlers, lessThan, value}): unit =
   case !value of
      Value.Top => ()
    | _ => (value := Value.Top
            ; List.foreach (!lessThan, makeTop)
            ; lessThan := []
            ; Handlers.run (!handlers)
            ; handlers := Handlers.empty)

fun 'a lowerBoundPoint {clone = cloneA, coerce = coerceA, equals = equalsA} =
   (fn (e' as T {lessThan, handlers, value}: 'a t, from) =>
    let
       val pointClone = Point.clone {clone = cloneA, equals = equalsA}
       val pointCoerce = Point.coerce {clone = cloneA, coerce = coerceA, equals = equalsA}
       val lowerBoundPoint = lowerBoundPoint {clone = cloneA, coerce = coerceA, equals = equalsA}
    in
       case !value of
          Value.Bottom =>
             let
                val p = pointClone from
             in
                if pointCoerce {from = from, to = p}
                   then (value := Value.Point p
                         ; List.foreach (!lessThan, fn e => lowerBoundPoint (e, p))
                         ; Handlers.run (!handlers))
                   else makeTop e'
             end
        | Value.Point p =>
             if pointCoerce {from = from, to = p}
                then ()
                else makeTop e'
        | Value.Top => ()
    end)

fun 'a setPoint {clone = cloneA, coerce = coerceA, equals = equalsA} =
   (fn (T {lessThan, handlers, value}: 'a t, p: 'a Point.t) =>
    let
       val lowerBoundPoint = lowerBoundPoint {clone = cloneA, coerce = coerceA, equals = equalsA}
    in
       case !value of
          Value.Bottom =>
             (value := Value.Point p
              ; List.foreach (!lessThan, fn e => lowerBoundPoint (e, p))
              ; Handlers.run (!handlers))
        | _ => Error.bug "FlatLatticeRec.setPoint"
    end)

fun 'a coerce {clone = cloneA, coerce = coerceA, equals = equalsA} =
   (fn {from = from as T {lessThan, value, ...}: 'a t, to: 'a t} =>
    if equals (from, to)
       then ()
       else let
               val lowerBoundPoint = lowerBoundPoint {clone = cloneA, coerce = coerceA, equals = equalsA}
               fun pushLessThan () = List.push (lessThan, to)
            in
               case !value of
                  Value.Bottom => pushLessThan ()
                | Value.Point from => (pushLessThan (); lowerBoundPoint (to, from))
                | Value.Top => makeTop to
            end)

fun lowerBound {clone = cloneA, coerce = coerceA, equals = equalsA} =
   (fn (e', v) =>
    let
       val lowerBoundPoint = lowerBoundPoint {clone = cloneA, coerce = coerceA, equals = equalsA}
    in
       case v of
          Value.Bottom => ()
        | Value.Point p => lowerBoundPoint (e', p)
        | Value.Top => makeTop e'
    end)

fun unify {clone = cloneA, coerce = coerceA, equals = equalsA, unify = unifyA} =
   (fn (e1 as T {lessThan = lessThan1, ...},
        e2 as T {lessThan = lessThan2, ...}) =>
    if equals (e1, e2)
       then ()
    else if true
       then let
               val pointUnify = Point.unify {equals = equalsA, unify = unifyA}
               val setPoint = setPoint {clone = cloneA, coerce = coerceA, equals = equalsA}
               fun pushLessThan () =
                  (List.push (lessThan1, e2); List.push (lessThan2, e1))
            in
               case (value e1, value e2) of
                  (Value.Top, Value.Top) => ()
                | (Value.Top, _) => makeTop e2
                | (_, Value.Top) => makeTop e1
                | (Value.Point p1, Value.Point p2) =>
                     if pointUnify (p1, p2)
                        then pushLessThan ()
                        else (makeTop e1; makeTop e2)
                | (Value.Point p1, Value.Bottom) =>
                     (pushLessThan ()
                      ; setPoint (e2, p1))
                | (Value.Bottom, Value.Point p2) =>
                     (pushLessThan ()
                      ; setPoint (e1, p2))
                 | (Value.Bottom, Value.Bottom) =>
                     pushLessThan ()
            end
       else let
               val coerce = coerce {clone = cloneA, coerce = coerceA, equals = equalsA}
            in
               coerce {from = e1, to = e2}
               ; coerce {from = e2, to = e1}
            end)
end

functor FlatLatticePoly (S: FLAT_LATTICE_POLY_STRUCTS) =
struct
   local
      structure L =
         FlatLatticeRec
         (struct
             open S
             structure Point =
                struct
                   open Point
                   fun coerce {clone = _, coerce = _, equals = equalsA} =
                      fn {from, to} =>
                      equals equalsA (from, to)
                   fun unify {equals = equalsA, unify = _} =
                      fn (p1, p2) =>
                      equals equalsA (p1, p2)
                end
          end)
      fun err (f, g) _ =
         Error.bug (concat ["FlatLatticePoly.", f, ": ", g])
   in
      open L
      structure Point = S.Point
      val coerce = fn {equals = equalsA} =>
         coerce {clone = err ("coerce", "clone"),
                 coerce = err ("coerce", "coerce"),
                 equals = equalsA}
      val lowerBound = fn {equals = equalsA} =>
         lowerBound {clone = err ("lowerBound", "clone"),
                     coerce = err ("lowerBound", "coerce"),
                     equals = equalsA}
      val lowerBoundPoint = fn {equals = equalsA} =>
         lowerBoundPoint {clone = err ("lowerBound", "clone"),
                          coerce = err ("lowerBound", "coerce"),
                          equals = equalsA}
      val unify = fn {equals = equalsA} =>
         unify {clone = err ("unify", "clone"),
                coerce = err ("unify", "coerce"),
                equals = equalsA,
                unify = err ("unify", "unify")}
   end
end

functor FlatLatticeParam (S: FLAT_LATTICE_PARAM_STRUCTS) =
struct
   local
      structure L =
         FlatLatticePoly
         (struct
             open S
             structure Point =
                struct
                   open Point
                   val clone = fn _ => fn p => p
                   val equals = fn _ => equals
                end
          end)
      fun err (f, g) _ =
         Error.bug (concat ["FlatLatticeParam.", f, ": ", g])
   in
      open L
      structure Point = S.Point
      val coerce = fn args =>
         coerce {equals = err ("coerce", "equals")} args
      val isPointEq = fn args => isPointEq (err ("isPointEq", "equals")) args
      val lowerBound = fn args =>
         lowerBound {equals = err ("lowerBound", "equals")} args
      val lowerBoundPoint = fn args =>
         lowerBoundPoint {equals = err ("lowerBound", "equals")} args
      val unify = fn args =>
         unify {equals = err ("unify", "equals")} args
      val op<= = fn (from, to) => coerce {from = from, to = to}
      val op== = unify
   end
end

functor FlatLatticeMono (S: FLAT_LATTICE_MONO_STRUCTS) =
struct
   local
      structure L =
         FlatLatticeParam
         (struct
             open S
             structure Point =
                struct
                   open Point
                   type 'a t = t
                   val layout = fn _ => layout
                end
          end)
      structure Void =
         struct
            datatype t = Void of t
            val layout: string -> t -> Layout.t = fn f => fn _ =>
               Error.bug (concat ["FlatLatticeMono.", f, ": Void.layout"])
         end
   in
      open L
      type t = Void.t t
      structure Point = S.Point
      structure Value =
         struct
            open Value
            val layout = layout (Void.layout "Value.layout")
         end
      val layout = layout (Void.layout "layout")
   end
end
