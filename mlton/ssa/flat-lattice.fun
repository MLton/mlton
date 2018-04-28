(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
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
