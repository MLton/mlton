(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
(*------------------------------------------------------------------*)
(*                           BoundedOrder                           *)
(*------------------------------------------------------------------*)

functor BoundedOrder(O: ORDER): BOUNDED_ORDER =
struct

structure O = O
structure R = Relation

datatype t =
   Min
 | Max
 | Inject of O.t

val smallest = Min
val largest = Max
val inject = Inject

val project =
   fn Inject x => x
    | _ => Error.bug "BoundedOrder.project"

val compare =
   fn (Min, Min) => R.EQUAL
    | (Min, _) => R.LESS
    | (Max, Max) => R.EQUAL
    | (Max, _) => R.GREATER
    | (Inject _, Min) => R.GREATER
    | (Inject _, Max) => R.LESS
    | (Inject x, Inject y) => O.compare(x, y)

val {equals, <, <=, >, >=, min, max} = R.compare compare

local open Layout
in fun layout x =
   case x of
      Min => str "Min"
    | Max => str "Max"
    | Inject x => O.layout x
end

end
