(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
(*-------------------------------------------------------------------*)
(*                               Type                                *)
(*-------------------------------------------------------------------*)

functor Type (): TYPE =
struct

datatype set =
    EmptySet
  | Set of elt
and elt = 
    Base
  | Pair of elt * elt
  | EltSet of set

exception Incompatible

fun combineSet(EmptySet, EmptySet) = EmptySet
  | combineSet(EmptySet, Set t) = Set t
  | combineSet(Set t, EmptySet) = Set t
  | combineSet(Set t, Set t') = Set(combineElt(t, t'))
and combineElt(Base, Base) = Base
  | combineElt(Pair(t, t1), Pair(t', t1')) =
    Pair(combineElt(t, t'), combineElt(t1, t1'))
  | combineElt(EltSet t, EltSet t') = EltSet(combineSet(t, t'))
  | combineElt _ = raise Incompatible

fun combineToCompat combine a =
    (combine a ; true) handle Incompatible => false

structure Set =
    struct
        type t = set
        val combine = combineSet
        val areCompatible = combineToCompat combine
    end

structure Elt =
    struct
        type t = elt
        val combine = combineElt
        val areCompatible = combineToCompat combine
    end

fun combineSetElt(EmptySet, t) = Set t
  | combineSetElt(Set t, t') = Set(Elt.combine(t, t'))

val areCompatibleSetElt = combineToCompat combineSetElt

end

structure Type = Type()
