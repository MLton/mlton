(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor UnorderedSet (Element: T):> SET where type Element.t = Element.t =
struct

structure Element = Element

open List

type t = Element.t List.t

val {empty, singleton, size, equals, <=, >=, <, >, +, -, intersect, unions,
     add, remove, contains, areDisjoint, subset, subsetSize,
     map, replace, layout} =
   List.set{equals = Element.equals,
            layout = Element.layout}

val partition = List.partition
val power = List.power
val subsets = List.subsets

val fromList = fn l => List.fold(l, empty, fn (x, s) => add(s, x))
val toList = fn x => x

val union = op +

end
