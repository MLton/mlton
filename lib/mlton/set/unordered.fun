functor UnorderedSet (Element: T): SET =
struct

structure Element = Element
   
open List

type t = Element.t List.t
    
val {empty, singleton, size, equals, <=, >=, <, >, +, -, intersect, unions,
     add, remove, contains, areDisjoint, subset, subsetSize,
     map, replace, layout} =
   List.set{equals = Element.equals,
	    layout = Element.layout}

val power = List.power
val subsets = List.subsets
   
val fromList = fn l => List.fold(l, empty, fn (x, s) => add(s, x))
val toList = fn x => x

val union = op +
   
end
