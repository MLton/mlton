functor MachineCases (S: MACHINE_CASES_STRUCTS): MACHINE_CASES = 
struct

open S

datatype t =
   Char of (char * Label.t) list
 | Int of (int * Label.t) list
 | Word of (word * Label.t) list

fun fold (c, a, f) =
   let
      fun doit cs = List.fold (cs, a, fn ((_, l), a) => f (l, a))
   in case c of
      Char cs => doit cs
    | Int cs => doit cs
    | Word cs => doit cs
   end

fun foreach (c, f) = fold (c, (), fn (l, ()) => f l)

fun layout c =
   let
      fun doit (l, f) = List.layout (Layout.tuple2 (f, Label.layout)) l
   in
      case c of
	 Char l => doit (l, Char.layout)
       | Int l => doit (l, Int.layout)
       | Word l => doit (l, Word.layout)
   end

end
