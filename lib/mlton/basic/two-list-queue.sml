structure TwoListQueue:> QUEUE =
struct

datatype 'a t = T of 'a list * 'a list

fun foldAnyOrder (T (l, r), ac, f) =
   List.fold (r, List.fold (l, ac, f), f)

fun foldr (T (l, r), ac, f) =
   List.foldr (l, List.fold (r, ac, f), f)

fun toList q = foldr (q, [], op ::)

fun deque (T (l, r)) =
   let val (l, r) = (case l of
			[] => (rev r, [])
		      | _ =>  (l, r))
   in case l of
      [] => NONE
    | x :: l => SOME (x, T (l, r))
   end

fun empty () = T ([], [])

val isEmpty =
   fn T ([], []) => true
    | _ => false

fun enque (T (l, r), x) = T (l, x :: r)

end

structure Queue = TwoListQueue
