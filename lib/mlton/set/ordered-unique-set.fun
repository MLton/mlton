functor OrderedUniqueSet(S: ORDERED_UNIQUE_SET_STRUCTS): ORDERED_UNIQUE_SET = 
struct

open S

type int = Int.t

datatype t = T of {elements: Element.t list,
		   length: int}

fun fromList(xs: Element.t list): t =
   T{length = List.length xs,
     elements = List.insertionSort(xs, Element.<)}
   
fun equals(T{elements = xs, length = n}, T{elements = xs', length = n'}) =
   n = n' andalso List.equals(xs, xs', Element.equals)
   
end
