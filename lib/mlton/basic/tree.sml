structure Tree: TREE =
struct

datatype 'a t = T of 'a * 'a t list

fun layout layout' (T (x, ts))
  = let open Layout
    in seq [str "(",
	    layout' x,
	    str ", ",
	    List.layout (layout layout') ts,
	    str ")"]
    end
end
