functor MonoList(X: T): T =
struct

type t = X.t list

fun equals(l, l') = List.equals(l, l', X.equals)

val layout = List.layout X.layout
			     
end
