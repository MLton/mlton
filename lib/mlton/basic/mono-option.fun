functor MonoOption (X: T): T =
struct

type t = X.t option

fun equals (o1, o2) = Option.equals (o1, o2, X.equals)

val layout = Option.layout X.layout
   
end
