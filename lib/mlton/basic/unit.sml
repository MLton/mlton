structure Unit: UNIT =
struct

type t = unit

val equals = fn ((), ()) => true

fun layout() = Layout.str"()"

end
