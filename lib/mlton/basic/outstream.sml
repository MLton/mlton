structure Outstream: OUTSTREAM =
struct

open Outstream0

fun layout _ = Layout.str "<outstream>"

end

structure Out = Outstream
