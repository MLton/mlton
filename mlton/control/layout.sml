structure Code =
   struct
      fun nest (prefix, x, y) =
   align [seq [str prefix, x],
	      seq [str "in ", y],
	      str "end"]

fun layoutLet (d, e) = nest ("let ", d, e)
fun layoutLocal (d, d') = nest ("local ", d, d')

