structure SourcePos: SOURCE_POS =
struct

datatype t = T of {column: int,
		   file: File.t,
		   line: int}

local
   fun f g (T r) = g r
in
   val file = f #file
end

val bogus = T {column = ~1,
	       file = "<bogus>",
	       line = ~1}

fun toString (T {column, file, line}) =
   concat [file, ":", Int.toString line, ".", Int.toString column]

end
