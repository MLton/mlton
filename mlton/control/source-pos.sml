(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
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
