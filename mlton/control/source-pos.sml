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
		   isBasis: bool,
		   line: int}

local
   fun f g (T r) = g r
in
   val column = f #column
   val file = f #file
   val isBasis = f #isBasis
   val line = f #line
end

fun equals (T r, T r') = r = r'

fun make {column, file, line} =
   let
      val s = "/basis-library/"
      val (file, isBasis) = 
	 case String.findSubstring {string = file, substring = s} of
	    NONE => (file, false)
	  | SOME i =>
	       (concat ["<basis>/",
			String.dropPrefix (file, i + String.size s)],
		true)
   in
      T {column = column,
	 file = file,
	 isBasis = isBasis,
	 line = line}
   end

val bogus = T {column = ~1,
	       file = "<bogus>",
	       isBasis = false,
	       line = ~1}

fun toString (T {column, file, line, ...}) =
   concat [file, ":", Int.toString line, ".", Int.toString column]

fun posToString (T {line, column, ...}) =
   concat [Int.toString line, ".", Int.toString column]

end
