(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
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
   val column = f #column
   val line = f #line
end

fun compare (T {column = c, file = f, line = l},
	     T {column = c', file = f', line = l'}) =
   case String.compare (f, f') of
      EQUAL =>
	 (case Int.compare (l, l') of
	     EQUAL => Int.compare (c, c')
	   | r => r)
    | r => r

fun equals (T r, T r') = r = r'

fun make {column, file, line} =
   T {column = column,
      file = file,
      line = line}

val basisString = "/basis/"

fun getBasis (T {file, ...}) =
   String.findSubstring {string = file, substring = basisString}

fun isBasis p = isSome (getBasis p)

fun file (p as T {file, ...}) =
   case getBasis p of
      NONE => file
    | SOME i =>
	 concat ["<basis>/",
		 String.dropPrefix (file, i + String.size basisString)]

val bogus = T {column = ~1,
	       file = "<bogus>",
	       line = ~1}

fun toString (p as T {column, line, ...}) =
   concat [file p, " ", Int.toString line, ".", Int.toString column]

fun posToString (T {line, column, ...}) =
   concat [Int.toString line, ".", Int.toString column]

end
