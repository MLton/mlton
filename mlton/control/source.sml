(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Source: SOURCE =
struct
  
datatype t = T of {file: File.t ref,
		   lineNum: int ref,
		   lineStart: int ref}

local
   fun make f (T r) = ! (f r)
in
   val file = make #file
end

fun getPos (T {file, lineNum, lineStart, ...}, n) =
   SourcePos.make {column = n - !lineStart,
		   file = !file,
		   line = !lineNum}
		
fun lineStart (s as T {lineStart, ...}) = getPos (s, !lineStart)

fun lineDirective (T {file, lineNum, lineStart},
		   f,
		   {lineNum = n, lineStart = s}) =
   (Option.app (f, fn f => file := f)
    ; lineNum := n
    ; lineStart := s)
		      
fun new file = T {file = ref file,
		  lineNum = ref 1,
		  lineStart = ref 1}

fun newline (T {lineStart, lineNum, ...}, n) =
   (Int.inc lineNum
    ; lineStart := n)
   
end

