(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure FileDesc: FILE_DESC =
   struct
      open Posix.IO Posix.FileSys

      type t = file_desc

      val layout = Word.layout o fdToWord
	 
      fun move {from, to} =
	 if from <> to
	    then (dup2 {old = from, new = to}
		  ; close from)
	 else ()

      fun fluidLet (d1, d2, f) =
	 let
	    val copy = dup d1
	    val _ = dup2 {old = d2, new = d1}
	 in
	    DynamicWind.wind (f, fn () => move {from = copy, to = d1})
	 end
   end
   
