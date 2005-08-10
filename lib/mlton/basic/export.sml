(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

structure Export: EXPORT =
struct
   
structure NJ = SMLofNJ

fun exportFn(file, command) =
   NJ.exportFn(File.toString file,
	       fn arg => ((command arg)
			  handle exn => (print ("Unhandled exception: "
						^ exnName exn ^ "\n") ;
					 raise exn)))
   
end
