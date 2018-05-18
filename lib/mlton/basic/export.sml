(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
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
