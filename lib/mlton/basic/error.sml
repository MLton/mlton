(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Error: ERROR =
struct
   
fun unimplemented msg = raise Fail(concat["unimplemented: ", msg])

fun bug msg = raise(Fail msg)

fun warning msg = TextIO.output(TextIO.stdErr, msg^"\n")

end
