(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Error: ERROR =
struct

fun bug msg = raise (Fail msg)

fun reraise (exn, msg) =
   bug (concat [msg, "::",
                case exn of
                   Fail msg => msg
                 | _ => "?"])

fun unimplemented msg = raise Fail (concat ["unimplemented: ", msg])

fun warning msg = TextIO.output (TextIO.stdErr, concat [msg, "\n"])

end
