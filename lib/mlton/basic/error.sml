(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Error: ERROR =
struct

fun bug msg = raise (Fail msg)

fun reraise (exn, {prefix, suffix}) =
   bug (concat [prefix,
                case exn of
                   Fail msg => msg
                 | _ => General.exnName exn,
                suffix])

fun reraisePrefix (exn, msg) = reraise (exn, {prefix = msg, suffix = ""})
fun reraiseSuffix (exn, msg) = reraise (exn, {prefix = "", suffix = msg})

fun unimplemented msg = raise Fail (concat ["unimplemented: ", msg])

fun warning msg = TextIO.output (TextIO.stdErr, concat [msg, "\n"])

end
