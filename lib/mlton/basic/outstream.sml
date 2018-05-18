(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Outstream: OUTSTREAM =
struct

open Outstream0

fun layout _ = Layout.str "<outstream>"

end

structure Out = Outstream
