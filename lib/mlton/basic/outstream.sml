(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Outstream: OUTSTREAM =
struct

open Outstream0

fun layout _ = Layout.str "<outstream>"

end

structure Out = Outstream
