(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Unit: UNIT =
struct

type t = unit

val equals = fn ((), ()) => true

fun layout() = Layout.str"()"

end
