(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

structure Relation: RELATION =
struct

open Relation0

val layout = Layout.str o toString

fun lexico (t, f) =
   case t of
      EQUAL => f ()
    | r => r

end
