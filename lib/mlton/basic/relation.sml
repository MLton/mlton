(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
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
