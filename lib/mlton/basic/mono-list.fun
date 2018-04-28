(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MonoList(X: T): T =
struct

type t = X.t list

fun equals(l, l') = List.equals(l, l', X.equals)

val layout = List.layout X.layout

end
