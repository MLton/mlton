(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor MonoList(X: T): T =
struct

type t = X.t list

fun equals(l, l') = List.equals(l, l', X.equals)

val layout = List.layout X.layout
			     
end
