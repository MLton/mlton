(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Sum(S: SUM_STRUCTS): SUM =
struct

open S

datatype t =
   X of X.t
 | Y of Y.t

val outX =
   fn X x => x
    | _ => Error.bug "outX"

val outY =
   fn Y y => y
    | _ => Error.bug "outY"

val map =
   fn (X x, f, _) => f x
    | (Y y, _, f) => f y

val equals =
   fn (X l, X l') => X.equals(l, l')
    | (Y l, Y l') => Y.equals(l, l')
    | _ => false

fun layout s = map(s, X.layout, Y.layout)

end
