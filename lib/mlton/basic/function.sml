(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

structure Function: FUNCTION =
struct

fun curry f x y = f(x, y)

fun uncurry f (x, y) = f x y
   
fun compose(f, g) x = f(g(x))

fun seq(f, g) x = g(f(x))

fun seq3(f, g, h) x = h(g(f(x)))

fun layout _ = Layout.str "<function>"

fun output(_, out) = Out.output(out, "<function>")
   
end
