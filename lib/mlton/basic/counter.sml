(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Counter: COUNTER =
struct

type int = Int.t
   
datatype t = T of int ref

fun new n = T(ref n)

fun reset(T r, n) = r := n

fun tick(T r) = Int.inc r

fun value(T r) = !r

fun next c = value c before tick c

val equals = fn (T r, T r') => r = r'
   
end
