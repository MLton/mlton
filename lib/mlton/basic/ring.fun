(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Ring(S: RING_STRUCTS):> RING where type t = S.t = 
struct

open S

fun a - b = a + (~ b)

fun isZero a = equals(a, zero)

fun double n = n + n

fun square n = n * n

fun sum l = List.fold(l, zero, op +)
   
end
