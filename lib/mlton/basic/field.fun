(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Field(F: FIELD_STRUCTS): FIELD =
struct

structure U = Ring(F)

open F U

val op / = fn (x, y) => x * inverse y
    
end
