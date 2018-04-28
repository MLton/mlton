(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Field(F: FIELD_STRUCTS): FIELD =
struct

structure U = Ring(F)

open F U

val op / = fn (x, y) => x * inverse y

end
