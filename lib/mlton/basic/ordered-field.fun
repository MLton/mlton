(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
(*-------------------------------------------------------------------*)
(*                           OrderedField                            *)
(*-------------------------------------------------------------------*)

functor OrderedField(F: ORDERED_FIELD_STRUCTS): ORDERED_FIELD =
struct

structure U = Field(F)
structure U' = OrderedRing(F)
open F U U'

end
