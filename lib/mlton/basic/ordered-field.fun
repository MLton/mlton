(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
