(*-------------------------------------------------------------------*)
(*                           OrderedField                            *)
(*-------------------------------------------------------------------*)

functor OrderedField(F: ORDERED_FIELD_STRUCTS): ORDERED_FIELD =
struct

structure U = Field(F)
structure U' = OrderedRing(F)
open F U U'

end
