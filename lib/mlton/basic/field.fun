functor Field(F: FIELD_STRUCTS): FIELD =
struct

structure U = Ring(F)

open F U

val op / = fn (x, y) => x * inverse y
    
end
