functor RealSize (S: REAL_SIZE_STRUCTS): REAL_SIZE = 
struct

open S

datatype t = R32 | R64

val all = [R32, R64]

val default = R64

val equals: t * t -> bool = op =

val memoize: (t -> 'a) -> t -> 'a =
   fn f =>
   let
      val r32 = f R32
      val r64 = f R64
   in
      fn R32 => r32
       | R64 => r64
   end

val toString =
   fn R32 => "32"
    | R64 => "64"

val layout = Layout.str o toString

val bytes: t -> int =
   fn R32 => 4
    | R64 => 8

val bits: t -> int =
   fn R32 => 32
    | R64 => 64

end
