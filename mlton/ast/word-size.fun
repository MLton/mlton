functor WordSize (S: WORD_SIZE_STRUCTS): WORD_SIZE =
struct

datatype t = W8 | W16 | W32

val equals: t * t -> bool = op =

val all = [W8, W16, W32]

val default = W32

val max: t -> word =
   fn W8 => 0wxFF
    | W16 => 0wxFFFF
    | W32 => 0wxFFFFFFFF

val allOnes = max

val bytes: t -> int = 
   fn W8 => 1
    | W16 => 2
    | W32 => 4

fun size s = 8 * bytes s

fun toString w = Int.toString (size w)

val memoize: (t -> 'a) -> t -> 'a =
   fn f =>
   let
      val a8 = f W8
      val a16 = f W16
      val a32 = f W32
   in
      fn W8 => a8
       | W16 => a16
       | W32 => a32
   end
   
end
