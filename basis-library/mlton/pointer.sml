structure MLtonPointer: MLTON_POINTER =
struct

open Primitive.Pointer

val add = Word.+
val diff = Word.-
val sub = Word.-
   
end
