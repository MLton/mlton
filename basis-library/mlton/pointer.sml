structure MLtonPointer: MLTON_POINTER =
struct

open Primitive.Pointer

fun add (p, t) = fromWord (Word.+ (toWord p, t))
fun diff (p, p') = Word.- (toWord p, toWord p')
fun sub (p, t) = fromWord (Word.- (toWord p, t))
   
end
