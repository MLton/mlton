structure Runtime: RUNTIME =
struct

val wordSize: int = 4
val arrayHeaderSize = 2 * wordSize
val labelSize = wordSize
val objectHeaderSize = wordSize
val pointerSize = wordSize

(* These checks, and in particular pointerBits and nonPointerBits
 * must agree with runtime/gc.h.
 *)

val pointerBits: int = 15
val nonPointerBits: int = 15

fun make (p', np') =
   let
      val p' = Int.^ (2, p')
      val np' = Int.^ (2, np')
   in
      fn (p, np) => p < p' andalso np < np'
   end

local
   val f = make (pointerBits, nonPointerBits)
in
   fun isValidObjectHeader {numPointers, numWordsNonPointers} =
      f (numPointers, numWordsNonPointers)
end

fun objectSize {numPointers, numWordsNonPointers} =
   wordSize * (numPointers + numWordsNonPointers)

local
   val f = make (pointerBits, nonPointerBits - 1)
in
   fun isValidArrayHeader {numBytesNonPointers, numPointers} =
      f (numPointers, numBytesNonPointers)
end
   
fun isWordAligned (n: int): bool =
   0 = Int.rem (n, wordSize)
   
fun isValidObjectSize (n: int): bool =
   n > 0 andalso isWordAligned n
   
end
