structure Runtime: RUNTIME =
struct

val wordSize: int = 4
val arrayHeaderSize = 2 * wordSize
val labelSize = wordSize
val limitSlop: int = 512
val objectHeaderSize = wordSize
val pointerSize = wordSize
val array0Size = arrayHeaderSize + wordSize (* for the forwarding pointer *)

(* These checks, and in particular pointerBits and nonPointerBits
 * must agree with runtime/gc.h.
 *)

val arrayTag: word =  0wx00000000
val normalTag: word = 0wx80000000
val pointerBits: int = 14
val nonPointerBits: int = 14
val nonPointersShift: int = pointerBits

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

fun objectHeader (z as {numPointers, numWordsNonPointers})  =
   let
      val _ = Assert.assert ("objectHeader", fn () => isValidObjectHeader z)
      open Word
   in
      orb (orb (normalTag, fromInt numPointers),
	   << (fromInt numWordsNonPointers, fromInt nonPointersShift))
   end

fun objectSize {numPointers, numWordsNonPointers} =
   wordSize * (numPointers + numWordsNonPointers)

local
   val f = make (pointerBits, nonPointerBits - 1)
in
   fun isValidArrayHeader {numBytesNonPointers, numPointers} =
      numBytesNonPointers >= 0
      andalso numPointers >= 0
      andalso (if numBytesNonPointers = 0
		  then numPointers <= 1
	       else numPointers = 0)
      andalso f (numPointers, numBytesNonPointers)
end

fun arrayHeader (z as {numBytesNonPointers, numPointers}) =
   let
      val _ = Assert.assert ("arrayHeader", fn () => isValidArrayHeader z)
      open Word
   in
      orb (orb (arrayTag, fromInt numPointers),
	   << (fromInt numBytesNonPointers, fromInt nonPointersShift))
   end

fun wordAlign (w: word): word =
   let
      open Word
   in
      andb (w + 0w3, notb 0w3)
   end
   
fun isWordAligned (n: int): bool =
   0 = Int.rem (n, wordSize)
   
fun isValidObjectSize (n: int): bool =
   n > 0 andalso isWordAligned n

val maxFrameSize = Int.^ (2, 16)

end
