(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Runtime: RUNTIME =
struct

structure GCField =
   struct
      datatype t =
	 Base
       | CanHandle
       | CurrentThread
       | FromSize
       | Frontier
       | Limit
       | LimitPlusSlop
       | MaxFrameSize
       | SignalIsPending
       | StackBottom
       | StackLimit
       | StackTop

      datatype ty = Int | Word

      val ty =
	 fn Base => Word
	  | CanHandle => Int
	  | CurrentThread => Word
	  | FromSize => Word
	  | Frontier => Word
	  | Limit => Word
	  | LimitPlusSlop => Word
	  | MaxFrameSize => Int
	  | SignalIsPending => Int
	  | StackBottom => Word
	  | StackLimit => Word
	  | StackTop => Word

      val baseOffset: int ref = ref 0
      val canHandleOffset: int ref = ref 0
      val currentThreadOffset: int ref = ref 0
      val fromSizeOffset: int ref = ref 0
      val frontierOffset: int ref = ref 0
      val limitOffset: int ref = ref 0
      val limitPlusSlopOffset: int ref = ref 0
      val maxFrameSizeOffset: int ref = ref 0
      val signalIsPendingOffset: int ref = ref 0
      val stackBottomOffset: int ref = ref 0
      val stackLimitOffset: int ref = ref 0
      val stackTopOffset: int ref = ref 0

      fun setOffsets {base, canHandle, currentThread, fromSize, frontier, limit,
		      limitPlusSlop, maxFrameSize, signalIsPending, stackBottom,
		      stackLimit, stackTop} =
	 (baseOffset := base
	  ; canHandleOffset := canHandle
	  ; currentThreadOffset := currentThread
	  ; fromSizeOffset := fromSize
	  ; frontierOffset := frontier
	  ; limitOffset := limit
	  ; limitPlusSlopOffset := limitPlusSlop
	  ; maxFrameSizeOffset := maxFrameSize
	  ; signalIsPendingOffset := signalIsPending
	  ; stackBottomOffset := stackBottom
	  ; stackLimitOffset := stackLimit
	  ; stackTopOffset := stackTop)

      val offset =
	 fn Base => !baseOffset
	  | CanHandle => !canHandleOffset
	  | CurrentThread => !currentThreadOffset
	  | FromSize => !fromSizeOffset
	  | Frontier => !frontierOffset
	  | Limit => !limitOffset
	  | LimitPlusSlop => !limitPlusSlopOffset
	  | MaxFrameSize => !maxFrameSizeOffset
	  | SignalIsPending => !signalIsPendingOffset
	  | StackBottom => !stackBottomOffset
	  | StackLimit => !stackLimitOffset
	  | StackTop => !stackTopOffset

      val toString =
	 fn Base => "Base"
	  | CanHandle => "CanHandle"
	  | CurrentThread => "CurrentThread"
	  | FromSize => "FromSize"
	  | Frontier => "Frontier"
	  | Limit => "Limit"
	  | LimitPlusSlop => "LimitPlusSlop"
	  | MaxFrameSize => "MaxFrameSize"
	  | SignalIsPending => "SignalIsPending"
	  | StackBottom => "StackBottom"
	  | StackLimit => "StackLimit"
	  | StackTop => "StackTop"

      val layout = Layout.str o toString
   end

val wordSize: int = 4
val objectHeaderSize = wordSize
val arrayHeaderSize = wordSize + objectHeaderSize
val array0Size = arrayHeaderSize + wordSize (* for the forwarding pointer *)
val limitSlop: int = 512
val labelSize = wordSize
val pointerSize = wordSize

val allocTooLarge: word = 0wxFFFFFFFC
   
(* These checks, and in particular pointerBits and nonPointerBits
 * must agree with runtime/gc.h.
 *)

val tagMask: word =        0wxC0000000
val nonPointerMask: word = 0wx0FFFC000
val pointerMask: word =    0wx00003FFF

val arrayTag: word =  0wx00000000
val stackTag: word =  0wxC0000000
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

fun splitHeader (header: word) =
   let
      open Word
   in 
     {tag = andb (header, tagMask),
      numNonPointers = Word.toInt (>> (andb (header, nonPointerMask), 
				       Word.fromInt pointerBits)),
      numPointers = Word.toInt (andb (header, pointerMask))}
   end
fun splitObjectHeader (header: word) =
   let
      val {tag, numNonPointers, numPointers} = splitHeader header
      val _ = Assert.assert ("splitObjectHeader", fn () => tag = normalTag)
   in
      {numWordsNonPointers = numNonPointers,
       numPointers = numPointers}
   end
fun splitArrayHeader (header: word) =
   let
      val {tag, numNonPointers, numPointers} = splitHeader header
      val _ = Assert.assert ("splitArrayHeader", fn () => tag = arrayTag)
   in
      {numBytesNonPointers = numNonPointers,
       numPointers = numPointers}
   end

fun wordAlign (w: word): word =
   let
      open Word
   in
      andb (MLton.Word.addCheck (w, 0w3), notb 0w3)
   end
   
fun isWordAligned (n: int): bool =
   0 = Int.rem (n, wordSize)
   
fun isValidObjectSize (n: int): bool =
   n > 0 andalso isWordAligned n

val maxFrameSize = Int.^ (2, 16)

end
