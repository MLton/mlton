(* Copyright (C) 2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Runtime (S: RUNTIME_STRUCTS): RUNTIME =
struct

open S

structure Type = Mtype ()

structure CFunction = CFunction (structure Type = Type)

structure GCField =
   struct
      datatype t =
	 CanHandle
       | CardMap
       | CurrentThread
       | Frontier
       | Limit
       | LimitPlusSlop
       | MaxFrameSize
       | ProfileAllocIndex
       | SignalIsPending
       | StackBottom
       | StackLimit
       | StackTop

      val ty =
	 fn CanHandle => Type.int
	  | CardMap => Type.pointer
	  | CurrentThread => Type.pointer
	  | Frontier => Type.pointer
	  | Limit => Type.pointer
	  | LimitPlusSlop => Type.pointer
	  | MaxFrameSize => Type.word
	  | ProfileAllocIndex => Type.word
	  | SignalIsPending => Type.int
	  | StackBottom => Type.pointer
	  | StackLimit => Type.pointer
	  | StackTop => Type.pointer

      val canHandleOffset: int ref = ref 0
      val cardMapOffset: int ref = ref 0
      val currentThreadOffset: int ref = ref 0
      val frontierOffset: int ref = ref 0
      val limitOffset: int ref = ref 0
      val limitPlusSlopOffset: int ref = ref 0
      val maxFrameSizeOffset: int ref = ref 0
      val profileAllocIndexOffset: int ref = ref 0
      val signalIsPendingOffset: int ref = ref 0
      val stackBottomOffset: int ref = ref 0
      val stackLimitOffset: int ref = ref 0
      val stackTopOffset: int ref = ref 0

      fun setOffsets {canHandle, cardMap, currentThread, frontier, limit,
		      limitPlusSlop, maxFrameSize, profileAllocIndex,
		      signalIsPending, stackBottom, stackLimit, stackTop} =
	 (canHandleOffset := canHandle
	  ; cardMapOffset := cardMap
	  ; currentThreadOffset := currentThread
	  ; frontierOffset := frontier
	  ; limitOffset := limit
	  ; limitPlusSlopOffset := limitPlusSlop
	  ; maxFrameSizeOffset := maxFrameSize
	  ; profileAllocIndexOffset := profileAllocIndex
	  ; signalIsPendingOffset := signalIsPending
	  ; stackBottomOffset := stackBottom
	  ; stackLimitOffset := stackLimit
	  ; stackTopOffset := stackTop)

      val offset =
	 fn CanHandle => !canHandleOffset
	  | CardMap => !cardMapOffset
	  | CurrentThread => !currentThreadOffset
	  | Frontier => !frontierOffset
	  | Limit => !limitOffset
	  | LimitPlusSlop => !limitPlusSlopOffset
	  | MaxFrameSize => !maxFrameSizeOffset
	  | ProfileAllocIndex => !profileAllocIndexOffset
	  | SignalIsPending => !signalIsPendingOffset
	  | StackBottom => !stackBottomOffset
	  | StackLimit => !stackLimitOffset
	  | StackTop => !stackTopOffset

      val toString =
	 fn CanHandle => "CanHandle"
	  | CardMap => "CardMap"
	  | CurrentThread => "CurrentThread"
	  | Frontier => "Frontier"
	  | Limit => "Limit"
	  | LimitPlusSlop => "LimitPlusSlop"
	  | MaxFrameSize => "MaxFrameSize"
	  | ProfileAllocIndex => "ProfileAllocIndex"
	  | SignalIsPending => "SignalIsPending"
	  | StackBottom => "StackBottom"
	  | StackLimit => "StackLimit"
	  | StackTop => "StackTop"

      val layout = Layout.str o toString
   end

structure ObjectType =
   struct
      datatype t =
	 Array of {numBytesNonPointers: int,
		   numPointers: int}
       | Normal of {numPointers: int,
		    numWordsNonPointers: int}
       | Stack

      val equals: t * t -> bool = op =

      fun layout (t: t): Layout.t =
	 let
	    open Layout
	 in
	    case t of
	       Array {numBytesNonPointers = nbnp, numPointers = np} =>
		  seq [str "Array ",
		       record [("numBytesNonPointers", Int.layout nbnp),
			       ("numPointers", Int.layout np)]]
	     | Normal {numPointers = np, numWordsNonPointers = nwnp} =>
		  seq [str "Normal ",
		       record [("numPointers", Int.layout np),
			       ("numWordsNonPointers", Int.layout nwnp)]]
	     | Stack => str "Stack"
	 end
   end

val maxTypeIndex = Int.pow (2, 19)
   
fun typeIndexToHeader typeIndex =
   (Assert.assert ("Runtime.header", fn () =>
		   0 <= typeIndex
		   andalso typeIndex < maxTypeIndex)
    ; Word.orb (0w1, Word.<< (Word.fromInt typeIndex, 0w1)))

fun headerToTypeIndex w = Word.toInt (Word.>> (w, 0w1))

val wordSize: int = 4
val arrayHeaderSize = 3 * wordSize
val intInfOverheadSize = arrayHeaderSize + wordSize (* for the sign *)
val labelSize = wordSize
val limitSlop: int = 512
val normalHeaderSize = wordSize
val pointerSize = wordSize
val array0Size = arrayHeaderSize + wordSize (* for the forwarding pointer *)

val arrayLengthOffset = ~ (2 * wordSize)
val allocTooLarge: word = 0wxFFFFFFFC

val headerOffset = ~wordSize

fun normalSize {numPointers, numWordsNonPointers} =
   wordSize * (numPointers + numWordsNonPointers)

fun wordAlignWord (w: word): word =
   let
      open Word
   in
      andb (MLton.Word.addCheck (w, 0w3), notb 0w3)
   end

fun wordAlignInt (i: int): int =
   Word.toInt (wordAlignWord (Word.fromInt i))
   
fun isWordAligned (n: int): bool =
   0 = Int.rem (n, wordSize)
   
fun isValidObjectSize (n: int): bool =
   n > 0 andalso isWordAligned n

val maxFrameSize = Int.pow (2, 16)

end
