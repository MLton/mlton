(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Runtime (S: RUNTIME_STRUCTS): RUNTIME =
struct

open S

structure GCField =
   struct
      datatype t =
         CanHandle
       | CardMap
       | CurrentThread
       | CurSourceSeqsIndex
       | ExnStack
       | Frontier
       | Limit
       | LimitPlusSlop
       | MaxFrameSize
       | SignalIsPending
       | StackBottom
       | StackLimit
       | StackTop

      val equals: t * t -> bool = op =
         
(*       val ty =
 *       fn CanHandle => CType.defaultInt
 *        | CardMap => CType.pointer
 *        | CurrentThread => CType.pointer
 *        | ExnStack => CType.defaultWord
 *        | Frontier => CType.pointer
 *        | Limit => CType.pointer
 *        | LimitPlusSlop => CType.pointer
 *        | MaxFrameSize => CType.defaultWord
 *        | SignalIsPending => CType.defaultInt
 *        | StackBottom => CType.pointer
 *        | StackLimit => CType.pointer
 *        | StackTop => CType.pointer
 *)

      val canHandleOffset: Bytes.t ref = ref Bytes.zero
      val cardMapOffset: Bytes.t ref = ref Bytes.zero
      val currentThreadOffset: Bytes.t ref = ref Bytes.zero
      val curSourceSeqsIndexOffset: Bytes.t ref = ref Bytes.zero
      val exnStackOffset: Bytes.t ref = ref Bytes.zero
      val frontierOffset: Bytes.t ref = ref Bytes.zero
      val limitOffset: Bytes.t ref = ref Bytes.zero
      val limitPlusSlopOffset: Bytes.t ref = ref Bytes.zero
      val maxFrameSizeOffset: Bytes.t ref = ref Bytes.zero
      val signalIsPendingOffset: Bytes.t ref = ref Bytes.zero
      val stackBottomOffset: Bytes.t ref = ref Bytes.zero
      val stackLimitOffset: Bytes.t ref = ref Bytes.zero
      val stackTopOffset: Bytes.t ref = ref Bytes.zero

      fun setOffsets {canHandle, cardMap, currentThread, curSourceSeqsIndex, 
                      exnStack, frontier, limit, limitPlusSlop, maxFrameSize, 
                      signalIsPending, stackBottom, stackLimit, stackTop} =
         (canHandleOffset := canHandle
          ; cardMapOffset := cardMap
          ; currentThreadOffset := currentThread
          ; curSourceSeqsIndexOffset := curSourceSeqsIndex
          ; exnStackOffset := exnStack
          ; frontierOffset := frontier
          ; limitOffset := limit
          ; limitPlusSlopOffset := limitPlusSlop
          ; maxFrameSizeOffset := maxFrameSize
          ; signalIsPendingOffset := signalIsPending
          ; stackBottomOffset := stackBottom
          ; stackLimitOffset := stackLimit
          ; stackTopOffset := stackTop)

      val offset =
         fn CanHandle => !canHandleOffset
          | CardMap => !cardMapOffset
          | CurrentThread => !currentThreadOffset
          | CurSourceSeqsIndex => !curSourceSeqsIndexOffset
          | ExnStack => !exnStackOffset
          | Frontier => !frontierOffset
          | Limit => !limitOffset
          | LimitPlusSlop => !limitPlusSlopOffset
          | MaxFrameSize => !maxFrameSizeOffset
          | SignalIsPending => !signalIsPendingOffset
          | StackBottom => !stackBottomOffset
          | StackLimit => !stackLimitOffset
          | StackTop => !stackTopOffset

      val toString =
         fn CanHandle => "CanHandle"
          | CardMap => "CardMap"
          | CurrentThread => "CurrentThread"
          | CurSourceSeqsIndex => "CurSourceSeqsIndex"
          | ExnStack => "ExnStack"
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

structure RObjectType =
   struct
      datatype t =
         Array of {hasIdentity: bool,
                   nonPointer: Bytes.t,
                   pointers: int}
       | Normal of {hasIdentity: bool,
                    nonPointer: Words.t,
                    pointers: int}
       | Stack
       | Weak
       | WeakGone

      fun layout (t: t): Layout.t =
         let
            open Layout
         in
            case t of
               Array {hasIdentity, nonPointer = np, pointers = p} =>
                  seq [str "Array ",
                       record [("hasIdentity", Bool.layout hasIdentity),
                               ("nonPointer", Bytes.layout np),
                               ("pointers", Int.layout p)]]
             | Normal {hasIdentity, nonPointer = np, pointers = p} =>
                  seq [str "Normal ",
                       record [("hasIdentity", Bool.layout hasIdentity),
                               ("nonPointer", Words.layout np),
                               ("pointers", Int.layout p)]]
             | Stack => str "Stack"
             | Weak => str "Weak"
             | WeakGone => str "WeakGone"
         end
      val _ = layout (* quell unused warning *)
   end

val maxTypeIndex = Int.pow (2, 19)
   
fun typeIndexToHeader typeIndex =
   (Assert.assert ("Runtime.header", fn () =>
                   0 <= typeIndex
                   andalso typeIndex < maxTypeIndex)
    ; Word.orb (0w1, Word.<< (Word.fromInt typeIndex, 0w1)))

fun headerToTypeIndex w = Word.toInt (Word.>> (w, 0w1))

val arrayHeaderSize = Bytes.scale (Bytes.inWord, 3)

val intInfOverhead = Bytes.+ (arrayHeaderSize, Bytes.inWord) (* for the sign *)

val labelSize = Bytes.inWord

val limitSlop = Bytes.fromInt 512

val normalHeaderSize = Bytes.inWord

val pointerSize = Bytes.inWord

val array0Size =
   Bytes.+ (arrayHeaderSize, Bytes.inWord) (* for the forwarding pointer *)

val arrayLengthOffset = Bytes.~ (Bytes.scale (Bytes.inWord, 2))

val allocTooLarge = Bytes.fromWord 0wxFFFFFFFC

val headerOffset = Bytes.~ Bytes.inWord

fun normalSize {nonPointers, pointers} =
   Bytes.+ (Words.toBytes nonPointers,
            Bytes.scale (pointerSize, pointers))
 
val maxFrameSize = Bytes.fromInt (Int.pow (2, 16))

end
