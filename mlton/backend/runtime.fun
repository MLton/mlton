(* Copyright (C) 2009,2016-2017,2019 Matthew Fluet.
 * Copyright (C) 2002-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Runtime (S: RUNTIME_STRUCTS): RUNTIME =
struct

open S

structure GCField =
   struct
      datatype t =
         AtomicState
       | CardMapAbsolute
       | CurSourceSeqIndex
       | ExnStack
       | Frontier
       | Limit
       | LimitPlusSlop
       | SignalIsPending
       | StackBottom
       | StackLimit
       | StackTop

      val atomicStateOffset: Bytes.t ref = ref Bytes.zero
      val cardMapAbsoluteOffset: Bytes.t ref = ref Bytes.zero
      val curSourceSeqIndexOffset: Bytes.t ref = ref Bytes.zero
      val exnStackOffset: Bytes.t ref = ref Bytes.zero
      val frontierOffset: Bytes.t ref = ref Bytes.zero
      val limitOffset: Bytes.t ref = ref Bytes.zero
      val limitPlusSlopOffset: Bytes.t ref = ref Bytes.zero
      val signalIsPendingOffset: Bytes.t ref = ref Bytes.zero
      val stackBottomOffset: Bytes.t ref = ref Bytes.zero
      val stackLimitOffset: Bytes.t ref = ref Bytes.zero
      val stackTopOffset: Bytes.t ref = ref Bytes.zero

      fun setOffsets {atomicState, cardMapAbsolute, curSourceSeqIndex,
                      exnStack, frontier, limit, limitPlusSlop,
                      signalIsPending, stackBottom, stackLimit, stackTop} =
         (atomicStateOffset := atomicState
          ; cardMapAbsoluteOffset := cardMapAbsolute
          ; curSourceSeqIndexOffset := curSourceSeqIndex
          ; exnStackOffset := exnStack
          ; frontierOffset := frontier
          ; limitOffset := limit
          ; limitPlusSlopOffset := limitPlusSlop
          ; signalIsPendingOffset := signalIsPending
          ; stackBottomOffset := stackBottom
          ; stackLimitOffset := stackLimit
          ; stackTopOffset := stackTop)

      val offset =
         fn AtomicState => !atomicStateOffset
          | CardMapAbsolute => !cardMapAbsoluteOffset
          | CurSourceSeqIndex => !curSourceSeqIndexOffset
          | ExnStack => !exnStackOffset
          | Frontier => !frontierOffset
          | Limit => !limitOffset
          | LimitPlusSlop => !limitPlusSlopOffset
          | SignalIsPending => !signalIsPendingOffset
          | StackBottom => !stackBottomOffset
          | StackLimit => !stackLimitOffset
          | StackTop => !stackTopOffset

      val atomicStateSize: Bytes.t ref = ref Bytes.zero
      val cardMapAbsoluteSize: Bytes.t ref = ref Bytes.zero
      val curSourceSeqIndexSize: Bytes.t ref = ref Bytes.zero
      val exnStackSize: Bytes.t ref = ref Bytes.zero
      val frontierSize: Bytes.t ref = ref Bytes.zero
      val limitSize: Bytes.t ref = ref Bytes.zero
      val limitPlusSlopSize: Bytes.t ref = ref Bytes.zero
      val signalIsPendingSize: Bytes.t ref = ref Bytes.zero
      val stackBottomSize: Bytes.t ref = ref Bytes.zero
      val stackLimitSize: Bytes.t ref = ref Bytes.zero
      val stackTopSize: Bytes.t ref = ref Bytes.zero

      fun setSizes {atomicState, cardMapAbsolute, curSourceSeqIndex,
                    exnStack, frontier, limit, limitPlusSlop,
                    signalIsPending, stackBottom, stackLimit, stackTop} =
         (atomicStateSize := atomicState
          ; cardMapAbsoluteSize := cardMapAbsolute
          ; curSourceSeqIndexSize := curSourceSeqIndex
          ; exnStackSize := exnStack
          ; frontierSize := frontier
          ; limitSize := limit
          ; limitPlusSlopSize := limitPlusSlop
          ; signalIsPendingSize := signalIsPending
          ; stackBottomSize := stackBottom
          ; stackLimitSize := stackLimit
          ; stackTopSize := stackTop)

      val size =
         fn AtomicState => !atomicStateSize
          | CardMapAbsolute => !cardMapAbsoluteSize
          | CurSourceSeqIndex => !curSourceSeqIndexSize
          | ExnStack => !exnStackSize
          | Frontier => !frontierSize
          | Limit => !limitSize
          | LimitPlusSlop => !limitPlusSlopSize
          | SignalIsPending => !signalIsPendingSize
          | StackBottom => !stackBottomSize
          | StackLimit => !stackLimitSize
          | StackTop => !stackTopSize

      val toString =
         fn AtomicState => "AtomicState"
          | CardMapAbsolute => "CardMapAbsolute"
          | CurSourceSeqIndex => "CurSourceSeqIndex"
          | ExnStack => "ExnStack"
          | Frontier => "Frontier"
          | Limit => "Limit"
          | LimitPlusSlop => "LimitPlusSlop"
          | SignalIsPending => "SignalIsPending"
          | StackBottom => "StackBottom"
          | StackLimit => "StackLimit"
          | StackTop => "StackTop"

      val layout = Layout.str o toString
   end

structure RObjectType =
   struct
      datatype t =
         Normal of {hasIdentity: bool,
                    bytesNonObjptrs: Bytes.t,
                    numObjptrs: int}
       | Sequence of {hasIdentity: bool,
                      bytesNonObjptrs: Bytes.t,
                      numObjptrs: int}
       | Stack
       | Weak of {gone: bool}

      fun layout (t: t): Layout.t =
         let
            open Layout
         in
            case t of
               Normal {hasIdentity, bytesNonObjptrs, numObjptrs} =>
                  seq [str "Normal ",
                       record [("hasIdentity", Bool.layout hasIdentity),
                               ("bytesNonObjptrs", Bytes.layout bytesNonObjptrs),
                               ("numObjptrs", Int.layout numObjptrs)]]
             | Sequence {hasIdentity, bytesNonObjptrs, numObjptrs} =>
                  seq [str "Sequence ",
                       record [("hasIdentity", Bool.layout hasIdentity),
                               ("bytesNonObjptrs", Bytes.layout bytesNonObjptrs),
                               ("numObjptrs", Int.layout numObjptrs)]]
             | Stack => str "Stack"
             | Weak {gone} => 
                  seq [str "Weak",
                       record [("gone", Bool.layout gone)]]
         end
      val _ = layout (* quell unused warning *)
   end

(* see gc/object.h *)
local
   val maxTypeIndex = Int.pow (2, 19)
in
   (* see gc/object.c:buildHeaderFromTypeIndex *)
   fun typeIndexToHeader typeIndex =
      (Assert.assert ("Runtime.header", fn () =>
                      0 <= typeIndex
                      andalso typeIndex < maxTypeIndex)
       ; Word.orb (0w1, Word.<< (Word.fromInt typeIndex, 0w1)))
      
   fun headerToTypeIndex w = Word.toInt (Word.>> (w, 0w1))
end

(* see gc/object.h *)
val objptrSize : unit -> Bytes.t =
   Promise.lazy (Bits.toBytes o Control.Target.Size.objptr)

(* see gc/object.h *)
val headerSize : unit -> Bytes.t =
   Promise.lazy (Bits.toBytes o Control.Target.Size.header)
val headerOffset : unit -> Bytes.t = 
   Promise.lazy (Bytes.~ o headerSize)

(* see gc/sequence.h *)
val sequenceLengthSize : unit -> Bytes.t =
   Promise.lazy (Bits.toBytes o Control.Target.Size.seqIndex)
val sequenceLengthOffset : unit -> Bytes.t =
   Promise.lazy (fn () => Bytes.~ (Bytes.+ (headerSize (),
                                            sequenceLengthSize ())))

(* see gc/object.h and gc/sequence.h *)
val sequenceMetaDataSize : unit -> Bytes.t =
   Promise.lazy (Bits.toBytes o Control.Target.Size.sequenceMetaData)
val normalMetaDataSize : unit -> Bytes.t =
   Promise.lazy (Bits.toBytes o Control.Target.Size.normalMetaData)

val cpointerSize : unit -> Bytes.t =
   Promise.lazy (Bits.toBytes o Control.Target.Size.cpointer)
val labelSize = cpointerSize

(* See gc/heap.h. *)
val limitSlop = Bytes.fromInt 512

(* See gc/frame.h. *)
val maxFrameSize = Bytes.fromInt (Int.pow (2, 16))

end
