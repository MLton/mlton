(* Copyright (C) 2009,2016-2017 Matthew Fluet.
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

      val atomicStateOffset: Bytes.t ref = ref Bytes.zero
      val cardMapAbsoluteOffset: Bytes.t ref = ref Bytes.zero
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

      fun setOffsets {atomicState, cardMapAbsolute, currentThread, curSourceSeqsIndex, 
                      exnStack, frontier, limit, limitPlusSlop, maxFrameSize, 
                      signalIsPending, stackBottom, stackLimit, stackTop} =
         (atomicStateOffset := atomicState
          ; cardMapAbsoluteOffset := cardMapAbsolute
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
         fn AtomicState => !atomicStateOffset
          | CardMapAbsolute => !cardMapAbsoluteOffset
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

      val atomicStateSize: Bytes.t ref = ref Bytes.zero
      val cardMapAbsoluteSize: Bytes.t ref = ref Bytes.zero
      val currentThreadSize: Bytes.t ref = ref Bytes.zero
      val curSourceSeqsIndexSize: Bytes.t ref = ref Bytes.zero
      val exnStackSize: Bytes.t ref = ref Bytes.zero
      val frontierSize: Bytes.t ref = ref Bytes.zero
      val limitSize: Bytes.t ref = ref Bytes.zero
      val limitPlusSlopSize: Bytes.t ref = ref Bytes.zero
      val maxFrameSizeSize: Bytes.t ref = ref Bytes.zero
      val signalIsPendingSize: Bytes.t ref = ref Bytes.zero
      val stackBottomSize: Bytes.t ref = ref Bytes.zero
      val stackLimitSize: Bytes.t ref = ref Bytes.zero
      val stackTopSize: Bytes.t ref = ref Bytes.zero

      fun setSizes {atomicState, cardMapAbsolute, currentThread, curSourceSeqsIndex, 
                    exnStack, frontier, limit, limitPlusSlop, maxFrameSize, 
                    signalIsPending, stackBottom, stackLimit, stackTop} =
         (atomicStateSize := atomicState
          ; cardMapAbsoluteSize := cardMapAbsolute
          ; currentThreadSize := currentThread
          ; curSourceSeqsIndexSize := curSourceSeqsIndex
          ; exnStackSize := exnStack
          ; frontierSize := frontier
          ; limitSize := limit
          ; limitPlusSlopSize := limitPlusSlop
          ; maxFrameSizeSize := maxFrameSize
          ; signalIsPendingSize := signalIsPending
          ; stackBottomSize := stackBottom
          ; stackLimitSize := stackLimit
          ; stackTopSize := stackTop)

      val size =
         fn AtomicState => !atomicStateSize
          | CardMapAbsolute => !cardMapAbsoluteSize
          | CurrentThread => !currentThreadSize
          | CurSourceSeqsIndex => !curSourceSeqsIndexSize
          | ExnStack => !exnStackSize
          | Frontier => !frontierSize
          | Limit => !limitSize
          | LimitPlusSlop => !limitPlusSlopSize
          | MaxFrameSize => !maxFrameSizeSize
          | SignalIsPending => !signalIsPendingSize
          | StackBottom => !stackBottomSize
          | StackLimit => !stackLimitSize
          | StackTop => !stackTopSize

      val toString =
         fn AtomicState => "AtomicState"
          | CardMapAbsolute => "CardMapAbsolute"
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
                   bytesNonObjptrs: Bytes.t,
                   numObjptrs: int}
       | Normal of {hasIdentity: bool,
                    bytesNonObjptrs: Bytes.t,
                    numObjptrs: int}
       | Stack
       | Weak of {gone: bool}

      fun layout (t: t): Layout.t =
         let
            open Layout
         in
            case t of
               Array {hasIdentity, bytesNonObjptrs, numObjptrs} =>
                  seq [str "Array ",
                       record [("hasIdentity", Bool.layout hasIdentity),
                               ("bytesNonObjptrs", Bytes.layout bytesNonObjptrs),
                               ("numObjptrs", Int.layout numObjptrs)]]
             | Normal {hasIdentity, bytesNonObjptrs, numObjptrs} =>
                  seq [str "Normal ",
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

(* see gc/array.h *)
val arrayLengthSize : unit -> Bytes.t =
   Promise.lazy (Bits.toBytes o Control.Target.Size.seqIndex)
val arrayLengthOffset : unit -> Bytes.t =
   Promise.lazy (fn () => Bytes.~ (Bytes.+ (headerSize (),
                                            arrayLengthSize ())))

(* see gc/object.h and gc/array.h *)
val arrayMetaDataSize : unit -> Bytes.t =
   Promise.lazy (Bits.toBytes o Control.Target.Size.arrayMetaData)
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
