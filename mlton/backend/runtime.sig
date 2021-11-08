(* Copyright (C) 2009,2016-2017,2019-2021 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature RUNTIME_STRUCTS =
   sig
   end

signature RUNTIME =
   sig
      include RUNTIME_STRUCTS

      structure GCField:
         sig
            datatype t =
               AtomicState
             | CardMapAbsolute
             | CurSourceSeqIndex
             | ExnStack
             | Frontier (* The place where the next object is allocated. *)
             | Limit (* frontier + heapSize - LIMIT_SLOP *)
             | LimitPlusSlop (* frontier + heapSize *)
             | SignalIsPending
             | StackBottom
             | StackLimit (* Must have StackTop <= StackLimit *)
             | StackTop (* Points at the next available byte on the stack. *)

            val layout: t -> Layout.t
            val offset: t -> Bytes.t (* Field offset in struct GC_state. *)
            val toString: t -> string
            val volatile: t -> bool
         end
      structure RObjectType:
         sig
            datatype t =
               Normal of {hasIdentity: bool,
                          bytesNonObjptrs: Bytes.t,
                          numObjptrs: int}
             | Sequence of {hasIdentity: bool,
                            bytesNonObjptrs: Bytes.t,
                            numObjptrs: int}
             | Stack
             | Weak of {gone: bool}
         end

      val cpointerSize: unit -> Bytes.t
      val headerOffset: unit -> Bytes.t
      val headerSize: unit -> Bytes.t
      val labelSize: unit -> Bytes.t
      val limitSlop: Bytes.t
      val maxFrameSize: Bytes.t
      val normalMetaDataSize: unit -> Bytes.t
      val objptrSize: unit -> Bytes.t
      val sequenceCounterOffset: unit -> Bytes.t
      val sequenceCounterSize: unit -> Bytes.t
      val sequenceLengthOffset: unit -> Bytes.t
      val sequenceLengthSize: unit -> Bytes.t
      val sequenceMetaDataSize: unit -> Bytes.t
      val typeIndexToHeader: int -> word
   end
