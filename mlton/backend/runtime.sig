(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
type word = Word.t

signature RUNTIME_STRUCTS =
   sig
   end

signature RUNTIME =
   sig
      include RUNTIME_STRUCTS

      structure GCField:
	 sig
	    datatype t =
	       CanHandle
	     | CardMap
	     | CurrentThread
	     | ExnStack
	     | Frontier (* The place where the next object is allocated. *)
	     | Limit (* frontier + heapSize - LIMIT_SLOP *)
	     | LimitPlusSlop (* frontier + heapSize *)
	     | MaxFrameSize
	     | SignalIsPending
	     | StackBottom
	     | StackLimit (* Must have StackTop <= StackLimit *)
	     | StackTop (* Points at the next available word on the stack. *)

	    val equals: t * t -> bool
	    val layout: t -> Layout.t
	    val offset: t -> Bytes.t (* Field offset in struct GC_state. *)
	    val setOffsets: {canHandle: Bytes.t,
			     cardMap: Bytes.t,
			     currentThread: Bytes.t,
			     exnStack: Bytes.t,
			     frontier: Bytes.t,
			     limit: Bytes.t,
			     limitPlusSlop: Bytes.t,
			     maxFrameSize: Bytes.t,
			     signalIsPending: Bytes.t,
			     stackBottom: Bytes.t,
			     stackLimit: Bytes.t,
			     stackTop: Bytes.t} -> unit
	    val toString: t -> string
	 end
      structure RObjectType:
	 sig
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
	 end

      val allocTooLarge: Bytes.t
      val arrayHeaderSize: Bytes.t
      val arrayLengthOffset: Bytes.t
      val array0Size: Bytes.t
      val headerOffset: Bytes.t
      val headerToTypeIndex: word -> int
      val intInfOverhead: Bytes.t
      val labelSize: Bytes.t
      (* Same as LIMIT_SLOP from gc.c. *)
      val limitSlop: Bytes.t
      val maxFrameSize: Bytes.t
      val normalHeaderSize: Bytes.t
      (* normalBytes does not include the header. *)
      val normalSize: {nonPointers: Words.t,
		       pointers: int} -> Bytes.t
      val pointerSize: Bytes.t
      val typeIndexToHeader: int -> word
   end
