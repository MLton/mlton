(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
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

      structure Type: MTYPE
      structure CFunction: C_FUNCTION
      sharing Type = CFunction.Type
      structure GCField:
	 sig
	    datatype t =
	       CanHandle
	     | CardMap
	     | CurrentThread
	     | Frontier (* The place where the next object is allocated. *)
	     | Limit (* frontier + heapSize - LIMIT_SLOP *)
	     | LimitPlusSlop (* frontier + heapSize *)
	     | MaxFrameSize
	     | ProfileAllocIndex
	     | SignalIsPending
	     | StackBottom
	     | StackLimit (* Must have StackTop <= StackLimit *)
	     | StackTop (* Points at the next available word on the stack. *)

	    val layout: t -> Layout.t
	    val offset: t -> int (* Field offset in struct GC_state. *)
	    val setOffsets: {canHandle: int,
			     cardMap: int,
			     currentThread: int,
			     frontier: int,
			     limit: int,
			     limitPlusSlop: int,
			     maxFrameSize: int,
			     profileAllocIndex: int,
			     signalIsPending: int,
			     stackBottom: int,
			     stackLimit: int,
			     stackTop: int} -> unit
	    val toString: t -> string
	    val ty: t -> Type.t
	 end
      structure ObjectType:
	 sig
	    datatype t =
	       Array of {numBytesNonPointers: int,
			 numPointers: int}
	     | Normal of {numPointers: int,
			  numWordsNonPointers: int}
	     | Stack
	 end

      (* All sizes are in bytes, unless they explicitly say "pointers". *)

      val allocTooLarge: word
      val arrayHeaderSize: int
      val arrayLengthOffset: int
      val array0Size: int
      val headerOffset: int
      val headerToTypeIndex: word -> int
      val isWordAligned: int -> bool
      val intInfOverheadSize: int
      val labelSize: int
      (* Same as LIMIT_SLOP from gc.c. *)
      val limitSlop: int
      val maxFrameSize: int
      val normalHeaderSize: int
      (* normalSize does not include the header. *)
      val normalSize: {numPointers: int,
		       numWordsNonPointers: int} -> int
      val pointerSize: int
      val typeIndexToHeader: int -> word
      val wordAlignInt: int -> int (* Can raise Overflow. *)
      val wordAlignWord: word -> word (* Can raise Overflow. *)
      val wordSize: int
   end
