type int = Int.t
type word = Word.t
   
signature GC_FIELD =
   sig
      datatype t =
	 Base
       | CanHandle
       | CurrentThread
       | Frontier (* The place where the next object is allocated. *)
       | Limit (* frontier + heapSize - LIMIT_SLOP *)
       | LimitPlusSlop (* frontier + heapSize *)
       | MaxFrameSize
       | SignalIsPending
       | StackBottom
       | StackLimit (* Must have  StackTop <= StackLimit *)
       | StackTop (* Points at the next available word on the stack. *)

      val layout: t -> Layout.t
      val offset: t -> int (* Field offset in struct GC_state. *)
      val setOffsets: {base: int,
		       canHandle: int,
		       currentThread: int,
		       frontier: int,
		       limit: int,
		       limitPlusSlop: int,
		       maxFrameSize: int,
		       signalIsPending: int,
		       stackBottom: int,
		       stackLimit: int,
		       stackTop: int} -> unit
      val toString: t -> string
   end
