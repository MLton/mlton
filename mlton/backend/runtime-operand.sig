signature RUNTIME_OPERAND =
   sig
      datatype t =
	 Frontier (* The place where the next object is allocated. *)
       | Limit (* frontier + heapSize - LIMIT_SLOP *)
       | LimitPlusSlop (* frontier + heapSize *)
       | StackLimit (* Must have  StackTop <= StackLimit *)
       | StackTop (* Points at the next available word on the stack. *)

      val layout: t -> Layout.t
      val toString: t -> string
   end
