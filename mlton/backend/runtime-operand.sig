signature RUNTIME_OPERAND =
   sig
      datatype t =
	 Frontier
       | Limit
       | LimitPlusSlop
       | StackLimit
       | StackTop

      val layout: t -> Layout.t
      val toString: t -> string
   end
