structure Err =
   struct
      datatype t = T of {inner: t option,
			 name: string,
			 obj: Layout.t}

      fun layout (T {inner, name, obj}): Layout.t =
	 let
	    open Layout
	 in
	    align [case inner of
		      NONE => empty
		    | SOME e => layout e,
	           seq [str (concat ["invalid ", name, ": "]), obj]]
	 end

      exception E of t

      fun check (name: string,
		 ok: unit -> bool,
		 layout: unit -> Layout.t): unit =
	 if ok () handle E e => raise E (T {inner = SOME e,
					    name = name,
					    obj = layout ()})
	    then ()
	 else raise E (T {inner = NONE,
			  name = name,
			  obj = layout ()})
   end
