signature SEXP_STRUCTS = 
   sig
   end

signature SEXP = 
   sig
      include SEXP_STRUCTS
      
      datatype t =
	 Atom of string
       | List of t list

      val input: In.t -> t option (* NONE if eof *)
      val layout: t -> Layout.t
   end
