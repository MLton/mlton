signature PARSE =
   sig
      datatype result =
	 Alias of File.t
       | Bad of string (* error message *)
       | DoesNotExist
       | Members of File.t list
      (* Pre: cmfile must not contain any path, i.e. it must be in the
       *      current directory.
       * The resulting members are either absolute or relative to the current
       * directory.
       *)
      val parse: {cmfile: string} -> result
   end
