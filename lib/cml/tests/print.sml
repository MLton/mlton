structure TextIO =
   struct
      fun print s =
	 MLton.Thread.atomically
	 (fn () => TextIO.print s)
   end

