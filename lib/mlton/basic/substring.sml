structure Substring: SUBSTRING =
   struct
      open Pervasive.Substring

      type t = substring

      val length = size

      val substring =
	 fn (s, {start, length}) => substring (s, start, length)

      val base =
	 fn ss => let val (s, start, length) = base ss
		  in (s, {start = start, length = length})
		  end

      val toString = string
	 
      val layout = String1.layout o toString 
   end
