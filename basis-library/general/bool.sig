signature BOOL_GLOBAL =
   sig
      datatype bool = datatype bool

      val not: bool -> bool
   end

signature BOOL =
   sig
      include BOOL_GLOBAL

      val fromString: string -> bool option 
      val scan: (char, 'a) StringCvt.reader -> (bool, 'a) StringCvt.reader
      val toString: bool -> string
   end
