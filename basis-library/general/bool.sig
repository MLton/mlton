signature BOOL_GLOBAL =
   sig
      datatype bool = false | true

      val not: bool -> bool
   end

signature BOOL =
   sig
      include BOOL_GLOBAL

      val toString: bool -> string
      val fromString: string -> bool option 
      val scan: (char, 'a) StringCvt.reader -> (bool, 'a) StringCvt.reader
   end
