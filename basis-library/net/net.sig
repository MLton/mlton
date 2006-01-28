signature NET =
   sig
      val htonl: Int32.int -> Int32.int
      val ntohl: Int32.int -> Int32.int
      val htons: Int16.int -> Int16.int
      val ntohs: Int16.int -> Int16.int
   end
