signature NET =
   sig
     val htonl: int -> int
     val ntohl: int -> int
     val htons: int -> int
     val ntohs: int -> int
   end