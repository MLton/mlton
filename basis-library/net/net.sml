structure Net : NET =
   struct
      structure Prim = Primitive.Net

      val htonl = Prim.htonl
      val ntohl = Prim.ntohl
      val htons = Prim.htons
      val ntohs = Prim.ntohs
   end