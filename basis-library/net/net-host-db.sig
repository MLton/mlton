signature NET_HOST_DB =
   sig
      eqtype in_addr
      eqtype addr_family
      type entry
      val name: entry -> string
      val aliases: entry -> string list
      val addrType: entry -> addr_family
      val addr: entry -> in_addr
      val addrs: entry -> in_addr list
      val getByName: string -> entry option
      val getByAddr: in_addr -> entry option
      val getHostName: unit -> string
      val scan: (char, 'a) StringCvt.reader -> (in_addr, 'a) StringCvt.reader
      val fromString: string -> in_addr option
      val toString: in_addr -> string
   end

signature NET_HOST_DB_EXTRA =
   sig
      include NET_HOST_DB
      type pre_in_addr
      val new_in_addr: unit -> (pre_in_addr * (unit -> in_addr))
      val inAddrToWord: in_addr -> word
      val wordToInAddr: word -> in_addr
      val any: unit -> in_addr
   end