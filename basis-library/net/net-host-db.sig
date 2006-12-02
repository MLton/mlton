signature NET_HOST_DB =
   sig
      eqtype addr_family
      type entry
      eqtype in_addr

      val addr: entry -> in_addr
      val addrType: entry -> addr_family
      val addrs: entry -> in_addr list
      val aliases: entry -> string list
      val fromString: string -> in_addr option
      val getByAddr: in_addr -> entry option
      val getByName: string -> entry option
      val getHostName: unit -> string
      val name: entry -> string
      val scan: (char, 'a) StringCvt.reader -> (in_addr, 'a) StringCvt.reader
      val toString: in_addr -> string
   end

signature NET_HOST_DB_EXTRA =
   sig
      include NET_HOST_DB
      type pre_in_addr

      val any: unit -> in_addr
      val inAddrToWord8Vector: in_addr -> Word8.word vector
      val new_in_addr: unit -> pre_in_addr * (unit -> in_addr)
      val preInAddrToWord8Array: pre_in_addr -> Word8.word array
   end
