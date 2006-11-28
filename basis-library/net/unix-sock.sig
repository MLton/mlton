signature UNIX_SOCK =
   sig
      type unix
      type 'sock_type sock = (unix, 'sock_type) Socket.sock
      type 'mode stream_sock = 'mode Socket.stream sock
      type dgram_sock = Socket.dgram sock
      type sock_addr = unix Socket.sock_addr
      val unixAF: Socket.AF.addr_family
      val toAddr: string -> sock_addr
      val fromAddr: sock_addr -> string
      structure Strm : 
         sig
            val socket: unit -> 'mode stream_sock
            val socketPair: unit -> 'mode stream_sock * 'mode stream_sock
         end
      structure DGrm : 
         sig
            val socket: unit -> dgram_sock
            val socketPair: unit -> dgram_sock * dgram_sock
         end
   end
