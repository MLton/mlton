structure UnixSock : UNIX_SOCK =
   struct
      structure Prim = Primitive.Socket.UnixSock

      datatype unix = UNIX
      type 'sock_type sock = (unix, 'sock_type) Socket.sock
      type 'mode stream_sock = 'mode Socket.stream sock
      type dgram_sock = Socket.dgram sock
      type sock_addr = unix Socket.sock_addr
      val unixAF = Primitive.Socket.AF.UNIX

      fun toAddr s = 
	let
	  val (sa, salen, finish) = Socket.new_sock_addr ()
	  val _ = Prim.toAddr (s, String.size s, sa, salen)
	in 
	  finish ()
	end
      fun fromAddr sa = 
	let
	  val sa = Socket.unpackSockAddr sa
	  val len = Prim.pathLen sa
	  val a = CharArray.array (len, #"\000")
	  val _ = Prim.fromAddr (sa, a, len)
	in
	  CharArray.extract (a, 0, SOME len)
	end 

      structure Strm =
	 struct
	   structure Prim = Prim.Strm

	    fun socket () = GenericSock.socket (unixAF, Socket.SOCK.stream)
	    fun socketPair () = GenericSock.socketPair (unixAF, Socket.SOCK.stream)
	 end
      structure DGrm =
	 struct
	    structure Prim = Prim.DGrm

	    fun socket () = GenericSock.socket (unixAF, Socket.SOCK.dgram)
	    fun socketPair () = GenericSock.socketPair (unixAF, Socket.SOCK.dgram)
	 end
   end
