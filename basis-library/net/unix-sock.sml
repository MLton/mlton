(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure UnixSock : UNIX_SOCK =
   struct
      structure Prim = PrimitiveFFI.Socket.UnixSock

      datatype unix = UNIX
      type 'sock_type sock = (unix, 'sock_type) Socket.sock
      type 'mode stream_sock = 'mode Socket.stream sock
      type dgram_sock = Socket.dgram sock
      type sock_addr = unix Socket.sock_addr
      val unixAF = PrimitiveFFI.Socket.AF.UNIX

      fun toAddr s = 
        let
          val (sa, salen, finish) = Socket.new_sock_addr ()
          val _ = Prim.toAddr (NullString.nullTerm s, 
                               C_Size.fromInt (String.size s), 
                               sa, salen)
        in 
          finish ()
        end

      fun fromAddr sa = 
        let
          val sa = Socket.unpackSockAddr sa
          val len = Prim.pathLen sa
          val a = CharArray.array (C_Size.toInt len, #"\000")
          val _ = Prim.fromAddr (sa, CharArray.toPoly a, len)
        in
           CharArraySlice.vector (CharArraySlice.slice (a, 0, SOME (C_Size.toInt len)))
        end 

      structure Strm =
         struct
            fun socket () = GenericSock.socket (unixAF, Socket.SOCK.stream)
            fun socketPair () = GenericSock.socketPair (unixAF, Socket.SOCK.stream)
         end
      structure DGrm =
         struct
            fun socket () = GenericSock.socket (unixAF, Socket.SOCK.dgram)
            fun socketPair () = GenericSock.socketPair (unixAF, Socket.SOCK.dgram)
         end
   end
