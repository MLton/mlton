(* Copyright (C) 2002-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure INetSock:> INET_SOCK =
   struct
      structure Prim = PrimitiveFFI.Socket.INetSock

      datatype inet = INET (* a phantom type*)
      type 'sock_type sock = (inet, 'sock_type) Socket.sock
      type 'mode stream_sock = 'mode Socket.stream sock
      type dgram_sock = Socket.dgram sock
      type sock_addr = inet Socket.sock_addr

      val inetAF = PrimitiveFFI.Socket.AF.INET

      fun toAddr (in_addr, port) =
         let
            val port = Word16.fromInt port
                       handle Overflow => PosixError.raiseSys PosixError.inval
            val port = Net.Word16.hton port
            val (sa, salen, finish) = Socket.new_sock_addr ()
            val _ = Prim.toAddr (NetHostDB.inAddrToWord8Vector in_addr,
                                 port, sa, salen)

         in
            finish ()
         end

      fun any port = toAddr (NetHostDB.any (), port)

      fun fromAddr sa =
        let
          val () = Prim.fromAddr (Socket.unpackSockAddr sa)
          val port = Prim.getPort ()
          val port = Net.Word16.ntoh port
          val port = Word16.toInt port
          val (ia, finish) = NetHostDB.new_in_addr ()
          val _ = Prim.getInAddr (NetHostDB.preInAddrToWord8Array ia)
        in
          (finish (), port)
        end

      structure UDP =
         struct
            fun socket' prot = GenericSock.socket' (inetAF, Socket.SOCK.dgram, prot)
            fun socket () = socket' 0
         end

      structure TCP =
         struct
            structure Prim = Prim.Ctl

            fun socket' prot = GenericSock.socket' (inetAF, Socket.SOCK.stream, prot)
            fun socket () = socket' 0

            fun getNODELAY sock =
               Socket.CtlExtra.getSockOptBool
               (Prim.IPPROTO_TCP, Prim.TCP_NODELAY) sock

            fun setNODELAY (sock, optval) =
               Socket.CtlExtra.setSockOptBool
               (Prim.IPPROTO_TCP, Prim.TCP_NODELAY) (sock,optval)
         end
   end
