(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure INetSock:> INET_SOCK =
   struct
      structure Prim = Primitive.Socket.INetSock
         
      datatype inet = INET (* a phantom type*)
      type 'sock_type sock = (inet, 'sock_type) Socket.sock
      type 'mode stream_sock = 'mode Socket.stream sock
      type dgram_sock = Socket.dgram sock
      type sock_addr = inet Socket.sock_addr

      val inetAF = NetHostDB.intToAddrFamily Primitive.Socket.AF.INET

      fun toAddr (in_addr, port) =
         if port < 0 orelse port >= 0x10000
            then PosixError.raiseSys PosixError.inval
         else
            let
               val (sa, salen, finish) = Socket.new_sock_addr ()
               val _ = Prim.toAddr (NetHostDB.inAddrToWord8Vector in_addr,
                                    Net.htons port, sa, salen)
            in
               finish ()
            end

      fun any port = toAddr (NetHostDB.any (), port)

      fun fromAddr sa =
        let
          val _ = Prim.fromAddr (Word8Vector.toPoly (Socket.unpackSockAddr sa))
          val port = Net.ntohs (Prim.getPort ())
          val (ia, finish) = NetHostDB.new_in_addr ()
          val _ = Prim.getInAddr (NetHostDB.preInAddrToWord8Array ia)
        in
          (finish (), port)
        end

      structure UDP =
         struct
            fun socket' prot =
               GenericSock.socket' (inetAF, Socket.SOCK.dgram, prot)
               
            fun socket () = socket' 0
         end
      
      structure TCP =
         struct
            structure Prim = Prim.TCP

            fun socket' prot =
               GenericSock.socket' (inetAF, Socket.SOCK.stream, prot)
               
            fun socket () = socket' 0

            fun getNODELAY sock =
              Socket.CtlExtra.getSockOptBool
              (Prim.TCP, Prim.NODELAY) sock

            fun setNODELAY (sock,optval) =
              Socket.CtlExtra.setSockOptBool
              (Prim.TCP, Prim.NODELAY) (sock,optval)
         end
   end
