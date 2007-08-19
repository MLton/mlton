(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonSocket: MLTON_SOCKET =
struct

structure Port =
   struct
      type t = int
   end

structure Address =
   struct
      type t = NetHostDB.in_addr
   end

structure Host =
   struct
      type t = {name: string}

      val get: NetHostDB.entry option -> t option =
        Option.map (fn entry => {name = NetHostDB.name entry})

      val getByAddress = get o NetHostDB.getByAddr
      val getByName = get o NetHostDB.getByName
   end

type passiveSocket = (INetSock.inet, Socket.passive Socket.stream) Socket.sock
type activeSocket = (INetSock.inet, Socket.active Socket.stream) Socket.sock
type t = passiveSocket

val listen: unit -> Port.t * passiveSocket =
   fn () =>
   let
      val sl: passiveSocket = INetSock.TCP.socket ()
      val _ = Socket.Ctl.setREUSEADDR (sl, true)
      val addr: INetSock.inet Socket.sock_addr = INetSock.any 0
      val _ = Socket.bind (sl, addr)
      val _ = Socket.listen (sl, 5)
      val addr: INetSock.inet Socket.sock_addr = Socket.Ctl.getSockName sl
      val (_, port: int) = INetSock.fromAddr addr
   in
      (port, sl)
   end

val listenAt: Port.t -> passiveSocket =
   fn port =>
   let
      val sl: passiveSocket = INetSock.TCP.socket ()
      val _ = Socket.Ctl.setREUSEADDR (sl, true)
      val addr: INetSock.inet Socket.sock_addr = INetSock.any port
      val _ = Socket.bind (sl, addr)
      val _ = Socket.listen (sl, 5)
   in
      sl
   end

fun sockToIO (sock: activeSocket) =
   let
      val fd = Socket.sockToFD sock
      val ins = TextIO.newIn (fd, "<socket>")
      val out = TextIO.newOut (Posix.IO.dup fd, "<socket>")
   in (ins, out)
   end

fun accept s =
   let
      val (sock: activeSocket, addr: INetSock.inet Socket.sock_addr) =
         Socket.accept s
      val (in_addr: NetHostDB.in_addr, port: int) = INetSock.fromAddr addr
      val (ins, out) = sockToIO sock
   in
      (in_addr, port, ins, out)
   end

fun connect (host, port) =
   let
      val hp: NetHostDB.entry = valOf (NetHostDB.getByName host)
      val res: activeSocket = INetSock.TCP.socket ()
      val addr: INetSock.inet Socket.sock_addr =
         INetSock.toAddr (NetHostDB.addr hp, port)
      val _ = Socket.connect (res, addr)
      val (ins, out) = sockToIO res
   in 
      (ins, out)
   end

fun shutdown (fd: Posix.IO.file_desc, mode: Socket.shutdown_mode): unit =
   Socket.shutdown (Socket.fdToSock fd, mode)

fun shutdownRead ins =
   shutdown (TextIO.inFd ins, Socket.NO_RECVS)

fun shutdownWrite out =
   (TextIO.flushOut out
    ; shutdown (TextIO.outFd out, Socket.NO_SENDS))

val fdToSock = Socket.fdToSock

structure Ctl = Socket.CtlExtra

end
