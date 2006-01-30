(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Socket:> SOCKET_EXTRA
   where type SOCK.sock_type = C.Int.t
   where type pre_sock_addr = Word8.word array
=
struct

structure Prim = PrimitiveFFI.Socket
structure Error = Posix.Error
structure Syscall = Error.SysCall
structure FileSys = Posix.FileSys

type sock = C.Sock.t
val sockToWord = SysWord.fromInt o C.Sock.toInt
val wordToSock = C.Sock.fromInt o SysWord.toInt
fun sockToFD sock = FileSys.wordToFD (sockToWord sock)
fun fdToSock fd = wordToSock (FileSys.fdToWord fd)

type pre_sock_addr = Word8.word array
datatype sock_addr = SA of Word8.word vector
fun unpackSockAddr (SA sa) = Word8Vector.fromPoly sa
fun new_sock_addr (): (pre_sock_addr * C.Socklen.t ref * (unit -> sock_addr)) = 
   let
      val salen = C.Size.toInt Prim.sockAddrStorageLen
      val sa = Array.array (salen, 0wx0)
      val salenRef = ref (C.Socklen.fromInt salen)
      fun finish () =
         SA (ArraySlice.vector (ArraySlice.slice 
                                (sa, 0, SOME (C.Socklen.toInt (!salenRef)))))
   in
      (sa, salenRef, finish)
   end
datatype dgram = DGRAM (* phantom *)
datatype stream = MODE (* phantom *)
datatype passive = PASSIVE (* phantom *)
datatype active = ACTIVE (* phantom *)

structure AF =
   struct
      type addr_family = NetHostDB.addr_family
      val i2a = NetHostDB.intToAddrFamily
      val names = [
                   ("UNIX", i2a Prim.AF.UNIX),
                   ("INET", i2a Prim.AF.INET),
                   ("INET6", i2a Prim.AF.INET6),
                   ("UNSPEC", i2a Prim.AF.UNSPEC)
                   ]
      fun list () = names
      fun toString af' =
         case List.find (fn (_, af) => af = af') names of
            SOME (name, _) => name
          | NONE => raise (Fail "Internal error: bogus addr_family")
      fun fromString name' =
         case List.find (fn (name, _) => name = name') names of
            SOME (_, af) => SOME af
          | NONE => NONE
   end

structure SOCK =
   struct
      type sock_type = C.Int.t
      val stream = Prim.SOCK.STREAM
      val dgram = Prim.SOCK.DGRAM
      val names = [
                   ("STREAM", stream),
                   ("DGRAM", dgram)
                   ]
      fun list () = names
      fun toString st' =
         case List.find (fn (_, st) => st = st') names of
            SOME (name, _) => name
          | NONE => raise (Fail "Internal error: bogus sock_type")
      fun fromString name' =
         case List.find (fn (name, _) => name = name') names of
            SOME (_, st) => SOME st
          | NONE => NONE
   end

structure CtlExtra =
   struct
      type level = C.Int.t
      type optname = C.Int.t
      type request = C.Int.t
      
      (* host byte order *)
      structure PW = PackWord32Host

      val wordLen = PW.bytesPerElem
      fun unmarshalWord (wa, _, s): word = 
         Word.fromLargeWord (PW.subArr (wa, s))
      val intLen: int = wordLen
      fun unmarshalInt (wa, l, s): int = 
         Word.toIntX (unmarshalWord (wa, l, s))
      val boolLen: int = intLen
      fun unmarshalBool (wa, l, s): bool = 
         if (unmarshalInt (wa, l, s)) = 0 then false else true
      val timeOptLen: int = boolLen + intLen
      fun unmarshalTimeOpt (wa, l, s): Time.time option =
         if unmarshalBool (wa, l, s)
            then SOME (Time.fromSeconds
                       (LargeInt.fromInt
                        (unmarshalInt (wa, l, s + 1))))
         else NONE

      fun marshalWord (w, wa, s) =
         PW.update (wa, s, Word.toLargeWord w)

      fun marshalInt (i, wa, s) =
         marshalWord (Word.fromInt i, wa, s)

      fun marshalBool (b, wa, s) =
         marshalInt (if b then 1 else 0, wa, s)

      fun marshalTimeOpt (t, wa, s) =
         case t of
            NONE => (marshalBool (false, wa, s)
                     ; marshalInt (0, wa, s + 1))
          | SOME t =>
               (marshalBool (true, wa, s)
                ; marshalWord (Word.fromLargeInt (Time.toSeconds t)
                               handle Overflow => Error.raiseSys Error.inval,
                               wa, s + 1))

      local
         fun make (optlen: int,
                   write: 'a * Word8Array.array * int -> unit,
                   unmarshal: Word8Array.array * int * int -> 'a) =
            let
               fun marshal (x: 'a): Word8Vector.vector =
                  let
                     val wa = Word8Array.array (optlen, 0wx0)
                  in
                     write (x, wa, 0)
                     ; Word8Array.vector wa
                  end
               fun getSockOpt (level: level, optname: optname) s =
                  let
                     val optval = Word8Array.array (optlen, 0wx0)
                     val optlen = ref (C.Socklen.fromInt optlen)
                  in
                     Syscall.simple
                     (fn () =>
                      Prim.Ctl.getSockOpt (s, level, optname,
                                           Word8Array.toPoly optval,
                                           optlen))
                     ; unmarshal (optval, C.Socklen.toInt (!optlen), 0)
                  end
               fun setSockOpt (level: level, optname: optname) (s, optval) =
                  let
                     val optval = marshal optval
                     val optlen = Word8Vector.length optval
                  in
                     Syscall.simple
                     (fn () => 
                      Prim.Ctl.setSockOpt (s, level, optname,
                                           Word8Vector.toPoly optval,
                                           C.Socklen.fromInt optlen))
                  end
               fun getIOCtl (request: request) s : 'a =
                  let
                     val optval = Word8Array.array (optlen, 0wx0)
                  in
                     Syscall.simple
                     (fn () =>
                      Prim.Ctl.getIOCtl
                      (s, request, Word8Array.toPoly optval))
                     ; unmarshal (optval, optlen, 0)
                  end
               fun setIOCtl (request: request) (s, optval: 'a): unit =
                  let
                     val optval = marshal optval
                  in
                     Syscall.simple
                     (fn () =>
                      Prim.Ctl.setIOCtl
                      (s, request, Word8Vector.toPoly optval))
                  end
            in
               (getSockOpt, getIOCtl, setSockOpt, setIOCtl)
            end
      in
         val (getSockOptInt, getIOCtlInt, setSockOptInt, _) =
            make (intLen, marshalInt, unmarshalInt)
         val (getSockOptBool, getIOCtlBool, setSockOptBool, _) =
            make (boolLen, marshalBool, unmarshalBool)
         val (getSockOptTimeOpt, _, setSockOptTimeOpt, _) =
            make (timeOptLen, marshalTimeOpt, unmarshalTimeOpt)
      end

      val getDEBUG = getSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_DEBUG)
      val setDEBUG = setSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_DEBUG)
      val getREUSEADDR = getSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_REUSEADDR)
      val setREUSEADDR = setSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_REUSEADDR)
      val getKEEPALIVE = getSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_KEEPALIVE)
      val setKEEPALIVE = setSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_KEEPALIVE)
      val getDONTROUTE = getSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_DONTROUTE)
      val setDONTROUTE = setSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_DONTROUTE)
      val getBROADCAST = getSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_BROADCAST)
      val getLINGER = getSockOptTimeOpt (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_LINGER)
      val setLINGER = setSockOptTimeOpt (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_LINGER)
      val setBROADCAST = setSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_BROADCAST)
      val getOOBINLINE = getSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_OOBINLINE)
      val setOOBINLINE = setSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_OOBINLINE)
      val getSNDBUF = getSockOptInt (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_SNDBUF)
      val setSNDBUF = setSockOptInt (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_SNDBUF)
      val getRCVBUF = getSockOptInt (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_RCVBUF)
      val setRCVBUF = setSockOptInt (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_RCVBUF)
      fun getTYPE s = getSockOptInt (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_TYPE) s
      fun getERROR s =
         let
            val se = getSockOptInt (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_ERROR) s
         in
            if 0 = se
               then NONE
            else SOME (Posix.Error.errorMsg se, SOME se)
         end handle Error.SysErr z => SOME z
      local
         fun getName (s, f: sock * pre_sock_addr * C.Socklen.t ref -> int) =
            let
               val (sa, salen, finish) = new_sock_addr ()
               val () = Syscall.simple (fn () => f (s, sa, salen))
            in
               finish ()
            end
      in
         fun getPeerName s = getName (s, Prim.Ctl.getPeerName)
         fun getSockName s = getName (s, Prim.Ctl.getSockName)
      end
      val getNREAD = getIOCtlInt Prim.Ctl.FIONREAD
      val getATMARK = getIOCtlBool Prim.Ctl.SIOCATMARK
   end

structure Ctl =
   struct
      open CtlExtra

      val getERROR = isSome o CtlExtra.getERROR
   end

fun sameAddr (SA sa1, SA sa2) = sa1 = sa2

fun familyOfAddr (SA sa) = NetHostDB.intToAddrFamily (Prim.familyOfAddr sa)

fun bind (s, SA sa) =
   Syscall.simple (fn () => Prim.bind (s, sa, C.Socklen.fromInt (Vector.length sa)))

fun listen (s, n) = 
   Syscall.simple (fn () => Prim.listen (s, n))

fun nonBlock' ({restart: bool},
               f : unit -> int, post : int -> 'a, again, no : 'a) =
   Syscall.syscallErr
   ({clear = false, restart = restart},
    fn () => let val res = f ()
             in 
                {return = res,
                 post = fn () => post res,
                 handlers = [(again, fn () => no)]}
             end)

fun nonBlock (f, post, no) =
   nonBlock' ({restart = true}, f, post, Error.again, no)

local
   structure PIO = PrimitiveFFI.Posix.IO
in
   fun withNonBlock (s, f: unit -> 'a) =
      let
         val fd = s
         val flags = 
            Syscall.simpleResultRestart (fn () => PIO.fcntl2 (fd, PIO.F_GETFL))
         val _ =
            Syscall.simpleResultRestart
            (fn () => 
             PIO.fcntl3 (fd, PIO.F_SETFL,
                         Word.toIntX
                         (Word.orb (Word.fromInt flags,
                                    SysWord.fromInt PrimitiveFFI.Posix.FileSys.O.NONBLOCK))))
      in
         DynamicWind.wind
         (f, fn () =>
          Syscall.simple (fn () => PIO.fcntl3 (fd, PIO.F_SETFL, flags)))
      end
end

fun connect (s, SA sa) =
   Syscall.simple (fn () => Prim.connect (s, sa, C.Socklen.fromInt (Vector.length sa)))

fun connectNB (s, SA sa) =
   nonBlock'
   ({restart = false}, fn () => 
    withNonBlock (s, fn () => Prim.connect (s, sa, C.Socklen.fromInt (Vector.length sa))),
    fn _ => true,
    Error.inprogress, false)

fun accept s =
   let
      val (sa, salen, finish) = new_sock_addr ()
      val s = Syscall.simpleResultRestart (fn () => Prim.accept (s, sa, salen))
   in
      (s, finish ())
   end

fun acceptNB s =
   let
      val (sa, salen, finish) = new_sock_addr ()
   in
      nonBlock
      (fn () => withNonBlock (s, fn () => Prim.accept (s, sa, salen)),
       fn s => SOME (s, finish ()),
       NONE)
   end

fun close s = Syscall.simple (fn () => Prim.close s)

datatype shutdown_mode = NO_RECVS | NO_SENDS | NO_RECVS_OR_SENDS

fun shutdownModeToHow m =
   case m of
      NO_RECVS => Prim.SHUT_RD
    | NO_SENDS => Prim.SHUT_WR
    | NO_RECVS_OR_SENDS => Prim.SHUT_RDWR

fun shutdown (s, m) =
   let val m = shutdownModeToHow m
   in Syscall.simple (fn () => Prim.shutdown (s, m))
   end

type sock_desc = OS.IO.iodesc

fun sockDesc sock = FileSys.fdToIOD (sockToFD sock)

fun sameDesc (desc1, desc2) =
   OS.IO.compare (desc1, desc2) = EQUAL

fun select {rds: sock_desc list, 
            wrs: sock_desc list, 
            exs: sock_desc list, 
            timeout: Time.time option} =
   let
      fun mk poll (sd,pds) =
         let
            val pd = Option.valOf (OS.IO.pollDesc sd)
            val pd = poll pd
         in
            pd::pds
         end
      val pds =
         (List.foldr (mk OS.IO.pollIn)
          (List.foldr (mk OS.IO.pollOut)
           (List.foldr (mk OS.IO.pollPri)
            [] exs) wrs) rds)
      val pis = OS.IO.poll (pds, timeout)
      val {rds, wrs, exs} =
         List.foldr
         (fn (pi,{rds,wrs,exs}) =>
          let
             fun mk (is,l) =
                if is pi
                   then (OS.IO.pollToIODesc (OS.IO.infoToPollDesc pi))::l
                else l
          in
             {rds = mk (OS.IO.isIn, rds),
              wrs = mk (OS.IO.isOut, wrs),
              exs = mk (OS.IO.isPri, exs)}
          end) 
         {rds = [], wrs = [], exs = []}
         pis
   in
      {rds = rds, wrs = wrs, exs = exs}
   end

val ioDesc = sockDesc
 
type out_flags = {don't_route: bool, oob: bool}

fun mk_out_flags {don't_route, oob} =
   Word.orb (if don't_route then Word.fromInt Prim.MSG_DONTROUTE else 0wx0,
                Word.orb (if oob then Word.fromInt Prim.MSG_OOB else 0wx0,
                             0wx0))
val no_out_flags = {don't_route = false, oob = false}

local
   fun make (base, toPoly, primSend, primSendTo) =
      let
         val base = fn sl => let val (buf, i, sz) = base sl
                             in (toPoly buf, i, sz)
                             end
         fun send' (s, sl, out_flags) =
            let
               val (buf, i, sz) = base sl
            in
               Syscall.simpleResultRestart
               (fn () => primSend (s, buf, i, C.Size.fromInt sz, 
                                   Word.toInt (mk_out_flags out_flags)))
            end
         fun send (sock, buf) = send' (sock, buf, no_out_flags)
         fun sendNB' (s, sl, out_flags) =
            let
               val (buf, i, sz) = base sl
            in
               nonBlock
               (fn () =>
                primSend (s, buf, i, C.Size.fromInt sz,
                          Word.toInt (
                          Word.orb (Word.fromInt Prim.MSG_DONTWAIT, 
                                    mk_out_flags out_flags))),
                SOME, 
                NONE)
            end
         fun sendNB (sock, sl) = sendNB' (sock, sl, no_out_flags)
         fun sendTo' (s, SA sa, sl, out_flags) =
            let
               val (buf, i, sz) = base sl
            in
               Syscall.simpleRestart
               (fn () =>
                primSendTo (s, buf, i, C.Size.fromInt sz,
                            Word.toInt (mk_out_flags out_flags), 
                            sa, C.Socklen.fromInt (Vector.length sa)))
            end
         fun sendTo (sock, sock_addr, sl) =
            sendTo' (sock, sock_addr, sl, no_out_flags)
         fun sendToNB' (s, SA sa, sl, out_flags) =
            let
               val (buf, i, sz) = base sl
            in
               nonBlock 
               (fn () =>
                primSendTo (s, buf, i, C.Size.fromInt sz,
                            Word.toInt (
                            Word.orb (Word.fromInt Prim.MSG_DONTWAIT,
                                      mk_out_flags out_flags)),
                            sa, C.Socklen.fromInt (Vector.length sa)),
                fn _ => true,
                false)
            end
         fun sendToNB (sock, sa, sl) =
            sendToNB' (sock, sa, sl, no_out_flags)
      in
         (send, send', sendNB, sendNB', sendTo, sendTo', sendToNB, sendToNB')
      end
in
   val (sendArr, sendArr', sendArrNB, sendArrNB',
        sendArrTo, sendArrTo', sendArrToNB, sendArrToNB') =
      make (Word8ArraySlice.base, Word8Array.toPoly,
            Prim.sendArr, Prim.sendArrTo)
   val (sendVec, sendVec', sendVecNB, sendVecNB',
        sendVecTo, sendVecTo', sendVecToNB, sendVecToNB') =
      make (Word8VectorSlice.base, Word8Vector.toPoly,
            Prim.sendVec, Prim.sendVecTo)
end

type in_flags = {peek: bool, oob: bool}

val no_in_flags = {peek = false, oob = false}
            
fun mk_in_flags {peek, oob} =
   Word.orb (if peek then Word.fromInt Prim.MSG_PEEK else 0wx0,
                Word.orb (if oob then Word.fromInt Prim.MSG_OOB else 0wx0,
                             0wx0))

fun recvArr' (s, sl, in_flags) =
   let
      val (buf, i, sz) = Word8ArraySlice.base sl
   in
      Syscall.simpleResultRestart
      (fn () => Prim.recv (s, Word8Array.toPoly buf, i, C.Size.fromInt sz, 
                           Word.toInt (mk_in_flags in_flags)))
   end

fun getVec (a, n, bytesRead) =
   if n = bytesRead
      then Word8Vector.fromArray a
   else Word8ArraySlice.vector (Word8ArraySlice.slice (a, 0, SOME bytesRead))
      
fun recvVec' (sock, n, in_flags) =
   let
      val a = Word8Array.rawArray n
      val bytesRead =
         recvArr' (sock, Word8ArraySlice.full a, in_flags)
   in
      getVec (a, n, bytesRead)
   end

fun recvArr (sock, sl) = recvArr' (sock, sl, no_in_flags)

fun recvVec (sock, n) = recvVec' (sock, n, no_in_flags)

fun recvArrFrom' (s, sl, in_flags) =
   let
      val (buf, i, sz) = Word8ArraySlice.base sl
      val (sa, salen, finish) = new_sock_addr ()
      val n =
         Syscall.simpleResultRestart
         (fn () => Prim.recvFrom (s, Word8Array.toPoly buf, i, C.Size.fromInt sz,
                                  Word.toInt (mk_in_flags in_flags), 
                                  sa, salen))
   in
      (n, finish ())
   end

fun recvVecFrom' (sock, n, in_flags) =
   let
      val a = Word8Array.fromPoly (Primitive.Array.array n)
      val (bytesRead, sock_addr) =
         recvArrFrom' (sock, Word8ArraySlice.full a, in_flags)
   in
      (getVec (a, n, bytesRead), sock_addr)
   end

fun recvArrFrom (sock, sl) = recvArrFrom' (sock, sl, no_in_flags)

fun recvVecFrom (sock, n) = recvVecFrom' (sock, n, no_in_flags)

fun mk_in_flagsNB z = Word.orb (mk_in_flags z, Word.fromInt Prim.MSG_DONTWAIT)

fun recvArrNB' (s, sl, in_flags) =
   let
      val (buf, i, sz) = Word8ArraySlice.base sl
   in
      nonBlock
      (fn () => Prim.recv (s, Word8Array.toPoly buf, i, C.Size.fromInt sz,
                           Word.toInt (mk_in_flagsNB in_flags)),
       SOME, 
       NONE)
   end

fun recvVecNB' (s, n, in_flags) =
   let
      val a = Word8Array.rawArray n
   in
      nonBlock
      (fn () => Prim.recv (s, Word8Array.toPoly a, 0, C.Size.fromInt n,
                           Word.toInt (mk_in_flagsNB in_flags)),
       fn bytesRead => SOME (getVec (a, n, bytesRead)),
       NONE)
   end

fun recvArrNB (sock, sl) = recvArrNB' (sock, sl, no_in_flags)

fun recvVecNB (sock, n) = recvVecNB' (sock, n, no_in_flags)

fun recvArrFromNB' (s, sl, in_flags) =
   let
      val (buf, i, sz) = Word8ArraySlice.base sl
      val (sa, salen, finish) = new_sock_addr ()
   in
      nonBlock
      (fn () => Prim.recvFrom (s, Word8Array.toPoly buf, i, C.Size.fromInt sz,
                               Word.toInt (mk_in_flagsNB in_flags), sa, salen),
       fn n => SOME (n, finish ()),
       NONE)
   end

fun recvVecFromNB' (s, n, in_flags) =
   let
      val a = Word8Array.fromPoly (Primitive.Array.array n)
      val (sa, salen, finish) = new_sock_addr ()
   in
      nonBlock
      (fn () => Prim.recvFrom (s, Word8Array.toPoly a, 0, C.Size.fromInt n,
                               Word.toInt (mk_in_flagsNB in_flags), sa, salen),
       fn bytesRead => SOME (getVec (a, n, bytesRead), finish ()),
       NONE)
   end

fun recvArrFromNB (sock, sl) = recvArrFromNB' (sock, sl, no_in_flags)

fun recvVecFromNB (sock, n) = recvVecFromNB' (sock, n, no_in_flags)

(* Phantom type. *)
type ('af, 'sock_type) sock = sock

type 'af sock_addr = sock_addr

type 'mode stream = stream
   
end
