(* Copyright (C) 2012,2013,2015,2017 Matthew Fluet.
 * Copyright (C) 2002-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Socket :> SOCKET_EXTRA =
struct

structure Prim = PrimitiveFFI.Socket
structure Error = Posix.Error
structure Syscall = Error.SysCall
structure FileSys = Posix.FileSys

structure Sock = Net.Sock
type sock = Sock.t
val fromRep = Sock.fromRep
val toRep = Sock.toRep
val sockToWord = C_Sock.castToSysWord o Sock.toRep
val wordToSock = Sock.fromRep o C_Sock.castFromSysWord
val sockToFD = PrePosix.FileDesc.fromRep o Sock.toRep
val fdToSock = Sock.fromRep o PrePosix.FileDesc.toRep

type pre_sock_addr = Word8.word array
datatype sock_addr = SA of Word8.word vector
fun unpackSockAddr (SA sa) = sa
fun newSockAddr (): (pre_sock_addr * C_Socklen.t ref * (unit -> sock_addr)) = 
   let
      val salen = C_Size.toInt Prim.sockAddrStorageLen
      val sa = Array.array (salen, 0wx0: Word8.word)
      val salenRef = ref (C_Socklen.fromInt salen)
      fun finish () = 
         SA (ArraySlice.vector 
             (ArraySlice.slice (sa, 0, SOME (C_Socklen.toInt (!salenRef)))))
   in
      (sa, salenRef, finish)
   end
datatype dgram = DGRAM (* phantom *)
datatype stream = MODE (* phantom *)
datatype passive = PASSIVE (* phantom *)
datatype active = ACTIVE (* phantom *)

structure AddrFamily = Net.AddrFamily
structure AF =
   struct
      type addr_family = AddrFamily.t
      val names : (string * addr_family) list = 
         ("UNIX", AddrFamily.fromRep Prim.AF.UNIX) ::
         ("INET", AddrFamily.fromRep Prim.AF.INET) ::
         ("INET6", AddrFamily.fromRep Prim.AF.INET6) ::
         ("UNSPEC", AddrFamily.fromRep Prim.AF.UNSPEC) ::
         nil
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

structure SockType = Net.SockType
structure SOCK =
   struct
      type sock_type = SockType.t
      val toRep = SockType.toRep
      val fromRep = SockType.fromRep
      val stream = SockType.fromRep Prim.SOCK.STREAM
      val dgram = SockType.fromRep Prim.SOCK.DGRAM
      val names : (string * sock_type) list = 
         ("STREAM", stream) ::
         ("DGRAM", dgram) ::
         nil
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
structure SOCKExtra = SOCK

structure CtlExtra =
   struct
      type level = C_Int.t
      type optname = C_Int.t
      type request = C_Int.t

      fun getSockOptC_Int (level: level, optname: optname) s : C_Int.t =
         let
            val optval = ref (C_Int.fromInt 0)
            val () =
               Syscall.simple
               (fn () => Prim.Ctl.getSockOptC_Int (Sock.toRep s, level, optname, optval))
         in
            ! optval
         end
      fun setSockOptC_Int (level: level, optname: optname) (s, optval: C_Int.t) : unit =
         let
            val () =
               Syscall.simple
               (fn () => Prim.Ctl.setSockOptC_Int (Sock.toRep s, level, optname, optval))
         in
            ()
         end

      fun getSockOptBool (level: level, optname: optname) s : bool =
         if getSockOptC_Int (level, optname) s = 0 then false else true
      fun setSockOptBool (level: level, optname: optname) (s, optval: bool) : unit =
         setSockOptC_Int (level, optname) (s, if optval then C_Int.fromInt 1 else C_Int.fromInt 0)
      fun gsSockOptBool (level: level, optname: optname) =
         (getSockOptBool (level, optname), setSockOptBool (level, optname))

      fun getSockOptInt (level: level, optname: optname) s : int =
         C_Int.toInt (getSockOptC_Int (level, optname) s)
      fun setSockOptInt (level: level, optname: optname) (s, optval: int) : unit =
         setSockOptC_Int (level, optname) (s, C_Int.fromInt optval)
      fun gsSockOptInt (level: level, optname: optname) =
         (getSockOptInt (level, optname), setSockOptInt (level, optname))

      fun getSockOptTimeOption (level: level, optname: optname) s : Time.time option =
         let
            val optval_l_onoff = ref (C_Int.fromInt 0)
            val optval_l_linger = ref (C_Int.fromInt 0)
            val () =
               Syscall.simple
               (fn () => Prim.Ctl.getSockOptC_Linger (Sock.toRep s, level, optname,
                                                      optval_l_onoff, optval_l_linger))

         in
            if ! optval_l_onoff = 0
               then NONE
            else SOME (Time.fromSeconds (C_Int.toLarge (! optval_l_linger)))
         end
      fun setSockOptTimeOption (level: level, optname: optname) (s, optval: Time.time option) : unit =
         let
            val (optval_l_onoff, optval_l_linger) =
               case optval of
                  NONE => (C_Int.fromInt 0, C_Int.fromInt 0)
                | SOME t => (C_Int.fromInt 1, C_Int.fromLarge (Time.toSeconds t))
            val () =
               Syscall.simple
               (fn () => Prim.Ctl.setSockOptC_Linger (Sock.toRep s, level, optname,
                                                      optval_l_onoff, optval_l_linger))

         in
            ()
         end

      val (getDEBUG, setDEBUG) = gsSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_DEBUG)
      val (getREUSEADDR, setREUSEADDR) = gsSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_REUSEADDR)
      val (getKEEPALIVE, setKEEPALIVE) = gsSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_KEEPALIVE)
      val (getDONTROUTE, setDONTROUTE) = gsSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_DONTROUTE)
      val getLINGER = getSockOptTimeOption (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_LINGER)
      val setLINGER = setSockOptTimeOption (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_LINGER)
      val (getBROADCAST, setBROADCAST) = gsSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_BROADCAST)
      val (getOOBINLINE, setOOBINLINE) = gsSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_OOBINLINE)
      val (getSNDBUF, setSNDBUF) = gsSockOptInt (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_SNDBUF)
      val (getRCVBUF, setRCVBUF) = gsSockOptInt (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_RCVBUF)
      fun getTYPE s = SOCK.fromRep (getSockOptC_Int (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_TYPE) s)
      fun getERROR s =
         let
            val se = getSockOptC_Int (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_ERROR) s
            val se = PrePosix.SysError.fromRep se
         in
            if PosixError.cleared = se
               then NONE
               else SOME (Error.errorMsg se, SOME se)
         end handle Error.SysErr z => SOME z

      local
         fun getName (s, f: C_Sock.t * pre_sock_addr * C_Socklen.t ref -> C_Int.int C_Errno.t) =
            let
               val (sa, salen, finish) = newSockAddr ()
               val () = Syscall.simple (fn () => f (Sock.toRep s, sa, salen))
            in
               finish ()
            end
      in
         fun getPeerName s = getName (s, Prim.Ctl.getPeerName)
         fun getSockName s = getName (s, Prim.Ctl.getSockName)
      end
      fun getNREAD s =
         let
            val argp = ref (C_Int.fromInt ~1)
            val () = Syscall.simple (fn () => Prim.Ctl.getNREAD (Sock.toRep s, argp))
         in
            C_Int.toInt (!argp)
         end
      fun getATMARK s =
         let
            val argp = ref (C_Int.fromInt ~1)
            val () = Syscall.simple (fn () => Prim.Ctl.getATMARK (Sock.toRep s, argp))
         in
            if C_Int.toInt (!argp) = 0 then false else true
         end
   end

structure Ctl =
   struct
      open CtlExtra

      val getERROR = isSome o CtlExtra.getERROR
   end

fun sameAddr (SA sa1, SA sa2) = sa1 = sa2

fun familyOfAddr (SA sa) = AddrFamily.fromRep (Prim.familyOfAddr sa)

fun bind (s, SA sa) =
   Syscall.simple (fn () => Prim.bind (Sock.toRep s, sa, C_Socklen.fromInt (Vector.length sa)))

fun listen (s, n) = 
   Syscall.simple (fn () => Prim.listen (Sock.toRep s, C_Int.fromInt n))

fun nonBlock' ({restart: bool},
               errVal : ''a, f : unit -> ''a C_Errno.t, post : ''a -> 'b, again, no : 'b) =
   Syscall.syscallErr
   ({clear = false, restart = restart, errVal = errVal}, fn () => 
    {return = f (),
     post = post,
     handlers = [(again, fn () => no)]})

fun nonBlock (errVal, f, post, no) =
   nonBlock' ({restart = true}, errVal, f, post, Error.again, no)

local
   structure PIO = PrimitiveFFI.Posix.IO
   structure OS = Primitive.MLton.Platform.OS
   structure MinGW = PrimitiveFFI.MinGW
   
   fun withNonBlockNormal (s, f: unit -> 'a) =
      let
         val fd = Sock.toRep s
         val flags = 
            Syscall.simpleResultRestart (fn () => PIO.fcntl2 (fd, PIO.F_GETFL))
         val () =
            Syscall.simpleRestart
            (fn () => 
             PIO.fcntl3 (fd, PIO.F_SETFL,
                         C_Int.orb (flags, PrimitiveFFI.Posix.FileSys.O.NONBLOCK)))
      in
         DynamicWind.wind
         (f, fn () =>
          Syscall.simpleRestart (fn () => PIO.fcntl3 (fd, PIO.F_SETFL, flags)))
      end
   
   fun withNonBlockMinGW (s, f: unit -> 'a) =
      let
         val fd = Sock.toRep s
         val () = MinGW.setNonBlock fd
      in
         DynamicWind.wind
         (f, fn () => MinGW.clearNonBlock fd)
      end
in
   val withNonBlock = fn x =>
      case OS.host of
         OS.MinGW => withNonBlockMinGW x
       | _ => withNonBlockNormal x
end

fun connect (s, SA sa) =
   Syscall.simple (fn () => Prim.connect (Sock.toRep s, sa, C_Socklen.fromInt (Vector.length sa)))

fun connectNB (s, SA sa) =
   nonBlock'
   ({restart = false}, C_Int.fromInt ~1, fn () => 
    withNonBlock (s, fn () => Prim.connect (Sock.toRep s, sa, C_Socklen.fromInt (Vector.length sa))),
    fn _ => true,
    Error.inprogress, false)

fun accept s =
   let
      val (sa, salen, finish) = newSockAddr ()
      val s = Syscall.simpleResultRestart (fn () => Prim.accept (Sock.toRep s, sa, salen))
   in
      (Sock.fromRep s, finish ())
   end

fun acceptNB s =
   let
      val (sa, salen, finish) = newSockAddr ()
   in
      nonBlock
      (C_Int.fromInt ~1, 
       fn () => withNonBlock (s, fn () => Prim.accept (Sock.toRep s, sa, salen)),
       fn s => SOME (Sock.fromRep s, finish ()),
       NONE)
   end

fun close s = Syscall.simple (fn () => Prim.close (Sock.toRep s))

datatype shutdown_mode = NO_RECVS | NO_SENDS | NO_RECVS_OR_SENDS

fun shutdownModeToHow m =
   case m of
      NO_RECVS => Prim.SHUT_RD
    | NO_SENDS => Prim.SHUT_WR
    | NO_RECVS_OR_SENDS => Prim.SHUT_RDWR

fun shutdown (s, m) =
   let val m = shutdownModeToHow m
   in Syscall.simple (fn () => Prim.shutdown (Sock.toRep s, m))
   end

type sock_desc = FileSys.file_desc

fun sockDesc sock = sockToFD sock

fun sameDesc (desc1, desc2) = desc1 = desc2

fun select {rds: sock_desc list, 
            wrs: sock_desc list, 
            exs: sock_desc list, 
            timeout: Time.time option} =
   let
      local
         fun mk l =
            let
               val vec = Vector.fromList l
               val arr = Array.array (Vector.length vec, 0: C_Int.t)
            in
               (PrePosix.FileDesc.vectorToRep vec, arr)
            end
      in
         val (read_vec, read_arr) = mk rds
         val (write_vec, write_arr) = mk wrs
         val (except_vec, except_arr) = mk exs
      end
      val setTimeout = 
         case timeout of
            NONE => Prim.setTimeoutNull
          | SOME t => 
               if Time.< (t, Time.zeroTime)
                  then Error.raiseSys Error.inval
               else let
                       val q = LargeInt.quot (Time.toMicroseconds t, 1000000)
                       val q = C_Time.fromLargeInt q
                       val r = LargeInt.rem (Time.toMicroseconds t, 1000000)
                       val r = C_SUSeconds.fromLargeInt r
                    in
                       fn () => Prim.setTimeout (q, r)
                    end handle Overflow => Error.raiseSys Error.inval
      val res = 
         Syscall.simpleResult 
         (fn () =>
          (setTimeout ()
           ; Prim.select (read_vec, write_vec, except_vec,
                          read_arr, write_arr, except_arr)))
      val (rds, wrs, exs) =
         if res = 0
            then ([],[],[])
         else 
            let
               fun mk (l, arr) = 
                  (List.rev o #1)
                  (List.foldl (fn (sd, (l, i)) =>
                               (if Array.sub (arr, i) <> (0: C_Int.t) then sd::l else l, i + 1))
                              ([], 0)
                              l)
            in
               (mk (rds, read_arr),
                mk (wrs, write_arr),
                mk (exs, except_arr))
            end
   in
      {rds = rds,
       wrs = wrs,
       exs = exs}
   end

val ioDesc = FileSys.fdToIOD o sockDesc

type out_flags = {don't_route: bool, oob: bool}

val no_out_flags = {don't_route = false, oob = false}

fun mk_out_flags {don't_route, oob} =
   C_Int.orb (if don't_route then Prim.MSG_DONTROUTE else 0x0,
   C_Int.orb (if oob then Prim.MSG_OOB else 0x0,
              0x0))

local
   fun make (toPoly, base, primSend, primSendTo) =
      let
         val base = fn sl => let val (buf, i, sz) = base sl
                             in (toPoly buf, i, sz)
                             end
         fun send' (s, sl, out_flags) =
            let
               val (buf, i, sz) = base sl
            in
               (C_SSize.toInt o Syscall.simpleResultRestart')
               ({errVal = C_SSize.castFromFixedInt ~1}, fn () => 
                primSend (Sock.toRep s, buf, C_Int.fromInt i, C_Size.fromInt sz, 
                          mk_out_flags out_flags))
            end
         fun send (sock, buf) = send' (sock, buf, no_out_flags)
         fun sendNB' (s, sl, out_flags) =
            let
               val (buf, i, sz) = base sl
            in
               nonBlock
               (C_SSize.castFromFixedInt ~1,
                fn () =>
                primSend (Sock.toRep s, buf, C_Int.fromInt i, C_Size.fromInt sz,
                          C_Int.orb (Prim.MSG_DONTWAIT, mk_out_flags out_flags)),
                SOME o C_SSize.toInt, 
                NONE)
            end
         fun sendNB (sock, sl) = sendNB' (sock, sl, no_out_flags)
         fun sendTo' (s, SA sa, sl, out_flags) =
            let
               val (buf, i, sz) = base sl
            in
               Syscall.simpleRestart'
               ({errVal = C_SSize.castFromFixedInt ~1}, fn () => 
                primSendTo (Sock.toRep s, buf, C_Int.fromInt i, C_Size.fromInt sz,
                            mk_out_flags out_flags, 
                            sa, C_Socklen.fromInt (Vector.length sa)))
            end
         fun sendTo (sock, sock_addr, sl) =
            sendTo' (sock, sock_addr, sl, no_out_flags)
         fun sendToNB' (s, SA sa, sl, out_flags) =
            let
               val (buf, i, sz) = base sl
            in
               nonBlock 
               (C_SSize.castFromFixedInt ~1,
                fn () =>
                primSendTo (Sock.toRep s, buf, C_Int.fromInt i, C_Size.fromInt sz,
                            C_Int.orb (Prim.MSG_DONTWAIT, mk_out_flags out_flags),
                            sa, C_Socklen.fromInt (Vector.length sa)),
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
      make (Word8Array.toPoly, Word8ArraySlice.base, Prim.sendArr, Prim.sendArrTo)
   val (sendVec, sendVec', sendVecNB, sendVecNB',
        sendVecTo, sendVecTo', sendVecToNB, sendVecToNB') =
      make (Word8Vector.toPoly, Word8VectorSlice.base, Prim.sendVec, Prim.sendVecTo)
end

type in_flags = {peek: bool, oob: bool}

val no_in_flags = {peek = false, oob = false}

fun mk_in_flags {peek, oob} =
   C_Int.orb (if peek then Prim.MSG_PEEK else 0x0,
   C_Int.orb (if oob then Prim.MSG_OOB else 0x0,
              0x0))

fun recvArr' (s, sl, in_flags) =
   let
      val (buf, i, sz) = Word8ArraySlice.base sl
   in
      (C_SSize.toInt o Syscall.simpleResultRestart')
      ({errVal = C_SSize.castFromFixedInt ~1}, fn () => 
       Prim.recv (Sock.toRep s, Word8Array.toPoly buf, C_Int.fromInt i, C_Size.fromInt sz, 
                  mk_in_flags in_flags))
   end

fun getVec (a, n, bytesRead) =
   if n = bytesRead
      then Word8Vector.unsafeFromArray a
   else Word8ArraySlice.vector (Word8ArraySlice.slice (a, 0, SOME bytesRead))

fun recvVec' (sock, n, in_flags) =
   let
      val a = Word8Array.alloc n
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
      val (sa, salen, finish) = newSockAddr ()
      val n =
         (C_SSize.toInt o Syscall.simpleResultRestart')
         ({errVal = C_SSize.castFromFixedInt ~1}, fn () => 
          Prim.recvFrom (Sock.toRep s, Word8Array.toPoly buf, C_Int.fromInt i, C_Size.fromInt sz,
                         mk_in_flags in_flags, 
                         sa, salen))
   in
      (n, finish ())
   end

fun recvVecFrom' (sock, n, in_flags) =
   let
      val a = Word8Array.alloc n
      val (bytesRead, sock_addr) =
         recvArrFrom' (sock, Word8ArraySlice.full a, in_flags)
   in
      (getVec (a, n, bytesRead), sock_addr)
   end

fun recvArrFrom (sock, sl) = recvArrFrom' (sock, sl, no_in_flags)

fun recvVecFrom (sock, n) = recvVecFrom' (sock, n, no_in_flags)

fun mk_in_flagsNB in_flags = C_Int.orb (mk_in_flags in_flags, Prim.MSG_DONTWAIT)

fun recvArrNB' (s, sl, in_flags) =
   let
      val (buf, i, sz) = Word8ArraySlice.base sl
   in
      nonBlock
      (C_SSize.castFromFixedInt ~1,
       fn () => Prim.recv (Sock.toRep s, Word8Array.toPoly buf, C_Int.fromInt i, C_Size.fromInt sz,
                           mk_in_flagsNB in_flags),
       SOME o C_SSize.toInt, 
       NONE)
   end

fun recvVecNB' (s, n, in_flags) =
   let
      val a = Word8Array.alloc n
   in
      nonBlock
      (C_SSize.castFromFixedInt ~1,
       fn () => Prim.recv (Sock.toRep s, Word8Array.toPoly a, 0, C_Size.fromInt n,
                           mk_in_flagsNB in_flags),
       fn bytesRead => SOME (getVec (a, n, C_SSize.toInt bytesRead)),
       NONE)
   end

fun recvArrNB (sock, sl) = recvArrNB' (sock, sl, no_in_flags)

fun recvVecNB (sock, n) = recvVecNB' (sock, n, no_in_flags)

fun recvArrFromNB' (s, sl, in_flags) =
   let
      val (buf, i, sz) = Word8ArraySlice.base sl
      val (sa, salen, finish) = newSockAddr ()
   in
      nonBlock
      (C_SSize.castFromFixedInt ~1,
       fn () => Prim.recvFrom (Sock.toRep s, Word8Array.toPoly buf, C_Int.fromInt i, C_Size.fromInt sz,
                               mk_in_flagsNB in_flags, sa, salen),
       fn n => SOME (C_SSize.toInt n, finish ()),
       NONE)
   end

fun recvVecFromNB' (s, n, in_flags) =
   let
      val a = Word8Array.alloc n
      val (sa, salen, finish) = newSockAddr ()
   in
      nonBlock
      (C_SSize.castFromFixedInt ~1,
       fn () => Prim.recvFrom (Sock.toRep s, Word8Array.toPoly a, 0, C_Size.fromInt n,
                               mk_in_flagsNB in_flags, sa, salen),
       fn bytesRead => SOME (getVec (a, n, C_SSize.toInt bytesRead), finish ()),
       NONE)
   end

fun recvArrFromNB (sock, sl) = recvArrFromNB' (sock, sl, no_in_flags)

fun recvVecFromNB (sock, n) = recvVecFromNB' (sock, n, no_in_flags)

(* Phantom type. *)
type ('af, 'sock_type) sock = sock

type 'af sock_addr = sock_addr

type 'mode stream = stream

end
