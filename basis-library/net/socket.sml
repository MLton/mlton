(* Copyright (C) 2002-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Socket : SOCKET_EXTRA =
struct

structure Prim = PrimitiveFFI.Socket
structure Error = Posix.Error
structure Syscall = Error.SysCall
structure FileSys = Posix.FileSys

type sock = C_Sock.t
val sockToWord = C_Sock.castToSysWord
val wordToSock = C_Sock.castFromSysWord
val sockToFD = fn x => x
val fdToSock = fn x => x

type pre_sock_addr = Word8.word array
datatype sock_addr = SA of Word8.word vector
fun unpackSockAddr (SA sa) = sa
fun new_sock_addr (): (pre_sock_addr * C_Socklen.t ref * (unit -> sock_addr)) = 
   let
      val salen = C_Size.toInt Prim.sockAddrStorageLen
      val sa = Array.array (salen, 0wx0)
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

structure AF =
   struct
      type addr_family = NetHostDB.addr_family
      val names : (string * addr_family) list = 
         ("UNIX", Prim.AF.UNIX) ::
         ("INET", Prim.AF.INET) ::
         ("INET6", Prim.AF.INET6) ::
         ("UNSPEC", Prim.AF.UNSPEC) ::
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

structure SOCK =
   struct
      type sock_type = C_Int.t
      val stream = Prim.SOCK.STREAM
      val dgram = Prim.SOCK.DGRAM
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

structure CtlExtra =
   struct
      type level = C_Int.t
      type optname = C_Int.t
      type request = C_Int.t

      (* host byte order *)
      type optvalVec = Word8.word vector
      type optvalArr = Word8.word array

      val bitsPerByte = 8

      val isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian
      val intLen = Int.quot (C_Int.precision', bitsPerByte)
      fun unmarshalInt (wa: optvalArr) : C_Int.int =
         let
            fun loop (i, acc) =
               if i >= intLen
                  then acc
                  else let
                          val w =
                             Array.sub 
                             (wa, if isBigEndian
                                     then i
                                     else (intLen - 1) - i)
                          val w = C_Int.castFromSysWord (Word8.castToSysWord w)
                       in
                          loop (i + 1, C_Int.orb (w, C_Int.<< (acc, 0w4)))
                       end
         in
            loop (0, 0)
         end
      fun marshalInt (i: C_Int.int) : optvalVec =
         let
            val wa = Array.array (intLen, 0wx0)
            fun loop (i, acc) =
               if i >= intLen
                  then ()
                  else let
                          val w = Word8.castFromSysWord (C_Int.castToSysWord acc)
                          val () =
                             Array.update
                             (wa, if isBigEndian
                                     then (intLen - 1) - i
                                     else i, w)
                       in
                          loop (i + 1, C_Int.>> (acc, 0w4))
                       end
         in
            loop (0, i)
            ; Array.vector wa
         end
      val boolLen = intLen
      fun unmarshalBool (wa: optvalArr) : bool =
         if (unmarshalInt wa) = 0 then false else true
      fun marshalBool (b: bool) : optvalVec = 
         marshalInt (if b then 1 else 0)
      val sizeLen = Int.quot (C_Size.wordSize, bitsPerByte)
      fun unmarshalSize (wa: optvalArr) : int =
         let
            fun loop (i, acc) =
               if i >= sizeLen
                  then acc
                  else let
                          val w =
                             Array.sub 
                             (wa, if isBigEndian
                                     then i
                                     else (sizeLen - 1) - i)
                          val w = C_Size.castFromSysWord (Word8.castToSysWord w)
                       in
                          loop (i + 1, C_Size.orb (w, C_Size.<< (acc, 0w4)))
                       end
         in
            C_Size.toInt (loop (0, 0wx0))
         end
      fun marshalSize (i: int) : optvalVec =
         let
            val wa = Array.array (sizeLen, 0wx0)
            fun loop (i, acc) =
               if i >= sizeLen
                  then ()
                  else let
                          val w = Word8.castFromSysWord (C_Size.castToSysWord acc)
                          val () =
                             Array.update
                             (wa, if isBigEndian
                                     then (sizeLen - 1) - i
                                     else i, w)
                       in
                          loop (i + 1, C_Size.>> (acc, 0w4))
                       end
         in
            loop (0, C_Size.fromInt i)
            ; Array.vector wa
         end
      (* Assume 'struct linger' has no padding. *)
      val optTimeLen: int = intLen + intLen
      fun unmarshalOptTime (wa: optvalArr) : Time.time option =
         let
            fun loopBool (i, acc) =
               if i >= intLen
                  then acc
                  else let
                          val w =
                             Array.sub 
                             (wa, if isBigEndian
                                     then i
                                     else (intLen - 1) - i)
                          val w = C_Int.castFromSysWord (Word8.castToSysWord w)
                       in
                          loopBool (i + 1, C_Int.orb (w, C_Int.<< (acc, 0w4)))
                       end
            fun loopInt (i, acc) =
               if i >= intLen
                  then acc
                  else let
                          val w =
                             Array.sub 
                             (wa, intLen + (if isBigEndian
                                               then i
                                               else (intLen - 1) - i))
                          val w = C_Int.castFromSysWord (Word8.castToSysWord w)
                       in
                          loopInt (i + 1, C_Int.orb (w, C_Int.<< (acc, 0w4)))
                       end
         in
            if loopBool (0, 0) = 0
               then NONE
               else SOME (Time.fromSeconds (C_Int.toLarge (loopInt (0, 0))))
         end
      fun marshalOptTime (to: Time.time option) : optvalVec =
         let
            val wa = Array.array (optTimeLen, 0wx0)
            fun loopBool (i, acc) =
               if i >= intLen
                  then ()
                  else let
                          val w = Word8.castFromSysWord (C_Int.castToSysWord acc)
                          val () =
                             Array.update
                             (wa, if isBigEndian
                                     then (intLen - 1) - i
                                     else i, w)
                       in
                          loopBool (i + 1, C_Int.>> (acc, 0w4))
                       end
            fun loopInt (i, acc) =
               if i >= intLen
                  then ()
                  else let
                          val w = Word8.castFromSysWord (C_Int.castToSysWord acc)
                          val () =
                             Array.update
                             (wa, intLen + (if isBigEndian
                                               then (intLen - 1) - i
                                               else i), w)
                       in
                          loopInt (i + 1, C_Int.>> (acc, 0w4))
                       end
         in
            case to of
               NONE => (loopBool (0, 0); loopInt (0, 0))
             | SOME t => (loopBool (0, 1); loopInt (0, C_Int.fromLarge (Time.toSeconds t)))
            ; Array.vector wa
         end

      local
         fun make (optlen: int,
                   marshal: 'a -> optvalVec,
                   unmarshal: optvalArr -> 'a) =
            let
               fun getSockOpt (level: level, optname: optname) s : 'a =
                  let
                     val optval = Array.array (optlen, 0wx0)
                     val optlen' = ref (C_Socklen.fromInt optlen)
                     val () = 
                        Syscall.simple
                        (fn () =>
                         Prim.Ctl.getSockOpt (s, level, optname, optval, optlen'))
                     val () =
                        if C_Socklen.toInt (!optlen') <> optlen
                           then raise (Fail "Socket.Ctl.getSockOpt: optlen' <> optlen")
                           else ()
                  in
                     unmarshal optval
                  end
               fun setSockOpt (level: level, optname: optname) (s, optval: 'a) : unit =
                  let
                     val optval = marshal optval
                     val optlen' = C_Socklen.fromInt optlen
                     val () =
                        Syscall.simple
                        (fn () =>
                         Prim.Ctl.setSockOpt (s, level, optname, optval, optlen'))
                  in
                     ()
                  end
               fun getIOCtl (request: request) s : 'a =
                  let
                     val optval = Array.array (optlen, 0wx0)
                     val () =
                        Syscall.simple
                        (fn () =>
                         Prim.Ctl.getIOCtl (s, request, optval))
                  in
                     unmarshal optval
                  end
               fun setIOCtl (request: request) (s, optval: 'a) : unit =
                  let
                     val optval = marshal optval
                     val () =
                        Syscall.simple
                        (fn () =>
                         Prim.Ctl.setIOCtl (s, request, optval))
                  in
                     ()
                  end
            in
               (getSockOpt, getIOCtl, setSockOpt, setIOCtl)
            end
      in
         val (getSockOptInt, getIOCtlInt, setSockOptInt, _) =
            make (intLen, marshalInt, unmarshalInt)
         val (getSockOptBool, getIOCtlBool, setSockOptBool, _) =
            make (boolLen, marshalBool, unmarshalBool)
         val (getSockOptSize, getIOCtlSize, setSockOptSize, _) =
            make (sizeLen, marshalSize, unmarshalSize)
         val (getSockOptOptTime, _, setSockOptOptTime, _) =
            make (optTimeLen, marshalOptTime, unmarshalOptTime)
      end

      val getDEBUG = getSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_DEBUG)
      val setDEBUG = setSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_DEBUG)
      val getREUSEADDR = getSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_REUSEADDR)
      val setREUSEADDR = setSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_REUSEADDR)
      val getKEEPALIVE = getSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_KEEPALIVE)
      val setKEEPALIVE = setSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_KEEPALIVE)
      val getDONTROUTE = getSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_DONTROUTE)
      val setDONTROUTE = setSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_DONTROUTE)
      val getLINGER = getSockOptOptTime (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_LINGER)
      val setLINGER = setSockOptOptTime (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_LINGER)
      val getBROADCAST = getSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_BROADCAST)
      val setBROADCAST = setSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_BROADCAST)
      val getOOBINLINE = getSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_OOBINLINE)
      val setOOBINLINE = setSockOptBool (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_OOBINLINE)
      val getSNDBUF = getSockOptSize (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_SNDBUF)
      val setSNDBUF = setSockOptSize (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_SNDBUF)
      val getRCVBUF = getSockOptSize (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_RCVBUF)
      val setRCVBUF = setSockOptSize (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_RCVBUF)
      fun getTYPE s = getSockOptInt (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_TYPE) s
      fun getERROR s =
         let
            val se = getSockOptInt (Prim.Ctl.SOL_SOCKET, Prim.Ctl.SO_ERROR) s
         in
            if 0 = se
               then NONE
               else SOME (Error.errorMsg se, SOME se)
         end handle Error.SysErr z => SOME z
      local
         fun getName (s, f: sock * pre_sock_addr * C_Socklen.t ref -> C_Int.int C_Errno.t) =
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
      val getNREAD = getIOCtlSize Prim.Ctl.FIONREAD
      val getATMARK = getIOCtlBool Prim.Ctl.SIOCATMARK
   end

structure Ctl =
   struct
      open CtlExtra

      val getERROR = isSome o CtlExtra.getERROR
   end

fun sameAddr (SA sa1, SA sa2) = sa1 = sa2

fun familyOfAddr (SA sa) = Prim.familyOfAddr sa

fun bind (s, SA sa) =
   Syscall.simple (fn () => Prim.bind (s, sa, C_Socklen.fromInt (Vector.length sa)))

fun listen (s, n) = 
   Syscall.simple (fn () => Prim.listen (s, C_Int.fromInt n))

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
in
   fun withNonBlock (s, f: unit -> 'a) =
      let
         val fd = s
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
end

fun connect (s, SA sa) =
   Syscall.simple (fn () => Prim.connect (s, sa, C_Socklen.fromInt (Vector.length sa)))

fun connectNB (s, SA sa) =
   nonBlock'
   ({restart = false}, C_Int.fromInt ~1, fn () => 
    withNonBlock (s, fn () => Prim.connect (s, sa, C_Socklen.fromInt (Vector.length sa))),
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
      (C_Int.fromInt ~1, 
       fn () => withNonBlock (s, fn () => Prim.accept (s, sa, salen)),
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
               val arr = Array.array (Vector.length vec, 0)
            in
               (vec, arr)
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
                               (if Array.sub (arr, i) <> 0 then sd::l else l, i + 1))
                              ([],0) 
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
   fun make (base, primSend, primSendTo) =
      let
         val base = fn sl => let val (buf, i, sz) = base sl
                             in (buf, i, sz)
                             end
         fun send' (s, sl, out_flags) =
            let
               val (buf, i, sz) = base sl
            in
               (C_SSize.toInt o Syscall.simpleResultRestart')
               ({errVal = C_SSize.castFromFixedInt ~1}, fn () => 
                primSend (s, buf, C_Int.fromInt i, C_Size.fromInt sz, 
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
                primSend (s, buf, C_Int.fromInt i, C_Size.fromInt sz,
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
                primSendTo (s, buf, C_Int.fromInt i, C_Size.fromInt sz,
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
                primSendTo (s, buf, C_Int.fromInt i, C_Size.fromInt sz,
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
      make (Word8ArraySlice.base, Prim.sendArr, Prim.sendArrTo)
   val (sendVec, sendVec', sendVecNB, sendVecNB',
        sendVecTo, sendVecTo', sendVecToNB, sendVecToNB') =
      make (Word8VectorSlice.base, Prim.sendVec, Prim.sendVecTo)
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
       Prim.recv (s, Word8Array.toPoly buf, C_Int.fromInt i, C_Size.fromInt sz, 
                  mk_in_flags in_flags))
   end

fun getVec (a, n, bytesRead) =
   if n = bytesRead
      then Word8Vector.unsafeFromArray a
   else Word8ArraySlice.vector (Word8ArraySlice.slice (a, 0, SOME bytesRead))

fun recvVec' (sock, n, in_flags) =
   let
      val a = Word8Array.arrayUninit n
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
         (C_SSize.toInt o Syscall.simpleResultRestart')
         ({errVal = C_SSize.castFromFixedInt ~1}, fn () => 
          Prim.recvFrom (s, Word8Array.toPoly buf, C_Int.fromInt i, C_Size.fromInt sz,
                         mk_in_flags in_flags, 
                         sa, salen))
   in
      (n, finish ())
   end

fun recvVecFrom' (sock, n, in_flags) =
   let
      val a = Word8Array.arrayUninit n
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
       fn () => Prim.recv (s, Word8Array.toPoly buf, C_Int.fromInt i, C_Size.fromInt sz,
                           mk_in_flagsNB in_flags),
       SOME o C_SSize.toInt, 
       NONE)
   end

fun recvVecNB' (s, n, in_flags) =
   let
      val a = Word8Array.arrayUninit n
   in
      nonBlock
      (C_SSize.castFromFixedInt ~1,
       fn () => Prim.recv (s, Word8Array.toPoly a, 0, C_Size.fromInt n,
                           mk_in_flagsNB in_flags),
       fn bytesRead => SOME (getVec (a, n, C_SSize.toInt bytesRead)),
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
      (C_SSize.castFromFixedInt ~1,
       fn () => Prim.recvFrom (s, Word8Array.toPoly buf, C_Int.fromInt i, C_Size.fromInt sz,
                               mk_in_flagsNB in_flags, sa, salen),
       fn n => SOME (C_SSize.toInt n, finish ()),
       NONE)
   end

fun recvVecFromNB' (s, n, in_flags) =
   let
      val a = Word8Array.arrayUninit n
      val (sa, salen, finish) = new_sock_addr ()
   in
      nonBlock
      (C_SSize.castFromFixedInt ~1,
       fn () => Prim.recvFrom (s, Word8Array.toPoly a, 0, C_Size.fromInt n,
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
