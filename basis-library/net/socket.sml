structure Socket:> SOCKET_EXTRA
   where type SOCK.sock_type = Primitive.Socket.SOCK.sock_type
   where type pre_sock_addr = Word8.word array
=
struct

structure Prim = Primitive.Socket
structure PE = Posix.Error
structure PFS = Posix.FileSys

datatype sock = S of Prim.sock
fun sockToWord (S s) = SysWord.fromInt s
fun wordToSock s = S (SysWord.toInt s)
fun sockToFD sock = PFS.wordToFD (sockToWord sock)
fun fdToSock fd = wordToSock (PFS.fdToWord fd)

type pre_sock_addr = Prim.pre_sock_addr
datatype sock_addr = SA of Prim.sock_addr
fun unpackSockAddr (SA sa) = Word8Vector.fromPoly sa
fun new_sock_addr (): (pre_sock_addr * int ref * (unit -> sock_addr)) = 
   let
      val sa = Array.array (Prim.sockAddrLenMax, 0wx0)
      val salen = ref (Array.length sa)
      fun finish () =
	 SA (ArraySlice.vector (ArraySlice.slice (sa, 0, SOME (!salen))))
   in
      (sa, salen, finish)
   end
datatype dgram = DGRAM
datatype stream = MODE
datatype passive = PASSIVE
datatype active = ACTIVE

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
      type sock_type = Prim.SOCK.sock_type
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
      type level = Prim.Ctl.level
      type optname = Prim.Ctl.optname
      type request = Prim.Ctl.request
      (* host byte order (LSB) *)
      type read_data = Prim.Ctl.read_data
      type write_data = Prim.Ctl.write_data
      structure PW = PackWord32Little

      val wordLen = PW.bytesPerElem
      fun unmarshalWord (wa, l, s): word = 
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
			(unmarshalInt (wa, l, s + boolLen))))
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
		     ; marshalInt (0, wa, s + boolLen))
	  | SOME t => (marshalBool (true, wa, s)
		       ; marshalWord (Word.fromLargeInt (Time.toSeconds t), 
				      wa, s + boolLen))

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
	       fun getSockOpt (level: level, optname: optname) (S s) =
		  let
		     val optval = Word8Array.array (optlen, 0wx0)
		     val optlen = ref optlen
		  in
		     PE.checkResult
		     (Prim.Ctl.getSockOpt (s, level, optname,
					   Word8Array.toPoly optval,
					   optlen))
		     ; unmarshal (optval, !optlen, 0)
		  end
	       fun setSockOpt (level: level, optname: optname) (S s, optval) =
		  let
		     val optval = marshal optval
		     val optlen = Word8Vector.length optval
		  in
		     PE.checkResult
		     (Prim.Ctl.setSockOpt (s, level, optname,
					   Word8Vector.toPoly optval,
					   optlen))
		  end
	       fun getIOCtl (request: request) (S s): 'a =
		  let
		     val optval = Word8Array.array (optlen, 0wx0)
		  in
		     PE.checkResult (Prim.Ctl.getIOCtl
				     (s, request, Word8Array.toPoly optval))
		     ; unmarshal (optval, optlen, 0)
		  end
	       fun setIOCtl (request: request) (S s, optval: 'a): unit =
		  let
		     val optval = marshal optval
		     val optlen = Word8Vector.length optval
		  in
		     PE.checkResult (Prim.Ctl.setIOCtl
				     (s, request, Word8Vector.toPoly optval))
		  end
	    in
	       (getSockOpt, getIOCtl, setSockOpt, setIOCtl)
	    end
      in
	 val (getSockOptWord, getIOCtlWord, setSockOptWord, setIOCtlWord) =
	    make (wordLen, marshalWord, unmarshalWord)
	 val (getSockOptInt, getIOCtlInt, setSockOptInt, setIOCtlInt) =
	    make (intLen, marshalInt, unmarshalInt)
	 val (getSockOptBool, getIOCtlBool, setSockOptBool, setIOCtlBool) =
	    make (boolLen, marshalBool, unmarshalBool)
	 val (getSockOptTimeOpt, getIOCtlTimeOpt, setSockOptTimeOpt,
	      setIOCtlTimeOpt) =
	    make (timeOptLen, marshalTimeOpt, unmarshalTimeOpt)
      end
   end

structure Ctl =
   struct
      open CtlExtra

      val getDEBUG = getSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.DEBUG)
      val setDEBUG = setSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.DEBUG)
      val getREUSEADDR = getSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.REUSEADDR)
      val setREUSEADDR = setSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.REUSEADDR)
      val getKEEPALIVE = getSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.KEEPALIVE)
      val setKEEPALIVE = setSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.KEEPALIVE)
      val getDONTROUTE = getSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.DONTROUTE)
      val setDONTROUTE = setSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.DONTROUTE)
      val getBROADCAST = getSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.BROADCAST)
      val getLINGER = getSockOptTimeOpt (Prim.Ctl.SOCKET, Prim.Ctl.LINGER)
      val setLINGER = setSockOptTimeOpt (Prim.Ctl.SOCKET, Prim.Ctl.LINGER)
      val setBROADCAST = setSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.BROADCAST)
      val getOOBINLINE = getSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.OOBINLINE)
      val setOOBINLINE = setSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.OOBINLINE)
      val getSNDBUF = getSockOptInt (Prim.Ctl.SOCKET, Prim.Ctl.SNDBUF)
      val setSNDBUF = setSockOptInt (Prim.Ctl.SOCKET, Prim.Ctl.SNDBUF)
      val getRCVBUF = getSockOptInt (Prim.Ctl.SOCKET, Prim.Ctl.RCVBUF)
      val setRCVBUF = setSockOptInt (Prim.Ctl.SOCKET, Prim.Ctl.RCVBUF)
      fun getTYPE s =
	 Prim.SOCK.fromInt (getSockOptInt (Prim.Ctl.SOCKET, Prim.Ctl.TYPE) s)
      val getERROR = getSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.ERROR)
      local
	 fun getName 
	    (f: Prim.sock * pre_sock_addr * int ref -> int)
	    (S s) =
	    let
	       val (sa, salen, finish) = new_sock_addr ()
	       val _ = PE.checkResult (f (s, sa, salen))
	    in
	       finish ()
	    end
      in
	 fun getPeerName sock = getName Prim.Ctl.getPeerName sock
	 fun getSockName sock = getName Prim.Ctl.getSockName sock
      end
      val getNREAD = getIOCtlInt Prim.Ctl.NREAD
      val getATMARK = getIOCtlBool Prim.Ctl.ATMARK
   end

fun sameAddr (SA sa1, SA sa2) = sa1 = sa2

fun familyOfAddr (SA sa) = NetHostDB.intToAddrFamily (Prim.familyOfAddr sa)

fun bind (S s, SA sa) =
   PE.checkResult (Prim.bind (s, sa, Vector.length sa))

fun listen (S s, n) = PE.checkResult (Prim.listen (s, n))

fun nonBlock' (res: int, again, no, f) =
   if ~1 = res
      then
	 let
	    val e = PE.getErrno ()
	 in
	    if e = again
	       then no
	    else PE.raiseSys e
	 end
   else f res

fun nonBlock (res, no, f) = nonBlock' (res, PE.again, no, f)
   
local
   structure PIO = PosixPrimitive.IO
in
   fun withNonBlock (fd, f: unit -> 'a) =
      let
	 val flags = PIO.fcntl2 (fd, PIO.F_GETFL)
	 val _ = PIO.fcntl3 (fd, PIO.F_SETFL,
			     Word.toIntX
			     (Word.orb (Word.fromInt flags,
					PosixPrimitive.FileSys.O.nonblock)))
      in
	 DynamicWind.wind (f, fn () => (PIO.fcntl3 (fd, PIO.F_SETFL, flags)
					; ()))
      end
end

fun connect (S s, SA sa) =
   PE.checkResult (Prim.connect (s, sa, Vector.length sa))

fun connectNB (S s, SA sa) =
   nonBlock' (withNonBlock (s, fn () => Prim.connect (s, sa, Vector.length sa)),
	      PE.inprogress,
	      false,
	      fn _ => true)

fun accept (S s) =
   let
      val (sa, salen, finish) = new_sock_addr ()
      val s = PE.checkReturnResult (Prim.accept (s, sa, salen))
   in
      (S s, finish ())
   end

fun acceptNB (S s) =
   let
      val (sa, salen, finish) = new_sock_addr ()
   in
      nonBlock (withNonBlock (s, fn () => Prim.accept (s, sa, salen)),
		NONE,
		fn s => SOME (S s, finish ()))
   end

fun close (S s) = PE.checkResult (Prim.close (s))

datatype shutdown_mode = NO_RECVS | NO_SENDS | NO_RECVS_OR_SENDS

fun shutdownModeToHow m =
   case m of
      NO_RECVS => Prim.SHUT_RD
    | NO_SENDS => Prim.SHUT_WR
    | NO_RECVS_OR_SENDS => Prim.SHUT_RDWR

fun shutdown (S s, m) =
   PE.checkResult
   (Prim.shutdown (s, shutdownModeToHow m))

type sock_desc = OS.IO.iodesc

fun sockDesc sock = PFS.fdToIOD (sockToFD sock)

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
 
type 'a buf = {buf: 'a, i: int, sz: int option}

type out_flags = {don't_route: bool, oob: bool}

fun mk_out_flags {don't_route, oob} =
   Word.orb (if don't_route then Prim.MSG_DONTROUTE else 0wx0,
		Word.orb (if oob then Prim.MSG_OOB else 0wx0,
			     0wx0))
val no_out_flags = {don't_route = false, oob = false}

local
   fun make (base, toPoly, primSend, primSendTo) =
      let
	 val base = fn sl => let
				val (buf, i, sz) = base sl
			     in
				(toPoly buf, i, sz)
			     end
	 fun send' (S s, sl, out_flags) =
	    let
	       val (buf, i, sz) = base sl
	    in
	       PE.checkReturnResult
	       (primSend (s, buf, i, sz, mk_out_flags out_flags))
	    end
	 fun send (sock, buf) = send' (sock, buf, no_out_flags)
	 fun sendNB' (S s, sl, out_flags) =
	    let
	       val (buf, i, sz) = base sl
	       val res =
		  primSend (s, buf, i, sz,
			    Word.orb (Prim.MSG_DONTWAIT, mk_out_flags out_flags))
	    in
	       nonBlock (res, NONE, SOME)
	    end
	 fun sendNB (sock, sl) = sendNB' (sock, sl, no_out_flags)
	 fun sendTo' (S s, SA sa, sl, out_flags) =
	    let
	       val (buf, i, sz) = base sl
	    in
	       PE.checkResult
	       (primSendTo (s, buf, i, sz, mk_out_flags out_flags, sa,
			    Vector.length sa))
	    end
	 fun sendTo (sock, sock_addr, sl) =
	    sendTo' (sock, sock_addr, sl, no_out_flags)
	 fun sendToNB' (S s, SA sa, sl, out_flags) =
	    let
	       val (buf, i, sz) = base sl
	    in
	       nonBlock (primSendTo (s, buf, i, sz,
				     Word.orb (Prim.MSG_DONTWAIT,
					       mk_out_flags out_flags),
				     sa, Vector.length sa),
			 false,
			 fn _ => true)
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
	    Prim.sendArr, Prim.sendToArr)
   val (sendVec, sendVec', sendVecNB, sendVecNB',
	sendVecTo, sendVecTo', sendVecToNB, sendVecToNB') =
      make (Word8VectorSlice.base, Word8Vector.toPoly,
	    Prim.sendVec, Prim.sendToVec)
end

type in_flags = {peek: bool, oob: bool}

val no_in_flags = {peek = false, oob = false}
	    
fun mk_in_flags {peek, oob} =
   Word.orb (if peek then Prim.MSG_PEEK else 0wx0,
		Word.orb (if oob then Prim.MSG_OOB else 0wx0,
			     0wx0))

fun recvArr' (S s, sl, in_flags) =
   let
      val (buf, i, sz) = Word8ArraySlice.base sl
   in
      PE.checkReturnResult
      (Prim.recv (s, Word8Array.toPoly buf, i, sz, mk_in_flags in_flags))
   end

fun recvVec' (sock, n, in_flags) =
   let
      val a = Word8Array.rawArray n
      val bytesRead =
	 recvArr' (sock, Word8ArraySlice.full a, in_flags)
   in
      if n = bytesRead
	 then Word8Vector.fromArray a
      else Word8Array.extract (a, 0, SOME bytesRead)
   end

fun recvArr (sock, sl) = recvArr' (sock, sl, no_in_flags)

fun recvVec (sock, n) = recvVec' (sock, n, no_in_flags)

fun recvArrFrom' (S s, sl, in_flags) =
   let
      val (buf, i, sz) = Word8ArraySlice.base sl
      val (sa, salen, finish) = new_sock_addr ()
      val n =
	 PE.checkReturnResult
	 (Prim.recvFrom
	  (s, Word8Array.toPoly buf, i, sz, mk_in_flags in_flags, sa, salen))
   in
      (n, finish ())
   end

fun recvVecFrom' (sock, n, in_flags) =
   let
      val a = Word8Array.fromPoly (Primitive.Array.array n)
      val (bytesRead, sock_addr) =
	 recvArrFrom' (sock, Word8ArraySlice.full a, in_flags)
   in
      (if n = bytesRead
	  then Word8Vector.fromArray a
       else Word8Array.extract (a, 0, SOME bytesRead),
	  sock_addr)
   end

fun recvArrFrom (sock, sl) = recvArrFrom' (sock, sl, no_in_flags)

fun recvVecFrom (sock, n) = recvVecFrom' (sock, n, no_in_flags)

fun mk_in_flagsNB z = Word.orb (mk_in_flags z, Prim.MSG_DONTWAIT)

fun recvArrNB' (S s, sl, in_flags) =
   let
      val (buf, i, sz) = Word8ArraySlice.base sl
   in
      nonBlock (Prim.recv (s, Word8Array.toPoly buf, i, sz,
			   mk_in_flagsNB in_flags),
		NONE,
		SOME)
		      
   end

fun recvVecNB' (S s, n, in_flags) =
   let
      val a = Word8Array.rawArray n
   in
      nonBlock (Prim.recv (s, Word8Array.toPoly a, 0, n, mk_in_flagsNB in_flags),
		NONE,
		fn bytesRead =>
		SOME (if n = bytesRead
			 then Word8Vector.fromArray a
		      else Word8Array.extract (a, 0, SOME bytesRead)))
		      
   end

fun recvArrNB (sock, sl) = recvArrNB' (sock, sl, no_in_flags)

fun recvVecNB (sock, n) = recvVecNB' (sock, n, no_in_flags)

fun recvArrFromNB' (S s, sl, in_flags) =
   let
      val (buf, i, sz) = Word8ArraySlice.base sl
      val (sa, salen, finish) = new_sock_addr ()
   in
      nonBlock
      (Prim.recvFrom (s, Word8Array.toPoly buf, i, sz, mk_in_flagsNB in_flags,
		      sa, salen),
       NONE,
       fn n => SOME (n, finish ()))
   end

fun recvVecFromNB' (S s, n, in_flags) =
   let
      val a = Word8Array.fromPoly (Primitive.Array.array n)
      val (sa, salen, finish) = new_sock_addr ()
   in
      nonBlock
      (Prim.recvFrom (s, Word8Array.toPoly a, 0, n, mk_in_flagsNB in_flags,
		      sa, salen),
       NONE,
       fn bytesRead =>
       SOME (if n = bytesRead
		then Word8Vector.fromArray a
	     else Word8Array.extract (a, 0, SOME bytesRead),
		finish ()))
   end

fun recvArrFromNB (sock, sl) = recvArrFromNB' (sock, sl, no_in_flags)

fun recvVecFromNB (sock, n) = recvVecFromNB' (sock, n, no_in_flags)

(* Phantom type. *)
type ('af,'sock_type) sock = sock

type 'af sock_addr = sock_addr

type 'mode stream = stream
   
end
