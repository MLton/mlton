structure Socket : SOCKET_EXTRA =
   struct
      structure Prim = Primitive.Socket
      structure PE = Posix.Error
      structure PFS = Posix.FileSys

      datatype ('af,'sock_type) sock = S of Prim.sock
      fun sockToWord (S s) = SysWord.fromInt s
      fun wordToSock s = S (SysWord.toInt s)
      fun sockToFD sock = PFS.wordToFD (sockToWord sock)
      fun fdToSock fd = wordToSock (PFS.fdToWord fd)

      type pre_sock_addr = Prim.pre_sock_addr
      datatype 'af sock_addr = SA of Prim.sock_addr
      fun unpackSockAddr (SA sa) = sa
      fun 'af new_sock_addr () : 
	  (pre_sock_addr * int ref * (unit -> 'af sock_addr)) = 
	let
	  val sa = Word8Array.array (Prim.sockAddrLenMax, 0wx0)
	  val salen = ref (Word8Array.length sa)
	  fun finish () =
	    SA (ArraySlice.vector
		(ArraySlice.slice (sa, 0, SOME (!salen))))
	in
	  (sa, salen, finish)
	end
      datatype dgram = DGRAM
      datatype 'mode stream = MODE
      datatype passive = PASSIVE
      datatype active = ACTIVE

      structure AF =
	 struct
	    type addr_family = Prim.AF.addr_family
	    val names = [
			 ("UNIX", Prim.AF.UNIX),
			 ("INET", Prim.AF.INET),
			 ("INET6", Prim.AF.INET6),
			 ("UNSPEC", Prim.AF.UNSPEC)
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
	    structure PW = Pack32Little

	    fun ('a, 'af, 'sock_type)
	        getSockOpt
		(level: level,
		 optname: optname,
		 optlen: int,
		 unmarshal: write_data * int * int -> 'a)
		((S s): ('af, 'sock_type) sock): 'a =
	      let
		val optval = Word8Array.array (optlen, 0wx0)
		val optlen = ref optlen
	      in
		PE.checkResult
		(Prim.Ctl.getSockOpt
		 (s, level, optname, optval, optlen));
		unmarshal (optval, !optlen, 0)
	      end
	    fun ('a, 'af, 'sock_type)
	        setSockOpt
		(level: level,
		 optname: optname,
		 marshal: 'a -> read_data)
		((S s): ('af, 'sock_type) sock,
		 optval: 'a): unit =
	      let
		val optval = marshal optval
		val optlen = Word8Vector.length optval
	      in
		PE.checkResult
		(Prim.Ctl.setSockOpt
		 (s, level, optname, optval, optlen))
	      end
	    fun ('a, 'af, 'sock_type)
		getIOCtl
		(request: request,
		 optlen: int,
		 unmarshal: write_data * int * int -> 'a)
		((S s): ('af, 'sock_type) sock): 'a =
	      let
		val optval = Word8Array.array (optlen, 0wx0)
	      in
		PE.checkResult
		(Prim.Ctl.getIOCtl
		 (s, request, optval));
		unmarshal (optval, optlen, 0)
	      end
	    fun ('a, 'af, 'sock_type)
	        setIOCtl
		(request: request,
		 marshal: 'a -> read_data)
		((S s): ('af, 'sock_type) sock,
		 optval: 'a): unit =
	       let
		 val optval = marshal optval
		 val optlen = Word8Vector.length optval
	       in
		 PE.checkResult
		 (Prim.Ctl.setIOCtl
		  (s, request, optval))
	       end

	    val wordLen = PW.bytesPerElem
	    fun unmarshalWord (wa, l, s) : word = 
	      Word.fromLargeWord (PW.subArr (wa, s))
	    val intLen : int = wordLen
	    fun unmarshalInt (wa, l, s) : int = 
	      Word.toIntX (unmarshalWord (wa, l, s))
	    val boolLen : int = intLen
	    fun unmarshalBool (wa, l, s) : bool = 
	      if (unmarshalInt (wa, l, s)) = 0 then false else true
	    val timeOptLen : int = boolLen + intLen
	    fun unmarshalTimeOpt (wa, l, s) : Time.time option =
	      if unmarshalBool (wa, l, s)
		then SOME (Time.fromSeconds
			   (LargeInt.fromInt
			    (unmarshalInt (wa, l, s + boolLen))))
		else NONE

	    fun marshalWord' (w, wa, s) =
	      PW.update (wa, s, Word.toLargeWord w)
	    fun marshalInt' (i, wa, s) =
	      marshalWord' (Word.fromInt i, wa, s)
	    fun marshalBool' (b, wa, s) =
	      marshalInt' (if b then 1 else 0, wa, s)
	    fun marshalTimeOpt' (t, wa, s) =
	      case t of
		NONE => (marshalBool' (false, wa, s);
			 marshalInt' (0, wa, s + boolLen))
	      | SOME t => (marshalBool' (true, wa, s);
			   marshalWord' (Word.fromLargeInt (Time.toSeconds t), 
					 wa, s + boolLen))
	    fun 'a marshal (len, f: 'a * Word8Array.array * int -> unit) (x: 'a) =
	      let
		val wa = Word8Array.array (len, 0wx0)
	      in
		f (x, wa, 0);
		Word8Array.vector wa
	      end
	    fun marshalWord w = marshal (wordLen, marshalWord') w
	    fun marshalInt i = marshal (intLen, marshalInt') i
	    fun marshalBool b = marshal (boolLen, marshalBool') b
	    fun marshalTimeOpt t = marshal (timeOptLen, marshalTimeOpt') t

	    fun ('af, 'sock_type) 
	        getSockOptWord
		(level: level,
		 optname: optname)
		(sock: ('af, 'sock_type) sock): word =
	      getSockOpt (level, optname, wordLen, unmarshalWord) sock
	    fun ('af, 'sock_type) 
	        getSockOptInt
		(level: level,
		 optname: optname)
		(sock: ('af, 'sock_type) sock): int =
	      getSockOpt (level, optname, intLen, unmarshalInt) sock
	    fun ('af, 'sock_type) 
	        getSockOptBool
		(level: level,
		 optname: optname)
		(sock: ('af, 'sock_type) sock): bool =
	      getSockOpt (level, optname, boolLen, unmarshalBool) sock
	    fun ('af, 'sock_type) 
	        getSockOptTimeOpt
		(level: level,
		 optname: optname)
		(sock: ('af, 'sock_type) sock): Time.time option =
	      getSockOpt (level, optname, timeOptLen, unmarshalTimeOpt) sock
	    fun ('af, 'sock_type) 
	        setSockOptWord
		(level: level,
		 optname: optname)
		(sock: ('af, 'sock_type) sock,
		 optval: word): unit =
	      setSockOpt (level, optname, marshalWord) (sock, optval)
	    fun ('af, 'sock_type) 
	        setSockOptInt
		(level: level, 
		 optname: optname)
		(sock: ('af, 'sock_type) sock,
		 optval: int): unit =
	      setSockOpt (level, optname, marshalInt) (sock, optval)
	    fun ('af, 'sock_type) 
	        setSockOptBool
		(level: level,
		 optname: optname)
		(sock: ('af, 'sock_type) sock,
		 optval: bool): unit =
	      setSockOpt (level, optname, marshalBool) (sock, optval)
	    fun ('af, 'sock_type) 
	        setSockOptTimeOpt
		(level: level,
		 optname: optname)
		(sock: ('af, 'sock_type) sock,
		 optval: Time.time option): unit =
	      setSockOpt (level, optname, marshalTimeOpt) (sock, optval)


	    fun ('af, 'sock_type) 
	        getIOCtlWord
		(request: request)
		(sock: ('af, 'sock_type) sock): word =
	      getIOCtl (request, wordLen, unmarshalWord) sock
	    fun ('af, 'sock_type) 
	        getIOCtlInt
		(request: request)
		(sock: ('af, 'sock_type) sock): int =
	      getIOCtl (request, intLen, unmarshalInt) sock
	    fun ('af, 'sock_type) 
	        getIOCtlBool
		(request: request)
		(sock: ('af, 'sock_type) sock): bool =
	      getIOCtl (request, boolLen, unmarshalBool) sock
	    fun ('af, 'sock_type) 
	        setIOCtlWord
		(request: request)
		(sock: ('af, 'sock_type) sock,
		 optval: word): unit =
	      setIOCtl (request, marshalWord) (sock, optval)
	    fun ('af, 'sock_type) 
	        setIOCtlInt
		(request: request)
		(sock: ('af, 'sock_type) sock,
		 optval: int): unit =
	      setIOCtl (request, marshalInt) (sock, optval)
	    fun ('af, 'sock_type) 
	        setIOCtlBool
		(request: request)
		(sock: ('af, 'sock_type) sock,
		 optval: bool): unit =
	      setIOCtl (request, marshalBool) (sock, optval)
	 end

      structure Ctl =
	 struct
	    open CtlExtra

	    fun getDEBUG sock = 
	      getSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.DEBUG) sock
	    fun setDEBUG (sock,optval) = 
	      setSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.DEBUG) (sock,optval)
	    fun getREUSEADDR sock = 
	      getSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.REUSEADDR) sock
	    fun setREUSEADDR (sock,optval) = 
	      setSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.REUSEADDR) (sock,optval)
	    fun getKEEPALIVE sock = 
	      getSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.KEEPALIVE) sock
	    fun setKEEPALIVE (sock,optval) = 
	      setSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.KEEPALIVE) (sock,optval)
	    fun getDONTROUTE sock = 
	      getSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.DONTROUTE) sock
	    fun setDONTROUTE (sock,optval) = 
	      setSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.DONTROUTE) (sock,optval)
	    fun getBROADCAST sock = 
	      getSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.BROADCAST) sock
	    fun getLINGER sock =
	      getSockOptTimeOpt (Prim.Ctl.SOCKET, Prim.Ctl.LINGER) sock
	    fun setLINGER (sock,optval) =
	      setSockOptTimeOpt (Prim.Ctl.SOCKET, Prim.Ctl.LINGER) (sock,optval)
	    fun setBROADCAST (sock,optval) = 
	      setSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.BROADCAST) (sock,optval)
	    fun getOOBINLINE sock = 
	      getSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.OOBINLINE) sock
	    fun setOOBINLINE (sock,optval) = 
	      setSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.OOBINLINE) (sock,optval)
	    fun getSNDBUF sock = 
	      getSockOptInt (Prim.Ctl.SOCKET, Prim.Ctl.SNDBUF) sock
	    fun setSNDBUF (sock,optval) = 
	      setSockOptInt (Prim.Ctl.SOCKET, Prim.Ctl.SNDBUF) (sock,optval)
	    fun getRCVBUF sock = 
	      getSockOptInt (Prim.Ctl.SOCKET, Prim.Ctl.RCVBUF) sock
	    fun setRCVBUF (sock,optval) = 
	      setSockOptInt (Prim.Ctl.SOCKET, Prim.Ctl.RCVBUF) (sock,optval)
	    fun getTYPE sock =
	      getSockOptInt (Prim.Ctl.SOCKET, Prim.Ctl.TYPE) sock
	    fun getERROR sock =
	      getSockOptBool (Prim.Ctl.SOCKET, Prim.Ctl.ERROR) sock
	    local
	      fun getName 
                  (f: Prim.sock * pre_sock_addr * int ref -> int)
                  (S s) =
		let
		  val (sa, salen, finish) = new_sock_addr ()
		  val _ = PE.checkResult
		          (f (s, sa, salen))
		in
		  finish ()
		end
	    in
	      fun getPeerName sock = getName Prim.Ctl.getPeerName sock
	      fun getSockName sock = getName Prim.Ctl.getSockName sock
	    end
	    fun setNBIO (sock,optval) =
	      setIOCtlBool Prim.Ctl.NBIO (sock,optval)
	    fun getNREAD sock =
	      getIOCtlInt Prim.Ctl.NREAD sock
	    fun getATMARK sock =
	      getIOCtlBool Prim.Ctl.ATMARK sock
	 end
      fun sameAddr (SA sa1, SA sa2) = sa1 = sa2
      fun familyOfAddr (SA sa) = Prim.familyOfAddr sa

      fun bind (S s, SA sa) =
	PE.checkResult
	(Prim.bind (s, sa, Word8Vector.length sa))
      fun listen (S s, n) =
	PE.checkResult
	(Prim.listen (s, n))
      fun connect (S s, SA sa) =
	PE.checkResult
	(Prim.connect (s, sa, Word8Vector.length sa))
      fun accept (S s) =
	let
	  val (sa, salen, finish) = new_sock_addr ()
	  val s = PE.checkReturnResult
	          (Prim.accept (s, sa, salen))
	in
	  (S s, finish ())
	end
      fun close (S s) =
	PE.checkResult
	(Prim.close (s))
      datatype shutdown_mode = NO_RECVS | NO_SENDS | NO_RECVS_OR_SENDS
      fun shutdownModeToHow m =
	case m of
	  NO_RECVS => Prim.SHUT_RD
	| NO_SENDS => Prim.SHUT_WR
	| NO_RECVS_OR_SENDS => Prim.SHUT_RDWR
      fun shutdown (S s, m) =
	PE.checkResult
	(Prim.shutdown (s, shutdownModeToHow m))
      fun pollDesc sock = 
	Option.valOf (OS.IO.pollDesc (sockToFD sock))
 
      type 'a buf = {buf : 'a, i : int, sz : int option}

      type out_flags = {don't_route : bool, oob : bool}
      fun mk_out_flags {don't_route, oob} =
	Word.orb (if don't_route then Prim.MSG_DONTROUTE else 0wx0,
	Word.orb (if oob then Prim.MSG_OOB else 0wx0,
		  0wx0))
      val no_out_flags = {don't_route = false, oob = false}

      fun sendVec' (S s, {buf, i, sz}, out_flags) =
	let
	  val max = Vector.checkSlice (buf, i, sz)
	in
	  PE.checkReturnResult 
	  (Prim.send (s, buf, i, max -? i, mk_out_flags out_flags))
	end
      fun sendArr' (sock, {buf, i, sz}, out_flags) =
	sendVec' (sock, 
		  {buf = Word8Vector.fromArray buf, i = i, sz = sz}, out_flags)
      fun sendVec (sock, buf) = 
	sendVec' (sock, buf, no_out_flags)
      fun sendArr (sock, buf) = 
	sendArr' (sock, buf, no_out_flags)

      fun sendVecTo' (S s, SA sa, {buf, i, sz}, out_flags) =
	let
	  val max = Vector.checkSlice (buf, i, sz)
	in
	  PE.checkReturnResult
	  (Prim.sendTo (s, buf, i, max -? i, mk_out_flags out_flags,
			sa, Word8Vector.length sa))
	end
      fun sendArrTo' (sock, sock_addr, {buf, i, sz}, out_flags) =
	sendVecTo' (sock, sock_addr, 
		    {buf = Word8Vector.fromArray buf, i = i, sz = sz}, out_flags)
      fun sendVecTo (sock, sock_addr, buf) = 
	sendVecTo' (sock, sock_addr, buf, no_out_flags)
      fun sendArrTo (sock, sock_addr, buf) = 
	sendArrTo' (sock, sock_addr, buf, no_out_flags)

      type in_flags = {peek : bool, oob : bool}
      fun mk_in_flags {peek, oob} =
	Word.orb (if peek then Prim.MSG_PEEK else 0wx0,
	Word.orb (if oob then Prim.MSG_OOB else 0wx0,
		  0wx0))
      val no_in_flags = {peek = false, oob = false}

      fun recvArr' (S s, {buf, i, sz}, in_flags) =
	let
	  val max = Array.checkSlice (buf, i, sz)
	in
	  PE.checkReturnResult
	  (Prim.recv (s, buf, i, max -? i, mk_in_flags in_flags))
	end
      fun recvVec' (sock, n, in_flags) =
	let
	  val a = Primitive.Array.array n
	  val bytesRead = 
	    recvArr' (sock, {buf = a, i = 0, sz = SOME n}, in_flags)
	in
	  if n = bytesRead
	    then Word8Vector.fromArray a
	    else Word8Array.extract (a, 0, SOME bytesRead)
	end
      fun recvArr (sock, buf) =
	recvArr' (sock, buf, no_in_flags)
      fun recvVec (sock, n) =
	recvVec' (sock, n, no_in_flags)

      fun recvArrFrom' (S s, {buf, i, sz}, in_flags) =
	let
	  val max = Array.checkSlice (buf, i, sz)
	  val (sa, salen, finish) = new_sock_addr ()
	  val n = PE.checkReturnResult
	          (Prim.recvFrom (s, buf, i, max -? i, mk_in_flags in_flags,
				  sa, salen))
	in
	  (n, finish ())
	end
      fun recvVecFrom' (sock, n, in_flags) =
	let
	  val a = Primitive.Array.array n
	  val (bytesRead, sock_addr) = 
	    recvArrFrom' (sock, {buf = a, i = 0, sz = SOME n}, in_flags)
	in
	  (if n = bytesRead
	     then Word8Vector.fromArray a
	     else Word8Array.extract (a, 0, SOME bytesRead),
	   sock_addr)
	end
      fun recvArrFrom (sock, buf) =
	recvArrFrom' (sock, buf, no_in_flags)
      fun recvVecFrom (sock, n) =
	recvVecFrom' (sock, n, no_in_flags)
   end