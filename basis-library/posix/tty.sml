(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure PosixTTY: POSIX_TTY =
   struct
      structure Cstring = C.CS
      structure Prim = PosixPrimitive.TTY
      open Prim
      structure Error = PosixError

      type pid = Prim.pid
      datatype file_desc = datatype Prim.file_desc
      structure V =
	 struct
	    open V

	    type cc = char array

	    val default = #"\000"

	    fun new() = Array.array(nccs, default)

	    fun updates(a, l) = List.app (fn (i, c) => Array.update(a, i, c)) l

	    fun cc l = let val a = new()
		       in updates(a, l);
			  a
		       end

	    fun update(a, l) =
	       let val a' = new()
	       in Array.copy{src = a, si = 0, len = NONE,
			     dst = a', di = 0};
		  updates(a', l);
		  a'
	       end

	    val sub = Array.sub
	 end
      
      structure I =
	 struct
	    open I PosixFlags
	 end
      
      structure O =
	 struct
	    open O PosixFlags
	 end
      
      structure C =
	 struct
	    open C PosixFlags
	 end
      
      structure L =
	 struct
	    open L PosixFlags
	 end

      type speed = Prim.speed

      val compareSpeed = SysWord.compare
      fun id x = x
      val speedToWord = id
      val wordToSpeed = id

      type termios = {iflag: I.flags,
		      oflag: O.flags,
		      cflag: C.flags,
		      lflag: L.flags,
		      cc: V.cc,
		      ispeed: speed,
		      ospeed: speed}

      val termios = id
      val fieldsOf = id

      val getiflag: termios -> I.flags = #iflag
      val getoflag: termios -> O.flags = #oflag
      val getcflag: termios -> C.flags = #cflag
      val getlflag: termios -> L.flags = #oflag
      val getcc: termios -> V.cc = #cc

      structure CF =
	 struct
	    val getospeed: termios -> speed = #ospeed
	    fun setospeed({iflag, oflag, cflag, lflag, cc, ispeed, ...}: termios,
			  ospeed: speed): termios =
	       {iflag = iflag,
		oflag = oflag,
		cflag = cflag,
		lflag = lflag,
		cc = cc,
		ispeed = ispeed,
		ospeed = ospeed}
		
	    val getispeed: termios -> speed = #ispeed
	       
	    fun setispeed({iflag, oflag, cflag, lflag, cc, ospeed, ...}: termios,
			  ispeed: speed): termios =
	       {iflag = iflag,
		oflag = oflag,
		cflag = cflag,
		lflag = lflag,
		cc = cc,
		ispeed = ispeed,
		ospeed = ospeed}
	 end
      
      structure Termios = Prim.Termios
	 
      fun getattr(FD fd) =
	 (Error.checkResult(Prim.getattr(fd))
	  ; {iflag = Termios.iflag(),
	     oflag = Termios.oflag(),
	     cflag = Termios.cflag(),
	     lflag = Termios.lflag(),
	     cc = Cstring.toCharArrayOfLength(Termios.cc(), V.nccs),
	     ispeed = Termios.ispeed(),
	     ospeed = Termios.ospeed()})

      fun setattr(FD fd, a, {iflag, oflag, cflag, lflag, cc, ispeed, ospeed}) =
	 (Termios.setiflag iflag
	  ; Termios.setoflag oflag
	  ; Termios.setcflag cflag
	  ; Termios.setlflag lflag
	  ; Termios.setospeed ospeed
	  ; Termios.setispeed ispeed
	  ; let val cs = Termios.cc() 
	    in Util.naturalForeach
	       (V.nccs, fn i => Cstring.update(cs, i, V.sub(cc, i)))
	    end
	  ; Error.checkResult(Prim.setattr(fd, a)))

      fun sendbreak(FD fd, n) =
	 Error.checkResult(Prim.sendbreak(fd, n))

      fun drain(FD fd) = Error.checkResult(Prim.drain fd)
	 
      fun flush(FD fd, n) = Error.checkResult(Prim.flush(fd, n))
	 
      fun flow(FD fd, n) = Error.checkResult(Prim.flow(fd, n))
	 
      fun getpgrp(FD fd) = Error.checkReturnResult(Prim.getpgrp fd)
	 
      fun setpgrp(FD fd, pid) = Error.checkResult(Prim.setpgrp(fd, pid))
   end
