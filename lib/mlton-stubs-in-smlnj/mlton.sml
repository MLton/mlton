(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)

(* This file is just a dummy provided in place of the structure that MLton
 * supplies so that we can compile under SML/NJ.
 *) 
structure MLton: MLTON =
   struct
      val isMLton = false
      fun serialize _ = raise Fail "serialize"
      fun deserialize _ = raise Fail "deserialize"
      fun cleanAtExit _ = raise Fail "cleanAtExit"
      val debug = true
      val safe = true
      fun size _ = ~1: int

      structure Array =
	 struct
	    open Array

	    fun unfoldi (n, a, f) =
	       let
		  val r = ref a
	       in
		  tabulate (n, fn i =>
			    let
			       val (b, a') = f (i, !r)
			       val _ = r := a'
			    in
			       b
			    end)
	       end

	    fun unfold (n, a, f) = unfoldi (n, a, f o #2)
	 end
      
      structure Cont =
	 struct
	    structure Cont = SMLofNJ.Cont
	       
	    type 'a t = 'a Cont.cont

	    val callcc = Cont.callcc
	    fun throw (k, v) = Cont.throw k v
	    fun throw' _ = raise Fail "Cont.throw'"
	    fun prepend _ = raise Fail "Cont.prepend"
	 end

      structure GC =
	 struct
	    fun collect _ = ()
	    fun setMessages _ = ()
	    fun setSummary _ = ()
	    fun time _ = Time.zeroTime
	 end

      structure Itimer =
	 struct
	    datatype which = Prof | Real | Virtual

	    fun whichSignal _ = Posix.Signal.alrm
	    fun set _ = raise Fail "Itimer.set"
	 end

      structure ProcEnv =
	 struct
	    fun setenv _ = raise Fail "setenv"
	 end
      
      structure Ptrace =
	 struct
	    type pid = Posix.Process.pid
	    fun attach _ = raise Fail "attach"
	    fun cont _ = raise Fail "cont"
	    fun detach _ = raise Fail "detach"
	    fun kill _ = raise Fail "kill"
	    fun peekText _ = raise Fail "peekText"
	    fun singleStep _ = raise Fail "singleStep"
	    fun sysCall _ = raise Fail "sysCall"
	 end

      structure Random =
	 struct
	    fun seed _ = 0w13: Word32.word
	    fun useed _ = 0w13: Word32.word
	    local
	       val seed: word ref = ref 0w13
	    in
	       (* From page 284 of Numerical Recipes in C. *)
	       fun rand (): word =
		  let
		     val res = 0w1664525 * !seed + 0w1013904223
		     val _ = seed := res
		  in
		     res
		  end

	       fun srand (w: word): unit = seed := w

	       structure String =
		  struct
		     open String
			
		     val tabulate = CharVector.tabulate
		  end
	       local
		  val chars =
		     "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
		  val n = Word.fromInt (String.size chars)
		  val r: word ref = ref 0w0
	       in
		  fun alphaNumString (length: int) =
		     String.tabulate
		     (length, fn i =>
		      let
			 val _ =
			    if 0 = Int.quot (i, 6) (* n^6 = 62^6 = 965,660,736 *)
			       then r := rand ()
			    else ()
			 val w = !r
			 val c = String.sub (chars, Word.toInt (Word.mod (w, n)))
			 val _ = r := Word.div (w, n)
		      in
			 c
		      end)
	       end
	    end
	 end

      structure Rlimit =
	 struct
	    type rlim = Word31.word
	       
	    val infinity: rlim = 0w0

	    type resource = int
	       
	    val cpuTime: resource = 0
	    val coreFileSize: resource = 0
	    val dataSize: resource = 0
	    val fileSize: resource = 0
	    val lockedInMemorySize: resource = 0
	    val numFiles: resource = 0
	    val numProcesses: resource = 0
	    val residentSetSize: resource = 0
	    val stackSize: resource = 0
	    val virtualMemorySize: resource = 0

	    fun get _ = raise Fail "Rlimit.get"
	    fun set _ = raise Fail "Rlimit.set"
	 end
      
      structure Rusage =
         struct
	   type t = {utime: Time.time, stime: Time.time}

	   (* Fake it with Posix.ProcEnv.times *)
	   fun rusage () =
	      let
		 val zero = {utime = Time.zeroTime, stime = Time.zeroTime}
	      in
		 let
		    val {utime, stime, cutime, cstime, ...} =
		       Posix.ProcEnv.times ()
		 in
		    {self = {utime = utime, stime = stime},
		     children = {utime = cutime, stime = cstime},
		     gc = zero}
		 end handle Time => {children = zero, gc = zero, self = zero}
		 (* The handle Time is there because of a bug in SML/NJ that
		  * causes a Time exception to be raised on machines with a
		  * large uptime (enough that the number of clock ticks is
		  * >= 2^31).
		  *)
	      end
	 end

      structure Signal =
	 struct
	    open Posix.Signal

	    val prof = alrm
	    val vtalrm = alrm

	    structure Mask =
	       struct
		  type t = unit

		  val all = ()
		  fun some _ = ()
		  fun block _ = raise Fail "block"
		  fun unblock _ = raise Fail "unblock"
		  fun set _ = raise Fail "set"
	       end

	    structure Handler =
	       struct
		  datatype t =
		     Default
		   | Ignore
		   | Handler of unit Thread.t -> unit Thread.t

		  fun simple _ = Default
		  fun get _ = Default
		  fun set _ = ()
	       end
	 end

      structure Socket =
	 struct
	    structure Address =
	       struct
		  type t = word
	       end

	    structure Host =
	       struct
		  type t = {name: string}

		  fun getByAddress _ = raise Fail "Socket.Host.getByAddress"
		  fun getByName _ = raise Fail "Socket.Host.getByName"
	       end

	    structure Port =
	       struct
		  type t = int
	       end

	    type t = unit
	       
	    fun accept _ = raise Fail "Socket.accept"
	    fun connect _ = raise Fail "Socket.connect"
	    fun listen _ = raise Fail "Socket.listen"
	    fun listenAt _ = raise Fail "Socket.listenAt"
	    fun shutdownRead _ = raise Fail "Socket.shutdownWrite"
	    fun shutdownWrite _ = raise Fail "Socket.shutdownWrite"
	 end

      structure Syslog = Syslog

      structure TextIO =
	 struct
	    fun equalsIn _ = raise Fail "equalsIn"
	    fun equalsOut _ = raise Fail "equalsOut"
	    fun inFd _ = raise Fail "inFd"
	    fun tempName _ = raise Fail "MLton.textIO.tempName"
	    fun mkstemps {prefix, suffix} =
	       let
		  val name = concat [prefix, Random.alphaNumString 6, suffix]
	       in (name, TextIO.openOut name)
	       end
	    fun mkstemp s = mkstemps {prefix = s, suffix = ""}
	    fun newIn (fd: Posix.IO.file_desc): TextIO.instream =
	       let
		  val reader =
		     TextPrimIO.RD
		     {name = "<fd>",
		      chunkSize = 4096,
		      readVec =
		      SOME (fn i => Byte.bytesToString (Posix.IO.readVec (fd, i))),
		      readArr = NONE,
		      readVecNB = NONE,
		      readArrNB = NONE,
		      block = NONE,
		      canInput = NONE,
		      avail = fn _ => NONE,
		      getPos = NONE,
		      setPos = NONE,
		      endPos = NONE,
		      verifyPos = NONE,
		      close = fn () => Posix.IO.close fd,
		      ioDesc = NONE}
	       in TextIO.mkInstream (TextIO.StreamIO.mkInstream (reader, ""))
	       end
	    fun newOut _ = raise Fail "newOut"
	    fun outFd _ = raise Fail "outFd"
	    fun setIn _ = raise Fail "setIn"
	 end

      structure Thread = Thread

      structure Vector =
	 struct
	    open Vector

	    fun unfoldi (n, a, f) =
	       let
		  val r = ref a
	       in
		  tabulate (n, fn i =>
			    let
			       val (b, a') = f (i, !r)
			       val _ = r := a'
			    in
			       b
			    end)
	       end

	    fun unfold (n, a, f) = unfoldi (n, a, f o #2)
	 end

      structure World =
	 struct
	    datatype status = Original | Clone

	    fun load _ = raise Fail "World.load"
	    fun save _ = raise Fail "World.save"
	    fun saveThread _ = raise Fail "World.saveThread"
	 end

      structure Word =
	 struct
	    open Word

	    fun ~ (x: word) = 0w0 - x
	    fun rol (w, w') =
	       let
		  val w' = w' mod (fromInt wordSize)
	       in
		  orb (>> (w, fromInt wordSize - w'),
		       << (w, w'))
	       end
	    fun ror (w, w') =
	       let
		  val w' = w' mod (fromInt wordSize)
	       in
		  orb (>> (w, w'),
		       << (w, fromInt wordSize - w'))
	       end
	 end
      
      structure Word8 =
	 struct
	    open Word8

	    fun ~ (x: word) = 0w0 - x
	    val _ = >> : word * Word.word -> word
	    fun rol (w: word, w': Word.word): word =
	       let
		  val w' = Word.mod (w', Word.fromInt wordSize)
	       in
		  orb (>> (w, Word.- (Word.fromInt wordSize, w')),
		       << (w, w'))
	       end
	    fun ror (w, w') =
	       let
		  val w' = Word.mod (w', Word.fromInt wordSize)
	       in
		  orb (>> (w, w'),
		       << (w, Word.- (Word.fromInt wordSize, w')))
	       end
	 end
   end
