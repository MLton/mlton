signature UNIX =
   sig
      type ('a, 'b) proc
      type signal
      datatype exit_status = 
	 W_EXITED
       | W_EXITSTATUS of Word8.word
       | W_SIGNALED of signal
       | W_STOPPED of signal
      val fromStatus: OS.Process.status -> exit_status
      val executeInEnv: string * string list * string list -> ('a, 'b) proc 
      val execute: string * string list -> ('a, 'b) proc
      val textInstreamOf: (TextIO.instream, 'a) proc -> TextIO.instream
      val binInstreamOf: (BinIO.instream, 'a) proc -> BinIO.instream
      val textOutstreamOf: ('a, TextIO.outstream) proc -> TextIO.outstream
      val binOutstreamOf: ('a, BinIO.outstream) proc -> BinIO.outstream
      val streamsOf: (TextIO.instream, TextIO.outstream) proc -> 
	             TextIO.instream * TextIO.outstream
      val reap: ('a, 'b) proc -> OS.Process.status
      val kill: ('a, 'b) proc * signal -> unit
      val exit: Word8.word -> 'a
   end
