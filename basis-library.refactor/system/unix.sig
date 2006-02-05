signature UNIX =
   sig
      type ('a, 'b) proc
      type signal
      datatype exit_status = 
         W_EXITED
       | W_EXITSTATUS of Word8.word
       | W_SIGNALED of signal
       | W_STOPPED of signal

      val binInstreamOf: (BinIO.instream, 'a) proc -> BinIO.instream
      val binOutstreamOf: ('a, BinIO.outstream) proc -> BinIO.outstream
      val execute: string * string list -> ('a, 'b) proc
      val executeInEnv: string * string list * string list -> ('a, 'b) proc 
      val exit: Word8.word -> 'a
      val fromStatus: OS.Process.status -> exit_status
      val kill: ('a, 'b) proc * signal -> unit
      val reap: ('a, 'b) proc -> OS.Process.status
      val streamsOf: ((TextIO.instream, TextIO.outstream) proc
                      -> TextIO.instream * TextIO.outstream)
      val textInstreamOf: (TextIO.instream, 'a) proc -> TextIO.instream
      val textOutstreamOf: ('a, TextIO.outstream) proc -> TextIO.outstream
   end
