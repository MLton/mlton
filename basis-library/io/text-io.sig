signature TEXT_IO_GLOBAL =
   sig
      val print: string -> unit
   end

signature TEXT_IO =
   sig
      include TEXT_IO_GLOBAL
	 
      structure StreamIO: TEXT_STREAM_IO

      type vector = StreamIO.vector
      type elem = StreamIO.elem
      type instream

      val canInput: instream * int -> int option 
      val closeIn: instream -> unit 
      val endOfStream: instream -> bool
      val getInstream: instream -> StreamIO.instream 
      val input1: instream -> elem option 
      val input: instream -> vector 
      val inputAll: instream -> vector 
      val inputLine: instream -> string
      val inputN: instream * int -> vector 
      val lookahead: instream -> elem option
      val mkInstream: StreamIO.instream -> instream
      val openIn: string -> instream 
      val scanStream:
 	 ((Char.char, StreamIO.instream) StringCvt.reader
	  -> ('a, StreamIO.instream) StringCvt.reader)
	 -> instream -> 'a option
      val setInstream: (instream * StreamIO.instream) -> unit
      val stdIn: instream
(*       val openString: string -> instream
 *       val getPosIn: instream -> StreamIO.in_pos 
 *       val setPosIn: (instream * StreamIO.in_pos) -> unit 
 *)

      type outstream
      val closeOut: outstream -> unit 
      val flushOut: outstream -> unit 
      val getOutstream: outstream -> StreamIO.outstream
      val getPosOut: outstream -> StreamIO.out_pos 
      val mkOutstream: StreamIO.outstream -> outstream
      val openAppend: string -> outstream 
      val openOut: string -> outstream 
      val output1: outstream * elem -> unit 
      val output: outstream * vector -> unit 
      val outputSubstr: outstream * substring -> unit
      val setOutstream: outstream * StreamIO.outstream -> unit
      val stdErr: outstream 
      val stdOut: outstream 
(*       val setPosOut: outstream * StreamIO.out_pos -> unit  *)
   end

signature TEXT_IO_EXTRA =
   sig
      include TEXT_IO

(*      val equalsIn: instream * instream -> bool *)
(*      val equalsOut: outstream * outstream -> bool *)
      val inFd: instream -> Posix.IO.file_desc
      val newIn: Posix.IO.file_desc -> instream
      val newOut: Posix.IO.file_desc -> outstream
      val outFd: outstream -> Posix.IO.file_desc
(*      val setIn: instream * instream -> unit *)
   end
   
