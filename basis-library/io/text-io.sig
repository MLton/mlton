signature TEXT_IO_GLOBAL =
   sig
      val print: string -> unit
   end

signature TEXT_IO =
   sig
      include TEXT_IO_GLOBAL

      structure StreamIO : TEXT_STREAM_IO
			   where type reader = TextPrimIO.reader
			   where type writer = TextPrimIO.writer
			   where type pos = TextPrimIO.pos

      (* IMPERATIVE_IO *)
      type vector = StreamIO.vector
      type elem = StreamIO.elem
      type instream
      type outstream
      val input: instream -> vector
      val input1: instream -> elem option
      val inputN: instream * int -> vector
      val inputAll: instream -> vector
      val canInput: instream * int -> int option
      val lookahead: instream -> elem option
      val closeIn: instream -> unit
      val endOfStream: instream -> bool
      val output: outstream * vector -> unit
      val output1: outstream * elem -> unit
      val flushOut: outstream -> unit
      val closeOut: outstream -> unit
      val mkInstream: StreamIO.instream -> instream
      val getInstream: instream -> StreamIO.instream
      val setInstream: instream * StreamIO.instream -> unit
      val mkOutstream: StreamIO.outstream -> outstream
      val getOutstream: outstream -> StreamIO.outstream
      val setOutstream: outstream * StreamIO.outstream -> unit
      val getPosOut: outstream -> StreamIO.out_pos
      val setPosOut: outstream * StreamIO.out_pos -> unit

      val inputLine: instream -> string
      val outputSubstr: outstream * substring -> unit
      val openIn: string -> instream
      val openOut: string -> outstream
      val openAppend: string -> outstream
      val openString: string -> instream
      val stdIn: instream
      val stdOut: outstream
      val stdErr: outstream
      val scanStream: ((Char.char, StreamIO.instream) StringCvt.reader -> 
		       ('a, StreamIO.instream) StringCvt.reader) -> 
	              instream -> 'a option
   end

signature TEXT_IO_EXTRA =
   sig
      include TEXT_IO

      val equalsIn: instream * instream -> bool
      val equalsOut: outstream * outstream -> bool
      val inFd: instream -> Posix.IO.file_desc
      val newIn: Posix.IO.file_desc -> instream
      val newOut: Posix.IO.file_desc -> outstream
      val outFd: outstream -> Posix.IO.file_desc
   end
