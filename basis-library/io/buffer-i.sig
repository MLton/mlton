signature BUFFER_I =
   sig
      structure StreamIO: STREAM_IO
      type vector = StreamIO.vector
      type elem = StreamIO.elem
      type instream
      val input: instream -> vector
      val input1: instream -> elem option
      val inputN: instream * int -> vector
      val inputAll: instream -> vector
      val canInput: instream * int -> int option
      val lookahead: instream -> elem option
      val closeIn: instream -> unit
      val endOfStream: instream -> bool
      val mkInstream: StreamIO.reader * vector -> instream
      val getInstream: instream -> StreamIO.instream
   end

signature BUFFER_I_EXTRA =
   sig
      include BUFFER_I

      val equalsIn: instream * instream -> bool
      val instreamReader: instream -> StreamIO.reader
      val mkInstream': {reader: StreamIO.reader,
			closed: bool,
			buffer_contents: vector option} -> instream
      val getInstream': ({reader: StreamIO.reader,
			  closed: bool,
			  buffer_contents: vector option} -> StreamIO.instream) ->
	                instream -> StreamIO.instream

      val openVector: vector -> instream

      val inputLine: instream -> vector
   end

signature BUFFER_I_EXTRA_FILE =
   sig
      include BUFFER_I_EXTRA

      val mkInstream'': {reader: StreamIO.reader,
			 closed: bool,
			 buffer_contents: vector option,
			 atExit: {close: bool}} -> instream

      val inFd: instream -> Posix.IO.file_desc
   end
