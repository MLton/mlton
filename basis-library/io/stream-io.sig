signature STREAM_IO =
   sig
      type elem
      type vector

      type instream
      type outstream
      type out_pos

      type reader
      type writer
      type pos

      val canInput: instream * int -> int option
      val closeIn: instream -> unit
      val closeOut: outstream -> unit
      val endOfStream: instream -> bool
      val filePosIn: instream -> pos
      val filePosOut: out_pos -> pos
      val flushOut: outstream -> unit
      val getBufferMode: outstream -> IO.buffer_mode
      val getPosOut: outstream -> out_pos
      val getReader: instream -> reader * vector
      val getWriter: outstream -> writer * IO.buffer_mode
      val input1: instream -> (elem * instream) option
      val input: instream -> vector * instream
      val inputAll: instream -> vector * instream
      val inputN: instream * int -> vector * instream
      val mkInstream: reader * vector -> instream
      val mkOutstream: writer * IO.buffer_mode -> outstream
      val output1: outstream * elem -> unit
      val output: outstream * vector -> unit
      val setBufferMode: outstream * IO.buffer_mode -> unit
      val setPosOut: out_pos -> outstream
   end

signature STREAM_IO_EXTRA =
   sig
      include STREAM_IO
      type vector_slice

      structure Close:
	 sig
	    type t

	    val close: t -> unit
	    val equalsInstream: t * instream -> bool
	    val make: instream -> t
	 end

      val openVector: vector -> instream
      val input1': instream -> elem option * instream
      val inputLine: instream -> (vector * instream) option
      val equalsIn: instream * instream -> bool
      val instreamReader: instream -> reader
      val mkInstream': {reader: reader,
			closed: bool,
			buffer_contents: vector option} -> instream

      val outputSlice: outstream * vector_slice -> unit
      val equalsOut: outstream * outstream -> bool
      val outstreamWriter: outstream -> writer
      val mkOutstream': {writer: writer,
			 closed: bool,
			 buffer_mode: IO.buffer_mode} -> outstream
   end

signature STREAM_IO_EXTRA_FILE =
   sig
      include STREAM_IO_EXTRA

      val inFd: instream -> Posix.IO.file_desc
      val mkInstream'': {reader: reader,
			 closed: bool,
			 buffer_contents: vector option,
			 atExit: {close: bool}} -> instream
      val outFd: outstream -> Posix.IO.file_desc
      val mkOutstream'': {writer: writer,
			  closed: bool,
			  buffer_mode: IO.buffer_mode,
			  atExit: {close: bool}} -> outstream
  end
