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
      val input: instream -> vector * instream
      val input1: instream -> (elem * instream) option
      val inputN: instream * int -> vector * instream
      val inputAll: instream -> vector * instream
      val canInput: instream * int -> int option
      val closeIn: instream -> unit
      val endOfStream: instream -> bool
      val output: outstream * vector -> unit
      val output1: outstream * elem -> unit
      val flushOut: outstream -> unit
      val closeOut: outstream -> unit
      val mkInstream: reader * vector -> instream
      val getReader: instream -> reader * vector
      val filePosIn: instream -> pos
      val setBufferMode: outstream * IO.buffer_mode -> unit
      val getBufferMode: outstream -> IO.buffer_mode
      val mkOutstream: writer * IO.buffer_mode -> outstream
      val getWriter: outstream -> writer * IO.buffer_mode
      val getPosOut: outstream -> out_pos
      val setPosOut: out_pos -> outstream
      val filePosOut: out_pos -> pos
   end

signature STREAM_IO_EXTRA =
   sig
      include STREAM_IO

      val equalsIn: instream * instream -> bool
      val instreamReader: instream -> reader

      val equalsOut: outstream * outstream -> bool
      val outstreamWriter: outstream -> writer

      val inputLine: instream -> (vector * instream)
   end

signature STREAM_IO_EXTRA_FILE =
   sig
      include STREAM_IO_EXTRA

      val inFd: instream -> Posix.IO.file_desc
      val outFd: outstream -> Posix.IO.file_desc
  end