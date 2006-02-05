signature IMPERATIVE_IO =
   sig
      structure StreamIO: STREAM_IO

      type elem = StreamIO.elem
      type vector = StreamIO.vector
      type instream
      type outstream

      val canInput: instream * int -> int option
      val closeIn: instream -> unit
      val closeOut: outstream -> unit
      val endOfStream: instream -> bool
      val flushOut: outstream -> unit
      val getInstream: instream -> StreamIO.instream
      val getOutstream: outstream -> StreamIO.outstream
      val getPosOut: outstream -> StreamIO.out_pos
      val input1: instream -> elem option
      val input: instream -> vector
      val inputAll: instream -> vector
      val inputN: instream * int -> vector
      val lookahead: instream -> elem option
      val mkInstream: StreamIO.instream -> instream
      val mkOutstream: StreamIO.outstream -> outstream
      val output1: outstream * elem -> unit
      val output: outstream * vector -> unit
      val setInstream: instream * StreamIO.instream -> unit
      val setOutstream: outstream * StreamIO.outstream -> unit
      val setPosOut: outstream * StreamIO.out_pos -> unit
   end

signature IMPERATIVE_IO_EXTRA =
   sig
      structure StreamIO: STREAM_IO_EXTRA

      type elem = StreamIO.elem
      type instream
      type outstream
      type vector = StreamIO.vector
      type vector_slice = StreamIO.vector_slice

      val canInput: instream * int -> int option
      val closeIn: instream -> unit
      val closeOut: outstream -> unit
      val endOfStream: instream -> bool
      val equalsIn: instream * instream -> bool
      val flushOut: outstream -> unit
      val getInstream: instream -> StreamIO.instream
      val getOutstream: outstream -> StreamIO.outstream
      val getPosOut: outstream -> StreamIO.out_pos
      val inFd: instream -> Posix.IO.file_desc
      val input1: instream -> elem option
      val input: instream -> vector
      val inputAll: instream -> vector
      val inputLine: instream -> vector option
      val inputN: instream * int -> vector
      val lookahead: instream -> elem option
      val mkInstream: StreamIO.instream -> instream
      val mkOutstream: StreamIO.outstream -> outstream
      val newIn: Posix.IO.file_desc * string -> instream
      val newOut: Posix.IO.file_desc * string -> outstream
      val openAppend: string -> outstream
      val openIn: string -> instream
      val openOut: string -> outstream
      val openVector: vector -> instream
      val outFd: outstream -> Posix.IO.file_desc
      val output1: outstream * elem -> unit
      val output: outstream * vector -> unit
      val outputSlice: outstream * vector_slice -> unit
      val scanStream:
         ((elem, StreamIO.instream) StringCvt.reader
          -> ('a, StreamIO.instream) StringCvt.reader)
         -> instream -> 'a option
      val setInstream: instream * StreamIO.instream -> unit
      val setOutstream: outstream * StreamIO.outstream -> unit
      val setPosOut: outstream * StreamIO.out_pos -> unit
      val stdErr: outstream
      val stdIn: instream
      val stdOut: outstream
   end
