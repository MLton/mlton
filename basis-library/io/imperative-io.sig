signature IMPERATIVE_IO =
   sig
      structure StreamIO: STREAM_IO
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
   end

signature IMPERATIVE_IO_EXTRA =
   sig
      include IMPERATIVE_IO
   end