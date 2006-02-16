signature BIN_IO_1997 =
   sig
      structure StreamIO: BIN_STREAM_IO_1997

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
      val inputN: instream * int -> vector 
      val lookahead: instream -> elem option
      val mkInstream: StreamIO.instream -> instream
      val openIn: string -> instream 
(*
      val scanStream:
         ((Char.char, StreamIO.instream) StringCvt.reader
          -> ('a, StreamIO.instream) StringCvt.reader)
         -> instream -> 'a option 
*)
      val setInstream: (instream * StreamIO.instream) -> unit
(*
      val getPosIn: instream -> StreamIO.in_pos 
      val setPosIn: (instream * StreamIO.in_pos) -> unit 
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
      val setOutstream: outstream * StreamIO.outstream -> unit 
(*
      val setPosOut: outstream * StreamIO.out_pos -> unit  
*)
   end
