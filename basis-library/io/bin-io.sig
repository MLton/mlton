(*
signature BIN_IO =
   sig
      structure StreamIO: BIN_STREAM_IO

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

      val openIn: string -> instream
      val openOut: string -> outstream
      val openAppend: string -> outstream
   end
*)

signature BIN_IO =
   sig
      structure StreamIO: BIN_STREAM_IO

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

signature BIN_IO_EXTRA =
   sig
      include BIN_IO

      val equalsIn: instream * instream -> bool
      val equalsOut: outstream * outstream -> bool
      val newIn: Posix.IO.file_desc -> instream
      val newOut: Posix.IO.file_desc -> outstream
      val inFd: instream -> Posix.IO.file_desc
      val outFd: outstream -> Posix.IO.file_desc

      val stdIn: instream
      val stdErr: outstream
      val stdOut: outstream
   end
