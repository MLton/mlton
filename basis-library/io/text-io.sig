signature TEXT_IO_GLOBAL =
   sig
      val print: string -> unit
   end

signature TEXT_IO =
   sig
      include TEXT_IO_GLOBAL

      structure StreamIO: TEXT_STREAM_IO
(*       where type elem = Char.char *)  (* redundant *)
         where type pos = TextPrimIO.pos
         where type reader = TextPrimIO.reader
(*       where type vector = CharVector.vector *) (* redundant *)
         where type writer = TextPrimIO.writer

      type elem = StreamIO.elem
      type instream
      type outstream
      type vector = StreamIO.vector

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
      val inputLine: instream -> string option
      val inputN: instream * int -> vector 
      val lookahead: instream -> elem option
      val mkInstream: StreamIO.instream -> instream
      val mkOutstream: StreamIO.outstream -> outstream
      val openAppend: string -> outstream 
      val openIn: string -> instream 
      val openOut: string -> outstream 
      val openString: string -> instream
      val output1: outstream * elem -> unit 
      val output: outstream * vector -> unit 
      val outputSubstr: outstream * substring -> unit
      val scanStream:
         ((Char.char, StreamIO.instream) StringCvt.reader
          -> ('a, StreamIO.instream) StringCvt.reader)
         -> instream -> 'a option
      val setInstream: (instream * StreamIO.instream) -> unit
      val setOutstream: outstream * StreamIO.outstream -> unit
      val setPosOut: outstream * StreamIO.out_pos -> unit
      val stdErr: outstream 
      val stdIn: instream
      val stdOut: outstream 
   end

signature TEXT_IO_EXTRA =
   sig
      include TEXT_IO

      val equalsIn: instream * instream -> bool
      val inFd: instream -> Posix.IO.file_desc
      val newIn: Posix.IO.file_desc * string -> instream
      val newOut: Posix.IO.file_desc * string -> outstream
      val outFd: outstream -> Posix.IO.file_desc
   end
