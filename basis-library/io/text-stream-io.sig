(*
signature TEXT_STREAM_IO =
   sig
      include STREAM_IO
              where type vector = CharVector.vector 
	      where type elem = Char.char

      val inputLine: instream -> string * instream
      val outputSubstr: outstream * substring -> unit
   end
*)

signature TEXT_STREAM_IO =
   sig
      (* STREAM_IO *)
      type elem = Char.char
      type vector = CharVector.vector 
(*       
      type reader
*)
      
      type instream
      type outstream
      type writer
 
      type out_pos
      type pos (* = int *)

      val canInput: instream * int -> int option
      val closeIn: instream -> unit
      val endOfStream: instream -> bool 
      val filePosOut: out_pos -> pos
      val getWriter: outstream -> writer * IO.buffer_mode 
      val input1: instream -> (elem * instream) option 
      val input: instream -> vector * instream 
      val inputAll: instream -> vector * instream
      val inputN: instream * int -> vector * instream 
      val setPosOut: out_pos -> outstream 
(*      
      val mkInstream: reader * vector -> instream  (* need to update this *)
      val getReader: instream -> reader * vector 
      val output: outstream * vector -> unit 
      val output1: outstream * elem -> unit 
      val flushOut: outstream -> unit 
      val closeOut: outstream -> unit 
      val setBufferMode: outstream * IO.buffer_mode -> unit 
      val getBufferMode: outstream -> IO.buffer_mode 
      val mkOutstream: writer * IO.buffer_mode -> outstream 
      val getPosOut: outstream -> out_pos 
*)

      val inputLine: instream -> string * instream
(*       
      val outputSubstr: outstream * substring -> unit
*)
   end
