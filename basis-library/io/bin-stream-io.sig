(*
signature BIN_STREAM_IO =
   sig
      include STREAM_IO
              where type vector = Word8Vector.vector 
              where type elem = Word8Vector.elem
   end
*)

signature BIN_STREAM_IO =
   sig
      (* STREAM_IO *)
      type elem = Word8Vector.elem
      type vector = Word8Vector.vector 
(*       
      type reader
      type writer
*)
      
      type instream
      type outstream
 
      type out_pos
      type pos = int

      val canInput: instream * int -> int option
      val closeIn: instream -> unit
      val endOfStream: instream -> bool 
      val filePosOut: out_pos -> pos
      val input1: instream -> (elem * instream) option 
      val input: instream -> vector * instream 
      val inputAll: instream -> vector * instream
      val inputN: instream * int -> vector * instream 
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
      val getWriter: outstream -> writer * IO.buffer_mode 
      val getPosOut: outstream -> out_pos 
      val setPosOut: out_pos -> outstream 
*)
   end
