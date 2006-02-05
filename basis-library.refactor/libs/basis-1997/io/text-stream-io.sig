signature TEXT_STREAM_IO_1997 =
   sig
      include STREAM_IO_1997
         where type vector = CharVector.vector 
         where type elem = Char.char

      val inputLine: instream -> string * instream
(*
      val outputSubstr: outstream * substring -> unit
*)
   end
