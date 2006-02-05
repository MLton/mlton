signature TEXT_STREAM_IO =
   sig
      include STREAM_IO
         where type elem = Char.char
         where type vector = CharVector.vector 

      val inputLine: instream -> (string * instream) option
      val outputSubstr: outstream * substring -> unit
   end
