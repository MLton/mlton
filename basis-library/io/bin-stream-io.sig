signature BIN_STREAM_IO =
   sig
      include STREAM_IO
              where type vector = Word8Vector.vector 
              where type elem = Word8Vector.elem
   end
