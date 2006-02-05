signature BIN_STREAM_IO_1997 =
   sig
      include STREAM_IO_1997
         where type vector = Word8Vector.vector 
         where type elem = Word8Vector.elem
   end
