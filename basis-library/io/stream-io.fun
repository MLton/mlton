signature STREAM_IO_ARG = 
   sig
     structure PrimIO: PRIM_IO
     structure Vector: MONO_VECTOR
     structure Array: MONO_ARRAY
     sharing type PrimIO.elem = Vector.elem = Array.elem
     sharing type PrimIO.vector = Vector.vector = Array.vector
     sharing type PrimIO.array = Array.array
     val someElem: PrimIO.elem
   end

functor StreamIO (S: STREAM_IO_ARG) : STREAM_IO =
   struct
      open S

      type elem = PrimIO.elem
      type vector = PrimIO.vector
      type reader = PrimIO.reader
      type writer = PrimIO.writer
      type pos = PrimIO.pos

   end