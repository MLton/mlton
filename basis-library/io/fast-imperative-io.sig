signature FAST_IMPERATIVE_IO =
   sig
      include IMPERATIVE_IO
      structure BufferI: BUFFER_I where type elem = StreamIO.elem
                                    and type vector = StreamIO.vector
                                    and type instream = StreamIO.instream
				    and type reader = StreamIO.reader
				    and type pos = StreamIO.pos
      val mkInbuffer: BufferI.inbuffer -> instream
      val withIn: instream * 
	          (BufferI.inbuffer -> 'a) * 
		  (StreamIO.instream -> 'a) -> 'a
   end

signature FAST_IMPERATIVE_IO_EXTRA =
   sig
      include IMPERATIVE_IO_EXTRA
      structure BufferI: BUFFER_I where type elem = StreamIO.elem
                                    and type vector = StreamIO.vector
                                    and type instream = StreamIO.instream
				    and type reader = StreamIO.reader
				    and type pos = StreamIO.pos
      val mkInbuffer: BufferI.inbuffer -> instream
      val withIn: instream * 
	          (BufferI.inbuffer -> 'a) * 
		  (StreamIO.instream -> 'a) -> 'a
   end

signature FAST_IMPERATIVE_IO_EXTRA_FILE =
   sig
      include IMPERATIVE_IO_EXTRA_FILE
      structure BufferI: BUFFER_I where type elem = StreamIO.elem
                                    and type vector = StreamIO.vector
                                    and type instream = StreamIO.instream
				    and type reader = StreamIO.reader
				    and type pos = StreamIO.pos
      val mkInbuffer: BufferI.inbuffer -> instream
      val withIn: instream * 
	          (BufferI.inbuffer -> 'a) * 
		  (StreamIO.instream -> 'a) -> 'a
   end
