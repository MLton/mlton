signature IMPERATIVE_IO_EXTRA_ARG = 
   sig
      structure StreamIO: STREAM_IO
      structure Vector: MONO_VECTOR
      sharing type StreamIO.elem = Vector.elem
      sharing type StreamIO.vector = Vector.vector
      val mkReader: {fd: Posix.FS.file_desc,
		     name: string,
		     initBlkMode: bool} -> StreamIO.reader
      val mkWriter: {fd: Posix.FS.file_desc,
		     name: string,
		     appendMode: bool,
		     initBlkMode: bool,
		     chunkSize: int} -> StreamIO.writer
   end

functor ImperativeIOExtra(S: IMPERATIVE_IO_ARG): IMPERATIVE_IO_EXTRA =
   struct
      open S

      structure SIO = StreamIO
      structure V = Vector

      type vector = StreamIO.vector
      type elem = StreamIO.elem

      (*---------------*)
      (*   outstream   *)
      (*---------------*)

      datatype outstream = Out of SIO.outstream ref

      fun output (Out os, v) = SIO.output (!os, v)
      fun output1 (Out os, v) = SIO.output1 (!os, v)
      fun flushOut (Out os) = SIO.flushOut (!os)
      fun closeOut (Out os) = SIO.closeOut (!os)
      fun mkOutstream os = Out (ref os)
      fun getOutstream (Out os) = !os
      fun setOutstream (Out os, os') = os := os'
      fun getPosOut (Out os) = SIO.getPosOut (!os)
      fun setPosOut (Out os, out_pos) = os := SIO.setPosOut out_pos

      (*---------------*)
      (*   instream    *)
      (*---------------*)

      datatype instream = In of SIO.instream ref

      fun input (In is) = let val (v, is') = SIO.input (!is)
			  in is := is'; v
			  end
      fun input1 (In is) = 
	Option.map (fn (c, is') => (is := is'; c)) (SIO.input1 (!is))
      fun inputN (In is, n) = let val (v, is') = SIO.inputN (!is, n)
			      in is := is'; v
			      end
      fun inputAll (In is) = let val (v, is') = SIO.inputAll (!is)
			     in is := is'; v
			     end
      fun canInput (In is, n) = SIO.canInput (!is, n)
      fun lookahead (In is) = let val (v, _) = SIO.input (!is)
			      in
				if V.length v = 0
				  then NONE
				  else SOME (V.sub (v, 0))
			      end
      fun closeIn (In is) = SIO.closeIn (!is)
      fun endOfStream (In is) = SIO.endOfStream (!is)
      fun mkInstream is = In (ref is)
      fun getInstream (In is) = !is
      fun setInstream (In is, is') = is := is'
   end

signature IMPERATIVE_IO_ARG = 
   sig
      structure StreamIO: STREAM_IO
      structure Vector: MONO_VECTOR
      sharing type StreamIO.elem = Vector.elem
      sharing type StreamIO.vector = Vector.vector
   end

functor ImperativeIO(S: IMPERATIVE_IO_ARG): IMPERATIVE_IO = 
  ImperativeIOExtra(open S
		    fun mkReader _ = raise (Fail "<mkReader>")
		    fun mkWriter _ = raise (Fail "<mkWriter>"))
