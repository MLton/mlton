signature IMPERATIVE_IO_EXTRA_ARG = 
   sig
      structure StreamIO: STREAM_IO_EXTRA
      structure Vector: MONO_VECTOR
      structure Array: MONO_ARRAY
      sharing type StreamIO.elem = Vector.elem = Array.elem
      sharing type StreamIO.vector = Vector.vector = Array.vector
      val openVector: Vector.vector -> StreamIO.reader
   end

functor ImperativeIOExtra(S: IMPERATIVE_IO_EXTRA_ARG): IMPERATIVE_IO_EXTRA =
   struct
      open S

      structure SIO = StreamIO
      structure V = Vector
      structure A = Array

      type vector = StreamIO.vector
      type elem = StreamIO.elem

      (*---------------*)
      (*   outstream   *)
      (*---------------*)

      datatype outstream = Out of SIO.outstream ref

      fun equalsOut (Out os1, Out os2) = os1 = os2

      fun output (Out os, v) = SIO.output (!os, v)
      fun output1 (Out os, v) = SIO.output1 (!os, v)
      fun outputSlice (Out os, (v, i, sz)) = SIO.outputSlice (!os, (v, i, sz))
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

      fun equalsIn (In is1, In is2) = is1 = is2

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
      fun inputLine (In is) = let val (v, is') = SIO.inputLine (!is)
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


      val empty = V.fromList []

      val openVector = fn v =>
	let
	  val reader = openVector v
	  val instream = SIO.mkInstream (reader, empty)
	in
	  mkInstream instream
	end

      fun scanStream f is =
	case f SIO.input1 (getInstream is) of
	  NONE => NONE
	| SOME (v, is') => (setInstream (is, is'); SOME v)
   end

signature IMPERATIVE_IO_EXTRA_FILE_ARG =
   sig
      structure StreamIO: STREAM_IO_EXTRA_FILE
      structure Vector: MONO_VECTOR
      structure Array: MONO_ARRAY
      sharing type StreamIO.elem = Vector.elem = Array.elem
      sharing type StreamIO.vector = Vector.vector = Array.vector
      val openVector: Vector.vector -> StreamIO.reader

      val mkReader: {fd: Posix.FileSys.file_desc,
		     name: string,
		     initBlkMode: bool} -> StreamIO.reader
      val mkWriter: {fd: Posix.FileSys.file_desc,
		     name: string,
		     appendMode: bool,
		     initBlkMode: bool,
		     chunkSize: int} -> StreamIO.writer
      val chunkSize: int
      val fileTypeFlags: Posix.FileSys.O.flags list
   end

functor ImperativeIOExtraFile(S: IMPERATIVE_IO_EXTRA_FILE_ARG): IMPERATIVE_IO_EXTRA_FILE = 
   struct
      open S

      structure SIO = StreamIO
      structure V = Vector

      structure ImperativeIO = ImperativeIOExtra(open S)
      open ImperativeIO

      structure PIO = Posix.IO
      structure PFS = Posix.FileSys

      fun liftExn name function cause = raise IO.Io {name = name,
						     function = function,
						     cause = cause}

      val empty = V.fromList []

      (*---------------*)
      (*   outstream   *)
      (*---------------*)

      fun newOut (fd, name, appendMode, buffer_mode) =
	let
	  val writer = mkWriter {fd = fd, name = name,
				 appendMode = appendMode, 
				 initBlkMode = true,
				 chunkSize = chunkSize}
	  val outstream = SIO.mkOutstream (writer, buffer_mode)
	in
	  mkOutstream outstream
	end
      val stdErr = newOut (PFS.stderr, "<stderr>", true, IO.NO_BUF)
      val newOut = fn (fd, name, appendMode) => 
	newOut (fd, name, appendMode,
		if Posix.ProcEnv.isatty fd then IO.LINE_BUF else IO.BLOCK_BUF)
      val stdOut = newOut (PFS.stdout, "<stdout>", true)
      local
	val readWrite =
	  let open PFS.S
	  in flags [irusr, iwusr, irgrp, iwgrp, iroth, iwoth]
	  end
      in
	fun openOut file =
	  let
	    val fd = PFS.createf (file, PIO.O_WRONLY, 
				  PFS.O.flags (PFS.O.trunc::fileTypeFlags), 
				  readWrite)
	  in 
	    newOut (fd, file, false)
	  end
          handle exn => liftExn file "openOut" exn
	fun openAppend file =
	  let
	    val fd = PFS.createf (file, PIO.O_WRONLY,
				  PFS.O.flags (PFS.O.append::fileTypeFlags),
				  readWrite)
	  in
	    newOut (fd, file, true)
	  end
          handle exn => liftExn file "openAppend" exn
      end
      val newOut = fn fd => newOut (fd, "<not implemented>", false)
      val outFd = SIO.outFd o getOutstream

      (*---------------*)
      (*   instream   *)
      (*---------------*)

      fun newIn (fd, name) =
	let 
	  val reader = mkReader {fd = fd, name = name, initBlkMode = true}
	  val instream = SIO.mkInstream (reader, empty)
	in
	  mkInstream instream
	end
      val stdIn = newIn (PFS.stdin, "<stdin>")
      fun openIn file =
	let 
	  val fd = PFS.openf (file, PIO.O_RDONLY, 
			      PFS.O.flags fileTypeFlags)
	in 
	  newIn (fd, file)
	end
        handle exn => liftExn file "newIn" exn
      val newIn = fn fd => newIn (fd, "<not implemented>")
      val inFd = SIO.inFd o getInstream
   end

signature IMPERATIVE_IO_ARG = 
   sig
      structure StreamIO: STREAM_IO
      structure Vector: MONO_VECTOR
      structure Array: MONO_ARRAY
      sharing type StreamIO.elem = Vector.elem = Array.elem
      sharing type StreamIO.vector = Vector.vector = Array.vector
   end

functor ImperativeIO(S: IMPERATIVE_IO_ARG): IMPERATIVE_IO = 
  ImperativeIOExtra(open S
		    structure StreamIO =
		      struct
			open StreamIO
			fun equalsIn _ = raise (Fail "<equalsIn>")
			fun instreamReader _ = raise (Fail "<instreamReader>")
			fun equalsOut _ = raise (Fail "<equalsOut>")
			fun outstreamWriter _ = raise (Fail "<outstreamWriter>")
			fun inputLine _ = raise (Fail "<inputLine>")
			fun outputSlice _ = raise (Fail "<outputSlice>")
		      end
		    fun openVector _ = raise (Fail "<openVector>"))
