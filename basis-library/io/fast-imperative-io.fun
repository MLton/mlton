signature FAST_IMPERATIVE_IO_EXTRA_ARG =
   sig
      structure StreamIO: STREAM_IO_EXTRA
      structure BufferI: BUFFER_I_EXTRA
      structure Vector: MONO_VECTOR
      structure Array: MONO_ARRAY
      sharing type StreamIO.elem = BufferI.elem = Vector.elem = Array.elem
      sharing type StreamIO.vector = BufferI.vector = Vector.vector = Array.vector
      sharing type StreamIO.instream = BufferI.instream
      sharing type StreamIO.reader = BufferI.reader
      sharing type StreamIO.pos = BufferI.pos
   end

functor FastImperativeIOExtra
        (S: FAST_IMPERATIVE_IO_EXTRA_ARG): FAST_IMPERATIVE_IO_EXTRA =
   struct
      open S

      structure SIO = StreamIO
      structure BI = BufferI
      structure V = Vector
      structure A = Array

      type vector = SIO.vector
      type elem = SIO.elem

      fun liftExn name function cause = raise IO.Io {name = name,
						     function = function,
						     cause = cause}

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

      datatype instream' = Buffer of BI.inbuffer
	                 | Stream of SIO.instream
      datatype instream = In of instream' ref

      fun equalsIn (In is1, In is2) = is1 = is2

      fun input (In is) =
	case !is of
	  Buffer b => BI.input b
	| Stream s => let val (v, s') = SIO.input s
		      in is := Stream s'; v
		      end
      (* input1 will never move past a temporary end of stream *)
      fun input1 (In is) =
	case !is of
	  Buffer b => BI.input1 b
	| Stream s => (case SIO.input1 s of
			 SOME (c, s') => (is := Stream s'; SOME c)
		       | NONE => NONE)
      (* input1 will move past a temporary end of stream *)
      fun input1 (In is) =
	case !is of
	  Buffer b => BI.input1 b
	| Stream s => (case SIO.input1' s of
			 (c, s') => (is := Stream s'; c))
      fun inputN (In is, n) = 
	case !is of
	  Buffer b => BI.inputN (b, n)
	| Stream s => let val (v, s') = SIO.inputN (s, n)
		      in is := Stream s'; v
		      end
      fun inputAll (In is) =
	case !is of
	  Buffer b => BI.inputAll b
	| Stream s => let val (v, s') = SIO.inputAll s
		      in is := Stream s'; v
		      end
      fun inputLine (In is) =
	case !is of
	  Buffer b => BI.inputLine b
	| Stream s => let val (v, s') = SIO.inputLine s
		      in is := Stream s'; v
		      end
      fun canInput (In is, n) = 
	case !is of
	  Buffer b => BI.canInput (b, n)
	| Stream s => SIO.canInput (s, n)
      fun lookahead (In is) =
	case !is of
	  Buffer b => BI.lookahead b
	| Stream s => Option.map (fn (c, s') => c) (SIO.input1 s)
      fun closeIn (In is) =
	case !is of
	  Buffer b => BI.closeIn b
	| Stream s => SIO.closeIn s
      fun endOfStream (In is) =
	case !is of
	  Buffer b => BI.endOfStream b
	| Stream s => SIO.endOfStream s
      fun mkInbuffer b = In (ref (Buffer b))
      fun mkInstream s = In (ref (Stream s))
      fun getInstream (In is) =
	case !is of
	  Buffer b => let val s = BI.getInstream b
		      in is := Stream s; s
		      end
	| Stream s => s
      fun setInstream (In is, s) = is := Stream s
      fun withIn (In is, bf, sf) =
	case !is of
	  Buffer b => bf b
	| Stream s => sf s

      fun openVector v = mkInbuffer (BI.openVector v)

      fun scanStream f is =
	case f SIO.input1 (getInstream is) of
	  NONE => NONE
	| SOME (v, s') => (setInstream (is, s'); SOME v)
   end


signature FAST_IMPERATIVE_IO_EXTRA_FILE_ARG =
   sig
      structure StreamIO: STREAM_IO_EXTRA_FILE
      structure BufferI: BUFFER_I_EXTRA_FILE
      structure Vector: MONO_VECTOR
      structure Array: MONO_ARRAY
      sharing type StreamIO.elem = BufferI.elem = Vector.elem = Array.elem
      sharing type StreamIO.vector = BufferI.vector = Vector.vector = Array.vector
      sharing type StreamIO.instream = BufferI.instream
      sharing type StreamIO.reader = BufferI.reader
      sharing type StreamIO.pos = BufferI.pos 

      val chunkSize: int
      val fileTypeFlags: Posix.FileSys.O.flags list
      val mkReader: {fd: Posix.FileSys.file_desc,
		     name: string,
		     initBlkMode: bool} -> StreamIO.reader
      val mkWriter: {fd: Posix.FileSys.file_desc,
		     name: string,
		     appendMode: bool,
		     initBlkMode: bool,
		     chunkSize: int} -> StreamIO.writer
   end

functor FastImperativeIOExtraFile
        (S: FAST_IMPERATIVE_IO_EXTRA_FILE_ARG): FAST_IMPERATIVE_IO_EXTRA_FILE =
   struct
      structure ImperativeIO = FastImperativeIOExtra(open S)
      open ImperativeIO
      open S
      structure SIO = StreamIO
      structure BI = BufferI
      structure V = Vector

      structure PIO = Posix.IO
      structure PFS = Posix.FileSys

      fun liftExn name function cause = raise IO.Io {name = name,
						     function = function,
						     cause = cause}

      val empty = V.fromList []

      (*---------------*)
      (*   outstream   *)
      (*---------------*)

      fun newOut {fd, name, appendMode, 
		  buffer_mode, atExit} =
	let
	  val writer = mkWriter {fd = fd, name = name,
				 appendMode = appendMode, 
				 initBlkMode = true,
				 chunkSize = chunkSize}
	  val outstream = SIO.mkOutstream'' {writer = writer, 
					     closed = false,
					     buffer_mode = buffer_mode,
					     atExit = atExit}
	in
	  mkOutstream outstream
	end
      val stdErr = newOut {fd = PFS.stderr,
			   name = "<stderr>", 
			   appendMode = true, 
			   buffer_mode = IO.NO_BUF,
			   atExit = {close = false}}
      val newOut = fn {fd, name, appendMode, atExit} =>
	newOut {fd = fd, name = name, appendMode = appendMode,
		buffer_mode = if Posix.ProcEnv.isatty fd
				then IO.LINE_BUF
				else IO.BLOCK_BUF,
		atExit = atExit}
      val stdOut = newOut {fd = PFS.stdout, 
			   name = "<stdout>", 
			   appendMode = true,
			   atExit = {close = false}}
      val newOut = fn {fd, name, appendMode} =>
	newOut {fd = fd, name = name, appendMode = appendMode,
		atExit = {close = true}}
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
	    newOut {fd = fd, 
		    name = file, 
		    appendMode = false}
	  end
          handle exn => liftExn file "openOut" exn
	fun openAppend file =
	  let
	    val fd = PFS.createf (file, PIO.O_WRONLY,
				  PFS.O.flags (PFS.O.append::fileTypeFlags),
				  readWrite)
	  in
	    newOut {fd = fd, 
		    name = file, 
		    appendMode = true}
	  end
          handle exn => liftExn file "openAppend" exn
      end
      val newOut = fn (fd, name) => newOut {fd = fd, 
					    name = name,
					    appendMode = false}
      val outFd = SIO.outFd o getOutstream

      (*---------------*)
      (*   instream   *)
      (*---------------*)

      fun newIn {fd, name, buffer_contents, atExit} =
	let 
	  val reader = mkReader {fd = fd, name = name, initBlkMode = true}
	  val inbuffer = BI.mkInbuffer'' {reader = reader,
					  closed = false,
					  buffer_contents = buffer_contents,
					  atExit = atExit}
	in
	  mkInbuffer inbuffer
	end
      val newIn = fn {fd, name, atExit} =>
	newIn {fd = fd, name = name, buffer_contents = NONE, atExit = atExit}
      val newIn = fn {fd, name} =>
	newIn {fd = fd, name = name, atExit = {close = true}}
      val stdIn = newIn {fd = PFS.stdin, 
			 name = "<stdin>"}
      fun openIn file =
	let 
	  val fd = PFS.openf (file, PIO.O_RDONLY, 
			      PFS.O.flags fileTypeFlags)
	in 
	  newIn {fd = fd, 
		 name = file}
	end
        handle exn => liftExn file "newIn" exn
      val newIn = fn (fd, name) => newIn {fd = fd, name = name}
      fun inFd is = withIn (is, BI.inFd, SIO.inFd)
   end
