signature FAST_IMPERATIVE_IO_EXTRA_ARG =
   sig
      structure StreamIO: STREAM_IO_EXTRA
      structure BufferI: BUFFER_IO_EXTRA
      sharing StreamIO = BufferI.StreamIO
      structure Vector: MONO_VECTOR
      structure Array: MONO_ARRAY
      sharing type PrimIO.reader = StreamIO.reader
      sharing type PrimIO.writer = StreamIO.writer
      sharing type PrimoIO.pos = StreamIO.pos
      sharing type PrimIO.elem = StreamIO.elem = Vector.elem = Array.elem
      sharing type PrimIO.vector = StreamIO.vector = Vector.vector = Array.vector
      sharing type PrimIO.array = Array.array
      val someElem: PrimIO.elem
      val lineElem: Vector.elem
      val isLine: Vector.elem -> bool
      val hasLine: Vector.vector -> bool
   end

functor FastImperativeIOExtra
        (S: FAST_IMPERATIVE_IO_EXTRA_ARG) =
   struct
      open S

      structure PIO = PrimIO
      structure SIO = StreamIO
      structure V = Vector
      structure A = Array

      type vector = PrimIO.vector
      type elem = PrimIO.elem

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

      structure Buf =
	 struct
	 end
   
      datatype instream' = Buf of Buf.buf
	                 | Stream of SIO.instream
      datatype instream = In of instream' ref

      fun equalsIn (In is1, In is2) = is1 = is2

      fun input (In is) =
	case !is of
	  Buf buf => Buf.input buf
	| Stream s => let val (v, s') = SIO.input s
		      in is := Stream s'; v
		      end
      (* input1 will never move past a temporary end of stream *)
      fun input1 (In is) =
	case !is of
	  Buf buf => Buf.input1 buf
	| Stream s => 
	    Option.map (fn (c,s') => (is := Stream s'; c)) (SIO.input1 s)
      (* input1 will move past a temporary end of stream *)
      fun input1 (In is) =
	case !is of
	  Buf buf => Buf.input1 buf
	| Stream s => let val (v, s') = SIO.inputN (s, 1)
		      in 
			is := Stream s';
			if V.length v = 0 then NONE else SOME (V.sub (v, 0))
		      end
      fun inputN (In is, n) = 
	case !is of
	  Buf buf => Buf.inputN (buf, n)
	| Stream s => let val (v, s') = SIO.inputN (s, n)
		      in is := Stream s'; v
		      end
      fun inputAll (In is) =
	case !is of
	  Buf buf => Buf.inputAll buf
	| Stream s => let val (v, s') = SIO.inputAll s
		      in is := Stream s'; v
		      end
      fun inputLine (In is) =
	case !is of
	  Buf buf => Buf.inputLine buf
	| Stream s => let val (v, s') = SIO.inputAll s
		      in is := Stream s'; v
		      end
      fun canInput (In is, n) = 
	case !is of
	  Buf buf => Buf.canInput (buf, n)
	| Stream s => SIO.canInput (s, n)
      fun lookahead (In is) =
	case !is of
	  Buf buf => Buf.lookahead buf
	| Stream s => Option.map (fn (c, s') => c) (SIO.input1 s)
      fun closeIn (In is) =
	case !is of
	  Buf buf => Buf.closeIn buf
	| Stream s => SIO.closeIn s
      fun endOfStream (In is) =
	case !is of
	  Buf buf => Buf.endOfStream buf
	| Stream s => SIO.endOfStream s
      fun mkInstream s = In (ref (Stream s))
      fun getInstream (In is) =
	case !is of
	  Buf buf => Buf.mkInstream buf
	| Stream s => s
      fun setInstream (In is, s') = is := Stream s'

      val empty = V.fromList []

      val openVector = fn v =>
	let
	  val instream = SIO.openVector v
	in
	  mkInstream instream
	end

      fun scanStream f is =
	case f SIO.input1 (getInstream is) of
	  NONE => NONE
	| SOME (v, s') => (setInstream (is, s'); SOME v)
   end


signature FAST_IMPERATIVE_IO_EXTRA_FILE_ARG =
   sig
      structure PrimIO: PRIM_IO
      structure StreamIO: STREAM_IO_EXTRA_FILE
      structure Vector: MONO_VECTOR
      structure Array: MONO_ARRAY
      sharing type PrimIO.reader = StreamIO.reader
      sharing type PrimIO.writer = StreamIO.writer
      sharing type PrimIO.pos = StreamIO.pos
      sharing type PrimIO.elem = StreamIO.elem = Vector.elem = Array.elem
      sharing type PrimIO.vector = StreamIO.vector = Vector.vector = Array.vector
      sharing type PrimIO.array = Array.array
      val someElem: PrimIO.elem
      val lineElem: Vector.elem
      val isLine: Vector.elem -> bool
      val hasLine: Vector.vector -> bool

      structure Cleaner: CLEANER
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
        (S: FAST_IMPERATIVE_IO_EXTRA_FILE_ARG): IMPERATIVE_IO_EXTRA_FILE =
   struct
      structure ImperativeIO = FastImperativeIOExtra(open S)
      open ImperativeIO
      open S
      structure SIO = StreamIO
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
      val newOut = fn fd => newOut {fd = fd, 
				    name = "<not implemented>", 
				    appendMode = false}
      val outFd = SIO.outFd o getOutstream

      (*---------------*)
      (*   instream   *)
      (*---------------*)

      structure Buf =
	struct
	  open Buf

	  val openInstreams : (buf * {close: bool}) list ref = ref []
	  fun mkBuf'' {reader, closed, atExit} =
	    let
	      val b = mkBuf' {reader = reader, closed = closed}
	  val mkInstream 
	end

      fun newIn {fd, name} =
	let 
	  val reader = mkReader {fd = fd, name = name, initBlkMode = true}
	  val instream = SIO.mkInstream'' {reader = reader,
					   closed = false,
					   buffer_contents = NONE,
					   atExit = {close = true}}
	in
	  mkInstream instream
	end
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
      val newIn = fn fd => newIn {fd = fd, name = "<not implemented>"}
      val inFd = SIO.inFd o getInstream
   end

functor FastImperativeIOExtra
        (S: FAST_IMPERATIVE_IO_EXTRA_ARG): IMPERATIVE_IO_EXTRA =
   FastImperativeIOExtra(open S)
