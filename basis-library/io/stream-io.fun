signature STREAM_IO_EXTRA_ARG = 
   sig
      structure Cleaner: CLEANER
      structure PrimIO: PRIM_IO_EXTRA
      structure Array: MONO_ARRAY
      structure Vector: MONO_VECTOR
      sharing type PrimIO.elem = Array.elem = Vector.elem
      sharing type PrimIO.vector = Array.vector = Vector.vector
      sharing type PrimIO.array = Array.array
      val someElem: PrimIO.elem
   end

functor StreamIOExtra (S: STREAM_IO_EXTRA_ARG): STREAM_IO_EXTRA =
   struct
      open S

      structure PIO = PrimIO
      structure A = Array
      structure V = Vector

      type elem = PIO.elem
      type vector = PIO.vector
      type reader = PIO.reader
      type writer = PIO.writer
      type pos = PIO.pos

      fun liftExn name function cause = raise IO.Io {name = name,
						     function = function,
						     cause = cause}

      (*---------------*)
      (*   outstream   *)
      (*---------------*)

      datatype buf = Buf of {size: int ref,
			     array: A.array}
      datatype buffer_mode = NO_BUF
                           | LINE_BUF of buf
                           | BLOCK_BUF of buf
      fun newLineBuf bufSize =
	LINE_BUF (Buf {size = ref 0, 
		       array = A.array (bufSize, someElem)})
      fun newBlockBuf bufSize =
	BLOCK_BUF (Buf {size = ref 0, 
			array = A.array (bufSize, someElem)})

      datatype state = Active | Terminated | Closed
      fun active state = case state of Active => true | _ => false
      fun terminated state = not (active state)
      fun closed state = case state of Closed => true | _ => false

      datatype outstream = Out of {writer: writer,
				   augmented_writer: writer,
				   state: state ref,
				   buffer_mode: buffer_mode ref}

      val openOutstreams : outstream list ref = ref []

      fun outstreamSel (Out v, sel) = sel v
      fun writerSel (PIO.WR v, sel) = sel v
      fun outstreamWriter os = outstreamSel (os, #writer)
      fun outstreamName os = writerSel (outstreamWriter os, #name)

      fun flushGen (writeSeq: {buf: 'a, i: int, sz: int option} -> int,
		    {buf: 'a, i: int, sz: int}) =
	let
	  val max = i + sz
	  fun loop i =
	    if i = max
	      then ()
	      else let val j = writeSeq {buf = buf, i = i, sz = SOME (max - i)}
		   in 
		     if j = 0
		       then raise (Fail "partial write")
		       else loop (i + j)
		   end
	in loop i
	end

      fun flushVec (writer, x) =
	let val writeVec = writerSel (writer, #writeVec)
	in
	  case writeVec of
	    SOME writeVec => flushGen (writeVec, x)
	  | NONE => raise IO.BlockingNotSupported
	end
      fun flushArr (writer, x) =
	let val writeArr = writerSel (writer, #writeArr)
	in
	  case writeArr of
	    SOME writeArr => flushGen (writeArr, x)
	  | NONE => raise IO.BlockingNotSupported
	end

      fun flushBuf (writer, Buf {size, array}) =
	let val size' = !size 
	in 
	  size := 0;
	  flushArr (writer, {buf = array, i = 0, sz = size'})
	end

      fun output (os as Out {augmented_writer, 
			     state, 
			     buffer_mode, ...}, v) =
	if terminated (!state)
	  then liftExn (outstreamName os) "output" IO.ClosedStream
	  else let
		 fun put () = flushVec (augmented_writer, 
					{buf = v, i = 0, sz = V.length v})
		 fun doit (buf as Buf {size, array}, maybe) =
		   let
		     val curSize = !size
		     val newSize = curSize + V.length v
		   in
		     if newSize >= A.length array orelse maybe ()
		       then (flushBuf (augmented_writer, buf); put ())
		       else (A.copyVec {src = v, dst = array, di = curSize};
			     size := newSize)
		   end
	       in
		 case !buffer_mode of
		   NO_BUF => put ()
		 | LINE_BUF buf => doit (buf, fn () => true)
		 | BLOCK_BUF buf => doit (buf, fn () => false)
	       end
	handle exn => liftExn (outstreamName os) "output" exn

      local
	val buf1 = A.array (1, someElem)
      in
	fun output1 (os as Out {augmented_writer, 
				state,
				buffer_mode, ...}, c) =
	  if terminated (!state)
	    then liftExn (outstreamName os) "output" IO.ClosedStream
	    else let
		   fun doit (buf as Buf {size, array}, maybe) =
		     let
		       val _ = if 1 + !size >= A.length array
				 then flushBuf (augmented_writer, buf)
				 else ()
		       val _ = A.update (array, !size, c)
		       val _ = size := !size + 1
		       val _ = if maybe
				 then flushBuf (augmented_writer, buf)
				 else () 
		     in
		       ()
		     end
		 in
		   case !buffer_mode of
		     NO_BUF => (A.update (buf1, 0, c);
				flushArr (augmented_writer,
					  {buf = buf1, i = 0, sz = 1}))
		   | LINE_BUF buf => doit (buf, false)
		   | BLOCK_BUF buf => doit (buf, false)
		 end
	  handle exn => liftExn (outstreamName os) "output1" exn
      end

      fun flushOut (os as Out {augmented_writer, 
			       state, 
			       buffer_mode, ...}) =
	if terminated (!state)
	  then ()
	  else case !buffer_mode of
	         NO_BUF => ()
	       | LINE_BUF buf => flushBuf (augmented_writer, buf)
	       | BLOCK_BUF buf => flushBuf (augmented_writer, buf)
	handle exn => liftExn (outstreamName os) "flushOut" exn

      fun closeOut (os as Out {state, ...}) =
	if closed (!state)
	  then ()
	  else (flushOut os;
		openOutstreams := List.filter
                                  (fn Out {state = state', ...} =>
				   state <> state')
				  (!openOutstreams);
		if terminated (!state)
		  then (writerSel (outstreamWriter os, #close)) ()
		  else ();
		state := Closed)
	handle exn => liftExn (outstreamName os) "closeOut" exn

      fun getBufferMode (os as Out {buffer_mode, ...}) =
	case !buffer_mode of
	  NO_BUF => IO.NO_BUF
	| LINE_BUF _ => IO.LINE_BUF
	| BLOCK_BUF _ => IO.BLOCK_BUF

      fun setBufferMode (os as Out {buffer_mode, ...}, mode) =
	case mode of
	  IO.NO_BUF => (flushOut os;
			buffer_mode := NO_BUF)
	| IO.LINE_BUF => let
			   fun doit () = 
			     buffer_mode := 
			     newLineBuf (writerSel (outstreamWriter os, #chunkSize))
			 in
			   case !buffer_mode of
			     NO_BUF => doit ()
			   | LINE_BUF _ => ()
			   | BLOCK_BUF _ => (flushOut os; doit ())
			 end
	| IO.BLOCK_BUF => let
			    fun doit () = 
			      buffer_mode := 
			      newBlockBuf (writerSel (outstreamWriter os, #chunkSize))
			  in
			    case !buffer_mode of
			      NO_BUF => doit ()
			    | LINE_BUF _ => (flushOut os; doit ())
			    | BLOCK_BUF _ => ()
			  end

      fun mkOutstream (writer, buffer_mode) =
	let
	  val _ = Cleaner.addNew
	          (Cleaner.atExit, fn () =>
		   List.app (fn os =>
			     closeOut os) (!openOutstreams))
	  val bufSize = writerSel (writer, #chunkSize)
	  val os = Out {writer = writer,
			augmented_writer = PIO.augmentWriter writer,
			state = ref Active,
			buffer_mode = ref (case buffer_mode of
					     IO.NO_BUF => NO_BUF
					   | IO.LINE_BUF => newLineBuf bufSize
					   | IO.BLOCK_BUF => newBlockBuf bufSize)}
	  val _ = openOutstreams := os :: (!openOutstreams)
	in
	  os
	end

      fun getWriter (os as Out {writer, state, buffer_mode, ...}) =
	if closed (!state)
	  then liftExn (outstreamName os) "getWriter" IO.ClosedStream
	  else (flushOut os;
		state := Terminated;
		(writer, case !buffer_mode of
		           NO_BUF => IO.NO_BUF
			 | LINE_BUF _ => IO.LINE_BUF
			 | BLOCK_BUF _ => IO.BLOCK_BUF))

      fun outFd (os as Out {writer, ...}) =
	case writerSel (writer, #ioDesc) of
	  SOME ioDesc => valOf (Posix.FileSys.iodToFD ioDesc)
	| NONE => liftExn (outstreamName os) "outFd" (Fail "<no ioDesc>")

      datatype out_pos = OutPos of {pos: pos,
				    outstream: outstream}

      fun getPosOut (os as Out {...}) =
	(flushOut os;
	 case writerSel (outstreamSel (os, #writer), #getPos) of
	   NONE => liftExn (outstreamName os) "getPosOut" IO.RandomAccessNotSupported
	 | SOME getPos => OutPos {pos = getPos (),
				  outstream = os})

      fun setPosOut (OutPos {pos, outstream = os}) =
	(flushOut os;
	 case writerSel (outstreamSel (os, #writer), #setPos) of
	   NONE => liftExn (outstreamName os) "setPosOut" IO.RandomAccessNotSupported
	 | SOME setPos => setPos pos;
	 os)

      fun filePosOut (OutPos {pos, ...}) = pos

      (*---------------*)
      (*   instream    *)
      (*---------------*)

      datatype chain = End
	             | Eos of {next: state ref}
	             | Link of {inp: V.vector, pos: int, next: state ref}
      and state = Active of chain ref 
	        | Truncated 
	        | Closed
      fun active state = case state of Active _ => true | _ => false
      fun truncated state = not (active state)
      fun closed state = case state of Closed => true | _ => false

      datatype instream = In of {reader: reader,
				 augmented_reader: reader,
				 state: state ref,
				 tail: state ref ref}

      val openInstreams : instream list ref = ref []

      fun updateState (In {reader, augmented_reader, tail, ...}, state) =
	In {reader = reader,
	    augmented_reader = augmented_reader,
	    tail = tail,
	    state = state}

      fun instreamSel (In v, sel) = sel v
      fun readerSel (PIO.RD v, sel) = sel v
      fun instreamReader is = instreamSel (is, #reader)
      fun instreamName is = readerSel (instreamReader is, #name)

      val empty = V.tabulate (0, fn _ => someElem)

      fun extend function (is as In {augmented_reader, tail, ...}) blocking =
	case !(!tail) of
	  Active (r as ref End) =>
	    let
	      fun link inp = let
			       val next = ref (Active (ref End))
			       val this = if V.length inp = 0
					    then Eos {next = next}
					    else Link {inp = inp,
						       pos = 0,
						       next = next}
			       val _ = r := this
			       val _ = tail := next
			     in
			       SOME (inp, updateState (is, next))
			     end
	      fun readAndLink i =
		if blocking 
		  then case readerSel (augmented_reader, #readVec) of
		         NONE => liftExn (instreamName is) 
			                 function 
					 IO.BlockingNotSupported
		       | SOME readVec => 
			   let
			     val inp = readVec i
			               handle exn =>
				       liftExn (instreamName is) function exn
			   in
			     link inp
			   end
		  else case readerSel (augmented_reader, #readVecNB) of
		         NONE => liftExn (instreamName is) 
			                 function 
					 IO.NonblockingNotSupported
		       | SOME readVecNB =>
			   let
			     val inp = readVecNB i
			               handle exn =>
				       liftExn (instreamName is) function exn
			   in
			     case inp of
			       NONE => NONE
			     | SOME inp => link inp
			   end
	    in
	      readAndLink (readerSel (instreamReader is, #chunkSize))
	    end
	| _ => liftExn (instreamName is) function Match

      fun extendB function is = valOf (extend function is true)
      fun extendNB function is = extend function is false

      fun input (is as In {augmented_reader, state, ...}) =
	case !state of
	  Active (ref (Link {inp, pos, next})) => 
	    let
	      val inp = V.tabulate
		        (V.length inp - pos, 
			 fn i => V.sub (inp, pos + i))
	    in
	      (inp, updateState (is, next))
	    end
	| Active (ref (Eos {next})) => (empty, updateState (is, next))
	| Active (ref End) => extendB "input" is
	| Truncated => (empty, is)
	| Closed => (empty, is)

      fun inputN (is, n) = 
	if n < 0 orelse n > V.maxLen 
	  then raise Size
	  else let
		 fun finish (inps, is) =
		   let val inp = Vector.concat (List.rev inps)
		   in (inp, is)
		   end
		 fun loop (is as In {state, ...}, inps, n) =
		   case !state of
		     Active (ref (Link {inp, pos, next})) =>
		       if pos + n < V.length inp
			 then let
				val link = Link {inp = inp,
						 pos = pos + n,
						 next = next}
				val next = ref (Active (ref link))
				val inp' = Vector.tabulate
				  (n, fn i => V.sub (inp, pos + i))
				val inps = inp'::inps
			      in
				finish (inps, updateState (is, next))
			      end
			 else let
				val inp' = Vector.tabulate
				           (V.length inp - pos, 
					    fn i => V.sub (inp, i))
			      in
				loop (updateState (is, next), 
				      inp'::inps, n - (V.length inp - pos))
			      end
		   | Active (ref (Eos {next})) => 
		       finish (inps, if n > 0
				       then updateState(is, next)
				       else is)
		   | Active (ref End) => 
		       let val _ = extendB "canInput" is 
		       in loop (is, inps, n)
		       end
		   | _ => finish (inps, is)
	       in
		 loop (is, [], n)
	       end

      fun input1 is =
	let
	  val (inp, is') = inputN (is, 1)
	in
	  if V.length inp = 0
	    then NONE
	    else SOME (V.sub (inp, 0), is')
	end

      fun inputAll is =
	let
	  fun loop (is, ac) =
	    let val (inp, is') = input is
	    in
	      if V.length inp = 0
		then (V.concat (List.rev ac), is')
		else loop (is', inp::ac)
	    end
	in
	  loop (is, [])
	end

      fun inputLine is = raise (Fail "<not implemented>")

      fun canInput (is as In {state, ...}, n) =
	if n < 0 orelse n > V.maxLen
	  then raise Size
	  else let
		 fun start (is, inp) = add (is, [], inp, 0)
		 and add (is, inps, inp, k) =
		   let 
		     val l = V.length inp
		     val inps = inp::inps
		   in
		     if k + l > n
		       then finish (is, inps, n)
		       else loop (is, inps, k + l)
		   end
		 and loop (is, inps, k) =
		   case extendNB "canInput" is of
		     NONE => finish (is, inps, k)
		   | SOME (inp, is') => if V.length inp = 0
					  then finish (is, inps, k)
					  else add (is', inps, inp, k)
		 and finish (is, inps, k) =
		   let
		     val inp = V.concat (List.rev inps)
		     val next = instreamSel (is, #state)
		     val this = Active (ref (Link {inp = inp,
						   pos = 0,
						   next = next}))
		     val _ = state := this
		   in
		     SOME k
		   end
	       in 
		 case !state of
		   Active (ref (Link {inp, pos, next})) => 
		     SOME (Int.min (V.length inp - pos, n))
		 | Active (ref End) => 
		     (case extendNB "canInput" is of
			NONE => NONE
		      | SOME (inp, is') => if V.length inp = 0
					     then SOME 0
					     else start (is', inp))
		 | _ => SOME 0
	       end

      fun closeIn (is as In {state, tail, ...}) =
	case !(!tail) of
	  Active (ref (End)) =>
	    (!tail := Closed;
	     openInstreams := List.filter
	                      (fn In {state = state', ...} =>
			       state <> state')
			      (!openInstreams);
	     (readerSel (instreamReader is, #close)) ())
	| _ => ()
	handle exn => liftExn (instreamName is) "closeIn" exn

      fun endOfStream is =
	let val (inp, _) = input is
	in V.length inp = 0
	end

      fun mkInstream (reader, v) =
	let
	  val _ = Cleaner.addNew
	          (Cleaner.atExit, fn () =>
		   List.app (fn is =>
			     closeIn is) (!openInstreams))
	  val next = ref (Active (ref End))
	  val this = if V.length v = 0
		       then next
		       else ref (Active (ref (Link {inp = v, pos = 0, next = next})))
	  val is = In {reader = reader,
		       augmented_reader = PIO.augmentReader reader,
		       state = this,
		       tail = ref next}
	  val _ = openInstreams := is :: (!openInstreams)
	in
	  is
	end

      fun getReader (is as In {reader, tail, ...}) =
	(case !(!tail) of
	   Active (ref End) => !tail := Truncated
	 | _ => liftExn (instreamName is) "getReader" IO.ClosedStream;
	 (reader, empty))

      fun inFd (is as In {reader, ...}) =
	case readerSel (reader, #ioDesc) of
	  SOME ioDesc => valOf (Posix.FileSys.iodToFD ioDesc)
	| NONE => liftExn (instreamName is) "inFd" (Fail "<no ioDesc>")

      fun filePosIn (is as In {reader, tail, ...}) =
	case !(!tail) of
	  Active (ref End) =>
	    (case readerSel (reader, #getPos) of
	       NONE => raise IO.RandomAccessNotSupported
	     | SOME getPos => getPos ())
	| _ => raise IO.ClosedStream
        handle exn => liftExn (instreamName is) "filePosIn" exn
   end

signature STREAM_IO_ARG = 
   sig
      structure PrimIO: PRIM_IO
      structure Array: MONO_ARRAY
      structure Vector: MONO_VECTOR
      sharing type PrimIO.elem = Array.elem = Vector.elem
      sharing type PrimIO.vector = Array.vector = Vector.vector
      sharing type PrimIO.array = Array.array
      val someElem: PrimIO.elem
   end
functor StreamIO(S: STREAM_IO_ARG): STREAM_IO = 
  StreamIOExtra(open S
		structure Cleaner = EmptyCleaner)