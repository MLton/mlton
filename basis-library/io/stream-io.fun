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

      (*---------------*)
      (*   outstream   *)
      (*---------------*)

      datatype buf = Buf of {size: int ref,
			     array: Array.array}
      datatype buffer_mode = NO_BUF
                           | LINE_BUF of buf
                           | BLOCK_BUF of buf
      fun newLineBuf bufSize =
	LINE_BUF {size = ref 0,
		  array = Array.array (bufSize, someElem)}
      fun newBlockBuf bufSize =
	BLOCK_BUF {size = ref 0,
		   array = Array.array (bufSize, someElem)}

      datatype state = Active | Terminated | Closed
      fun active state =
	case state of
	  Active => true
	| _ => false
      fun terminated state = not (active state)
      fun closed state =
	case state of 
	  Closed => true
	| _ => false

      datatype outstream = Out of {writer: writer,
				   augmented_writer: writer,
				   state: state ref,
				   buffer_mode: buffer_mode ref}

      fun outstreamSel (os, sel) =
	let Out v = os
	in sel v
	end
      fun writerSel (writer, sel) =
	let PrimIO.WR wr = writer
	in sel wr
	end

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
	let PrimIO.WR {name, writeVec, ...} = writer
	in
	  case writeVec of
	    SOME writeVec => flushGen (writeVec, x)
	  | NONE => raise IO.BlockingNotSupported
	end
      fun flushArr (writer, x) =
	let PrimIO.WR {name, writeArr, ...} = writer
	in
	  case writeArr of
	    SOME writeArr => flushGen (writeArr, x)
	  | NONE => raise IO.BlockingNotSupported
	end

      fun flushBuf (writer, Buf {size, array}) =
	let size' = !size 
	in 
	  size := 0;
	  flushArr (writer, {buf = array, i = 0, sz = size'})
	end

      fun output (os as Out {augmented_writer, 
			     state, 
			     buffer_mode, ...}, v) =
	if terminated (!state)
	  then raise IO.Io {name = outstreamName os,
			    function = "output",
			    cause = IO.ClosedStream}
	  else let
		 fun put () = flushVec (augmented_writer, 
					{buf = v, i = 0, sz = Vector.length v})
		 fun doit (buf as Buf {size, array}, maybe) =
		   let
		     val curSize = !size
		     val newSize = curSize + Vector.length v
		   in
		     if newSize >= Array.length array orelse maybe ()
		       then (flushBuf (augmented_writer, buf); put ())
		       else (Array.copyVec {src = v, dst = array, di = curSize};
			     size := newSize)
		   end
	       in
		 case !buffer_mode of
		   NO_BUF => put ()
		 | LINE_BUF buf => doit (buf, fn () => true)
		 | BLOCK_BUF buf => doit (buf, fn () => false)
	       end
	handle exn => raise IO.Io {name = outstreamName os,
				   function = "output",
				   cause = exn}

      local
	val buf1 = Array.array (1, someElem)
      in
	fun output1 (os as Out {augmented_writer, 
				state,
				buffer_mode, ...}, c) =
	  if terminated (!state)
	    then raise IO.Io {name = outstreamName os,
			      function = "output",
			      cause = IO.ClosedStream}
	    else let
		   fun doit (buf as Buf {size, array}, maybe) =
		     let
		       val _ = if 1 + !size >= Array.length array
				 then flushBuf (augmented_writer, buf)
				 else ()
		       val _ = Array.update (array, !size, c)
		       val _ = size := !size + 1
		       val _ = if maybe
				 then flushBuf (augmented_writer, buf)
				 else ()				   
		     in
		       case !buffer_mode of
			 NO_BUF => (Array.update (buf1, 0, c);
				    flushArr (augmented_writer,
					      {buf = buf1, i = 0, sz = 1}))
	               | LINE_BUF buf => doit (buf, true)
		       | BLOCK_BUF buf => doit (buf, false)
		     end
		   
		 end
	  handle exn => raise IO.Io {name = outstreamName os,
				     function = "output1",
				     cause = exn}
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
	handle exn => raise IO.Io {name = outstreamName os,
				   function = "flushOut",
				   cause = exn}

      fun closeOut (os as Out {state, ...}) =
	if closed (!state)
	  then ()
	  else (flushOut ();
		if terminated (!state)
		  then (writerSel (outstreamWriter os, #close)) ()
		  else ();
		state := Closed)
	handle exn => IO.Io {name = oustreamName os,
			     function = "closeOut",
			     exn = exn}

      fun getBufferMode (os as Out {buffer_mode, ...}) =
	case buffer_mode of
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
			   | LINE_BUF => ()
			   | BLOCK_BUF => (flushOut os; doit ())
			 end
	| IO.BLOCK_BUF => let
			    fun doit () = 
			      buffer_mode := 
			      newBlockBuf (writerSel (outstreamWriter os, #chunkSize))
			  in
			    case !buffer_mode of
			      NO_BUF => doit ()
			    | LINE_BUF => (flushOut os; doit ())
			    | BLOCK_BUF => ()
			  end

      fun mkOutstream (writer, buffer_mode) =
	let
	  val bufSize = writerSel (writer, #chunkSize)
	in
	  Out {writer = writer,
	       augmented_writer = PrimIO.augmentWriter writer,
	       state = ref Active,
	       buffer_mode = case buffer_mode of
	                       IO.NO_BUF => NO_BUF
			     | IO.LINE_BUF => newLineBuf bufSize
			     | IO.BLOCK_BUF => newBlockBuf bufSize}
	end

      fun getWriter (os as Out {state, buffer_mode, ...}) =
	if closed (!state)
	  then raise IO.Io {name = oustreamName os,
			    function = "getWriter",
			    cause = IO.ClosedStream}
	  else (flushOut os;
		state := Terminated;
		(writer, case buffer_mode of
		           NO_BUF => IO.NO_BUF
			 | LINE_BUF => IO.LINE_BUF
			 | BLOCK_BUF _ => IO.BLOCK_BUF))

      datatype pos_out = PosOut of {pos: pos,
				    outstream: outstream}

      fun getPosOut (os as Out {...}) =
	(flushOut os;
	 case writerSel (outstreamSel (os, #writer), #getPos) of
	   NONE => raise IO.Io {name = outstreamName os,
				function = "getPosOut",
				cause = IO.RandomAccessNotSupported}
	 | SOME getPos => PosOut {pos = getPos (),
				  outstream = os})

      fun setPosOut (PosOut {pos, outstream = os}) =
	(flushOut os;
	 case writerSel (outstreamSel (os, #writer), #setPos) of
	   NONE => raise IO.Io {name = outstreamName os,
				function = "setPosOut",
				cause = IO.RandomAccessNotSupported}
	 | SOME setPos => setPos pos;
	 os)

      fun filePosOut (PosOut {pos, ...}) = pos

      (*---------------*)
      (*   instream    *)
      (*---------------*)

      datatype buf = Buf of {array: Array.array,
			     first: int ref,
			     last: int ref}
      datatype buffer_mode = NO_BUF
                           | BUF of buf
      fun newBufferMode bufSize =
	if bufSize = 1
	  then NO_BUF
	  else BUF (Buf {array = Array.array (bufSize, someElem),
			 first = ref 0,
			 last = ref 0})

      datatype chain = Chain of {inp: Vector.vector,
				 next: state ref}
      and state = Active of chain option ref | Truncated | Closed
      fun active state =
	case state of
	  Active => true
	| _ => false
      fun truncated state = not (active state)
      fun closed state =
	case state of 
	  Closed => true
	| _ => false

      datatype instream = In of {reader: reader,
				 augmented_reader: reader,
				 buffer_mode: buffer_mode,
				 state: state ref}

      fun instreamSel (os, sel) =
	let In v = is
	in sel v
	end

      fun readerSel (reader, sel) =
	let PrimIO.RD rd = reader
	in sel rd
	end

      fun instreamName is =
	readerSel (instreamSel (is, #reader), #name)

      fun input (is as In {state, ...}) =
	let
	in
	end

      fun input1 (is as In {pos, chain, ...}) =
	let
	in
	end

      fun inputN (is as In {...}, n) =
	let
	in
	end

      fun inputAll (is as In {...}) =
	let
	in
	end

      fun canInput (is as In {...}) =
	let
	in
	end

      fun closeIn (is as In {state, ...}) =
	let
	  fun truncate state =
	    case !state of
	      Active (ref NONE) => state := Closed
	    | Active (ref (SOME (Chain {next, ...}))) => truncate next
	    | Truncated => ()
	    | Closed => ()
	in
	  if closed (!state)
	    then ()
	    else (truncateState;
		  (readerSel (outstreamReader is, #close)) ())
	end
	handle exn => IO.Io {name = instreamName is,
			     function = "closeIn",
			     exn = exn}

      fun endOfStream (is as In {...}) =
	let
	in
	end

      fun mkInstream (reader, v) =
	In {reader = reader,
	    augmented_reader = PrimIO.augmentReader reader,
	    buffer_mode = newBufferMode (readerSel (instreamReader is, #chunkSize)),
	    state = let
		      val next = ref (SOME (Active (ref NONE)))
		      val chain = Chain {imp = v, next = next}
		      val state = ref (Active (ref (SOME chain)))
		    in 
		      state
		    end}

      fun getReader (is as In {reader, state, ...}) =
	let
	  fun truncate (state, ac) =
	    case !state of
	      Active (ref NONE) => (state := Truncated;
				    Vector.concat (List.rev ac))
	    | Active (ref (SOME (Chain {inp, next}))) => truncate (next, inp::ac)
	    | _ => raise IO.Io {name = instreamName is,
				function = "getReader",
				cause = IO.ClosedStream}
	in
	  (reader, truncate state)
	end

      fun filePosIn (is as In {...}) =
   end