signature STREAM_IO_EXTRA_ARG = 
   sig
      structure PrimIO: PRIM_IO_EXTRA
      structure Array: MONO_ARRAY
      structure Vector: MONO_VECTOR
      sharing type PrimIO.elem = Array.elem = Vector.elem
      sharing type PrimIO.vector = Array.vector = Vector.vector
      sharing type PrimIO.array = Array.array
      val someElem: PrimIO.elem

      val lineElem : Vector.elem
      val isLine : Vector.elem -> bool 
   end

functor StreamIOExtra 
        (S: STREAM_IO_EXTRA_ARG): STREAM_IO_EXTRA =
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

      val hasLine = V.exists isLine

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

      fun equalsOut (os1 as Out {state = state1, ...},
		     os2 as Out {state = state2, ...}) = state1 = state2
	
      fun outstreamSel (Out v, sel) = sel v
      fun outstreamWriter os = outstreamSel (os, #writer)
      fun writerSel (PIO.WR v, sel) = sel v
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
		 | LINE_BUF buf => doit (buf, fn () => hasLine v)
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
		   | LINE_BUF buf => doit (buf, isLine c)
		   | BLOCK_BUF buf => doit (buf, false)
		 end
	  handle exn => liftExn (outstreamName os) "output1" exn
      end

      fun outputSlice (os, (v, i, sz)) =
	let
	  val l = case sz of
	            SOME sz => sz
		  | NONE => V.length v - i
	  val v' = V.tabulate(l, fn j => V.sub(v, i + j))
	in
	  output (os, v')
	end
        handle exn => liftExn (outstreamName os) "outputSlice" exn

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
		if terminated (!state)
		  then ()
		  else (writerSel (outstreamWriter os, #close)) ();
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

      fun mkOutstream' {writer, closed, buffer_mode} =
	let
	  val bufSize = writerSel (writer, #chunkSize)
	in
	  Out {writer = writer,
	       augmented_writer = PIO.augmentWriter writer,
	       state = ref (if closed then Closed else Active),
	       buffer_mode = ref (case buffer_mode of
				    IO.NO_BUF => NO_BUF
				  | IO.LINE_BUF => newLineBuf bufSize
				  | IO.BLOCK_BUF => newBlockBuf bufSize)}
	end
      fun mkOutstream (writer, buffer_mode) =
	mkOutstream' {writer = writer, closed = false, buffer_mode = buffer_mode}

      fun getWriter (os as Out {writer, state, buffer_mode, ...}) =
	if closed (!state)
	  then liftExn (outstreamName os) "getWriter" IO.ClosedStream
	  else (flushOut os;
		state := Terminated;
		(writer, case !buffer_mode of
		           NO_BUF => IO.NO_BUF
			 | LINE_BUF _ => IO.LINE_BUF
			 | BLOCK_BUF _ => IO.BLOCK_BUF))

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

      datatype state = Link of {inp: V.vector, next: state ref}
	             | Eos of {next: state ref}
	             | End
	             | Truncated 
	             | Closed
      fun active state = 
	case state of 
	  Link _ => true
	| Eos _ => true
	| End => true 
	| _ => false
      fun closed state = case state of Closed => true | _ => false

      datatype instream = In of {common: {reader: reader,
					  augmented_reader: reader,
					  tail: state ref ref},
				 pos: int (* !state <> Link _ ==> pos = 0 *),
				 state: state ref}

      fun equalsIn (is1 as In {common = {tail = tail1, ...}, ...}, 
		    is2 as In {common = {tail = tail2, ...}, ...}) = 
	tail1 = tail2

      fun updateState (In {common, ...}, pos, state) =
	In {common = common,
	    pos = pos,
	    state = state}

      fun instreamSel (In v, sel) = sel v
      fun instreamCommon is = instreamSel (is, #common)
      fun instreamCommonSel (is, sel) = sel (instreamCommon is)
      fun instreamReader is = instreamCommonSel (is, #reader)
      fun readerSel (PIO.RD v, sel) = sel v
      fun instreamName is = readerSel (instreamReader is, #name)

      val empty = V.tabulate (0, fn _ => someElem)
      val line = V.tabulate (1, fn _ => lineElem)

      fun extend function 
                 (is as In {common = {augmented_reader, tail, ...}, ...}) 
		 blocking =
	case !(!tail) of
	  End =>
	    let
	      fun link inp = let
			       val next = ref End
			       val this = if V.length inp = 0
					    then Eos {next = next}
					    else Link {inp = inp,
						       next = next}
			       val _ = !tail := this
			       val _ = tail := next
			     in
			       SOME (inp, updateState (is, 0, next))
			     end
	      fun doit readVec =
		let
		  val inp = readVec (readerSel (instreamReader is, #chunkSize))
		            handle exn =>
			    liftExn (instreamName is) function exn
		in
		  case inp of
		    NONE => NONE
		  | SOME inp => link inp
		end
	    in
	      if blocking 
		then case readerSel (augmented_reader, #readVec) of
		       NONE => liftExn (instreamName is) 
			               function 
				       IO.BlockingNotSupported
		     | SOME readVec => doit (SOME o readVec)
		else case readerSel (augmented_reader, #readVecNB) of
		       NONE => liftExn (instreamName is) 
			               function 
				       IO.NonblockingNotSupported
		     | SOME readVecNB => doit readVecNB
	    end
	| _ => liftExn (instreamName is) function Match

      fun extendB function is = valOf (extend function is true)
      fun extendNB function is = extend function is false

      fun input (is as In {pos, state, ...}) =
	case !state of
	  Link {inp, next} => 
	    let
	      val inp = V.tabulate
		        (V.length inp - pos, 
			 fn i => V.sub (inp, pos + i))
	    in
	      (inp, updateState (is, 0, next))
	    end
	| Eos {next} => (empty, updateState (is, 0, next))
	| End => extendB "input" is
	| Truncated => (empty, is)
	| Closed => (empty, is)

      fun inputN (is, n) = 
	if n < 0 orelse n > V.maxLen 
	  then raise Size
	  else let
		 fun finish (inps, is) =
		   let val inp = V.concat (List.rev inps)
		   in (inp, is)
		   end
		 fun loop (is as In {pos, state, ...}, inps, n) =
		   case !state of
		     Link {inp, next} =>
		       if pos + n < V.length inp
			 then let
				val inp' = V.tabulate
				           (n, fn i => V.sub (inp, pos + i))
				val inps = inp'::inps
			      in
				finish (inps, updateState (is, pos + n, state))
			      end
			 else let
				val inp' = V.tabulate
				           (V.length inp - pos, 
					    fn i => V.sub (inp, pos + i))
				val inps = inp'::inps
			      in
				loop (updateState (is, 0, next), 
				      inps, n - (V.length inp - pos))
			      end
		   | Eos {next} => 
		       finish (inps, if n > 0
				       then updateState(is, 0, next)
				       else is)
		   | End => 
		       let val _ = extendB "inputN" is 
		       in loop (is, inps, n)
		       end
		   | _ => finish (inps, is)
	       in
		 loop (is, [], n)
	       end

      fun input1 (is as In {pos, state, ...}) =
	case !state of
	  Link {inp, next} =>
	    let
	      val (k, next) = if pos + 1 < V.length inp
				then (pos + 1, state)
				else (0, next)
	    in
	      SOME (V.sub (inp, pos), updateState (is, k, next))
	    end
	| End => 
	    let val _ = extendB "input1" is 
	    in input1 is
	    end
	| _ => NONE

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

      fun inputLine is = 
	let
	  fun findLine (v, i) =
	    case V.findi (fn (j, c) => j >= i andalso isLine c) v of
	      SOME (j, _) => SOME j
	    | NONE => NONE
	  fun finish (inps, is, trail) =
	    let
	      val inps = if trail
			   then line::inps
			   else inps
	      val inp = V.concat (List.rev inps)
	    in
	      (inp, is)
	    end
	  fun loop (is as In {pos, state, ...}, inps) =
	    case !state of
	      Link {inp, next} =>
		(case findLine (inp, pos) of
		   SOME i => let
			       val j = i + 1
			       val (k, next) = if j < V.length inp
						 then (j, state)
						 else (0, next)
			       val inp' = V.tabulate
				          (j - pos,
					   fn i => V.sub (inp, pos + i))
			       val inps = inp'::inps
			     in
			       finish (inps, updateState (is, k, next), false)
			     end
		 | NONE => let
			     val inp' = V.tabulate
			                (V.length inp - pos,
					 fn i => V.sub (inp, pos + i))
			     val inps = inp'::inps
			   in
			     loop (updateState (is, 0, next), inps)
			   end)
	    | Eos {next} => 
		finish (inps, is, List.length inps > 0)
	    | End => 
		let val _ = extendB "inputLine" is 
		in loop (is, inps)
		end
	    | _ => finish (inps, is, true)
	in
	  loop (is, [])
	end

      fun canInput (is as In {pos, state, ...}, n) =
	if n < 0 orelse n > V.maxLen
	  then raise Size
	else if n = 0
	  then SOME 0
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
		     val this = Link {inp = inp,
				      next = next}
		     val _ = state := this
		   in
		     SOME k
		   end
	       in 
		 case !state of
		   Link {inp, next} => 
		     SOME (Int.min (V.length inp - pos, n))
		 | End => 
		     (case extendNB "canInput" is of
			NONE => NONE
		      | SOME (inp, is') => if V.length inp = 0
					     then SOME 0
					     else start (is', inp))
		 | _ => SOME 0
	       end

      fun closeIn (is as In {common = {tail, ...}, state, ...}) =
	case !(!tail) of
	  End =>
	    (!tail := Closed;
	     (readerSel (instreamReader is, #close)) ())
	| _ => ()
	handle exn => liftExn (instreamName is) "closeIn" exn

      fun endOfStream is =
	let val (inp, _) = input is
	in V.length inp = 0
	end

      fun mkInstream' {reader, closed, buffer_contents} =
	let
	  val next = ref (if closed then Closed else End)
	  val this =
	    case buffer_contents of
	      NONE => next
	    | SOME v => let 
			  val chain = if V.length v = 0
					then Eos {next = next}
					else Link {inp = v,
						   next = next}
			  val this = ref chain
			in
			  this
			end
	in
	  In {common = {reader = reader,
			augmented_reader = PIO.augmentReader reader,
			tail = ref next},
	      pos = 0,
	      state = this}
	end
      fun mkInstream (reader, buffer_contents) =
	mkInstream' {reader = reader, closed = false, 
		     buffer_contents = if V.length buffer_contents = 0
					 then NONE
					 else SOME buffer_contents}
      fun openVector v =
	mkInstream' {reader = PIO.openVector v,
		     closed = false,
		     buffer_contents = NONE}

      fun getReader (is as In {common = {reader, tail, ...}, ...}) =
	(case !(!tail) of
	   End => !tail := Truncated
	 | _ => liftExn (instreamName is) "getReader" IO.ClosedStream;
	 (reader, empty))

      fun filePosIn (is as In {common = {reader, tail, ...}, ...}) =
	case !(!tail) of
	  End =>
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
functor StreamIO
        (S: STREAM_IO_ARG): STREAM_IO = 
  StreamIOExtra(open S
		val lineElem = someElem
		fun isLine _ = raise (Fail "<isLine>"))

signature STREAM_IO_EXTRA_FILE_ARG =
   sig
      structure PrimIO: PRIM_IO_EXTRA
      structure Array: MONO_ARRAY
      structure Vector: MONO_VECTOR
      sharing type PrimIO.elem = Array.elem = Vector.elem
      sharing type PrimIO.vector = Array.vector = Vector.vector
      sharing type PrimIO.array = Array.array
      val someElem: PrimIO.elem

      val lineElem : Vector.elem
      val isLine : Vector.elem -> bool

      structure Cleaner: CLEANER
   end

functor StreamIOExtraFile
        (S: STREAM_IO_EXTRA_FILE_ARG): STREAM_IO_EXTRA_FILE =
   struct
      open S

      structure PIO = PrimIO
      structure V = Vector

      structure StreamIO = StreamIOExtra(open S)
      open StreamIO

      structure PFS = Posix.FileSys

      fun liftExn name function cause = raise IO.Io {name = name,
						     function = function,
						     cause = cause}

      (*---------------*)
      (*   outstream   *)
      (*---------------*)

      fun writerSel (PIO.WR v, sel) = sel v
      fun outstreamName os = writerSel (outstreamWriter os, #name)

      fun outFd os =
	case writerSel (outstreamWriter os, #ioDesc) of
	  SOME ioDesc => valOf (Posix.FileSys.iodToFD ioDesc)
	| NONE => liftExn (outstreamName os) "outFd" (Fail "<no ioDesc>")

      val openOutstreams : (outstream * {close: bool}) list ref = ref []
      val mkOutstream'' =
	let	
	  val _ = Cleaner.addNew
	          (Cleaner.atExit, fn () =>
		   List.app (fn (os, {close}) =>
			     if close
			       then closeOut os
			       else flushOut os) (!openOutstreams))
	in
	  fn {writer, closed, buffer_mode, atExit} =>
	  let
	    val os = mkOutstream' {writer = writer,
				   closed = closed,
				   buffer_mode = buffer_mode}
	    val _ = if closed
		      then ()
		      else openOutstreams := (os,atExit) :: (!openOutstreams)
	  in
	    os
	  end
	end
      fun mkOutstream' {writer, closed, buffer_mode} =
	mkOutstream'' {writer = writer, closed = closed, 
		       buffer_mode = buffer_mode,
		       atExit = {close = true}}
      fun mkOutstream (writer, buffer_mode) =
	mkOutstream' {writer = writer, closed = false,
		      buffer_mode = buffer_mode}
      val closeOut = fn os =>
	let
	  val _ = openOutstreams := List.filter (fn (os', _) => 
						 not (equalsOut (os, os'))) 
                                                (!openOutstreams)
	in
	  closeOut os
	end

      (*---------------*)
      (*   instream    *)
      (*---------------*)

      fun readerSel (PIO.RD v, sel) = sel v
      fun instreamName is = readerSel (instreamReader is, #name)

      fun inFd is =
	case readerSel (instreamReader is, #ioDesc) of
	  SOME ioDesc => valOf (Posix.FileSys.iodToFD ioDesc)
	| NONE => liftExn (instreamName is) "inFd" (Fail "<no ioDesc>")

      val openInstreams : (instream * {close: bool}) list ref = ref []
      val mkInstream'' =
	let
	  val _ = Cleaner.addNew
	          (Cleaner.atExit, fn () =>
		   List.app (fn (is, {close}) => 
			     if close
			       then closeIn is
			       else ()) (!openInstreams))
	in
	  fn {reader, closed, buffer_contents, atExit} =>
	  let
	    val is = mkInstream' {reader = reader,
				  closed = closed,
				  buffer_contents = buffer_contents}
	    val _ = if closed
		      then ()
		      else openInstreams := (is,atExit) :: (!openInstreams)
	  in
	    is
	  end
	end
      fun mkInstream' {reader, closed, buffer_contents} =
	mkInstream'' {reader = reader, closed = closed, 
		      buffer_contents = buffer_contents,
		      atExit = {close = true}}
      fun mkInstream (reader, buffer_contents) =
	mkInstream' {reader = reader, closed = false, 
		     buffer_contents = if V.length buffer_contents = 0
					 then NONE
					 else SOME buffer_contents}
      val closeIn = fn is =>
	let
	  val _ = openInstreams := List.filter (fn (is',_) => 
						not (equalsIn (is, is'))) 
                                               (!openInstreams)
	in
	  closeIn is
	end
   end
