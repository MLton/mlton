signature STREAM_IO_EXTRA_ARG = 
   sig
      structure PrimIO: PRIM_IO where type pos = Position.int
      structure Array: MONO_ARRAY
      structure ArraySlice: MONO_ARRAY_SLICE
      structure Vector: sig 
	                  include MONO_VECTOR
			  val extract: vector * int * int option -> vector
			end
      structure VectorSlice: MONO_VECTOR_SLICE
      sharing type Array.array = ArraySlice.array = PrimIO.array
      sharing type Array.elem = ArraySlice.elem = PrimIO.elem = Vector.elem
	 = VectorSlice.elem
      sharing type Array.vector = ArraySlice.vector = PrimIO.vector
	 = Vector.vector = VectorSlice.vector
      sharing type ArraySlice.slice = PrimIO.array_slice
      sharing type ArraySlice.vector_slice = PrimIO.vector_slice
	 = VectorSlice.slice

      val isLine : Vector.elem -> bool 
      val lineElem : Vector.elem
      val someElem: PrimIO.elem
   end

functor StreamIOExtra (S: STREAM_IO_EXTRA_ARG): STREAM_IO_EXTRA =
   struct
      open S

      structure PIO = PrimIO
      structure A = Array
      structure AS = ArraySlice
      structure V = struct
		      open Vector
		      val extract = extract
		    end
      structure VS = VectorSlice

      type elem = PIO.elem
      type vector = PIO.vector
      type reader = PIO.reader
      type writer = PIO.writer
      type pos = PIO.pos

      fun liftExn name function cause = raise IO.Io {name = name,
						     function = function,
						     cause = cause}

      val hasLine = fn z => V.exists isLine z

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
      fun outstreamName os =
	 let
	    val PIO.WR {name, ...} = outstreamWriter os
	 in
	    name
	 end
      fun outstreamName os = writerSel (outstreamWriter os, #name)

      fun flushGen (write: 'a -> int,
		    base: 'a -> ('b * int * int),
		    slice: ('b * int * int option) -> 'a,
		    a: 'a) =
	 let
	    val (b, i, sz) = base a
	    val max = i + sz
	    fun loop i =
	       if i = max
		  then ()
	       else let
		       val j = write (slice (b, i, SOME (max - i)))
		    in 
		       if j = 0
			  then raise (Fail "partial write")
		       else loop (i + j)
		    end
	 in
	    loop i
	 end

      fun flushVec (writer, x) =
	 case writerSel (writer, #writeVec) of
	    NONE => raise IO.BlockingNotSupported
	  | SOME writeVec => flushGen (writeVec, VS.base, VS.slice, x)
     
      fun flushArr (writer, x) =
	 case writerSel (writer, #writeArr) of
	    NONE => raise IO.BlockingNotSupported
	  | SOME writeArr => flushGen (writeArr, AS.base, AS.slice, x)
     
      fun flushBuf (writer, Buf {size, array}) =
	 let
	    val size' = !size 
	 in 
	    size := 0
	    ; flushArr (writer, AS.slice (array, 0, SOME size'))
	 end

      fun output (os as Out {augmented_writer, 
			     state, 
			     buffer_mode, ...}, v) =
	if terminated (!state)
	  then liftExn (outstreamName os) "output" IO.ClosedStream
	  else let
		  fun put () = flushVec (augmented_writer, VS.full v)
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
					  AS.slice (buf1, 0, SOME 1)))
		   | LINE_BUF buf => doit (buf, isLine c)
		   | BLOCK_BUF buf => doit (buf, false)
		 end
	  handle exn => liftExn (outstreamName os) "output1" exn
      end

      fun outputSlice (os, (v, i, sz)) =
	let
	  val v' = V.extract(v, i, sz)
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

      datatype state = Link of {buf: buf}
	             | Eos of {buf: buf} (* V.length inp = 0 *)
	             | End
	             | Truncated
	             | Closed
      and buf = Buf of {inp: V.vector,
			base: pos option,
			next: state ref}

      datatype instream = In of {common: {reader: reader,
					  augmented_reader: reader,
					  tail: state ref ref},
				 pos: int,
				 buf: buf}

      (* @ s = Eos, End, Truncated, Closed ==>
       *   pos = V.length inp, !next = s
       *)

      fun equalsIn (is1 as In {common = {tail = tail1, ...}, ...}, 
		    is2 as In {common = {tail = tail2, ...}, ...}) = 
	tail1 = tail2

      fun update (In {common, ...}, pos, buf) =
	In {common = common,
	    pos = pos,
	    buf = buf}
      fun updatePos (is as In {buf, ...}, pos) = update (is, pos, buf)
      fun updateBufBeg (is, buf) = update (is, 0, buf)
      fun updateBufEnd (is, buf as Buf {inp, ...}) = update (is, V.length inp, buf)

      fun instreamSel (In v, sel) = sel v
      fun instreamCommon is = instreamSel (is, #common)
      fun instreamCommonSel (is, sel) = sel (instreamCommon is)
      fun instreamReader is = instreamCommonSel (is, #reader)
      fun instreamTail is = instreamCommonSel (is, #tail)
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
	      fun link (base, inp) = let
				       val next = ref End
				       val buf = Buf {inp = inp,
						      base = base,
						      next = next}
				       val this = if V.length inp = 0
						    then Eos {buf = buf}
						    else Link {buf = buf}
				       val _ = !tail := this
				       val _ = tail := next
				     in
				       SOME this
				     end
	      fun doit readVec =
		let
		  val base =
		    case readerSel (augmented_reader, #getPos) of
		      NONE => NONE
		    | SOME getPos => SOME (getPos ())
		  val inp = readVec (readerSel (augmented_reader, #chunkSize))
		            handle exn =>
			    liftExn (instreamName is) function exn
		in
		  case inp of
		    NONE => NONE
		  | SOME inp => link (base, inp)
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

      fun input (is as In {pos, buf as Buf {inp, next, ...}, ...}) =
	if pos < V.length inp
	  then (V.extract(inp, pos, NONE), 
		updateBufEnd (is, buf))
	  else let
		 fun doit next =
		   case next of
		     Link {buf as Buf {inp, ...}} => (inp, updateBufEnd (is, buf))
		   | Eos {buf} => (empty, updateBufBeg (is, buf))
		   | End => doit (extendB "input" is)
		   | _ => (empty, is)
	       in
		 doit (!next)
	       end

      fun inputN (is, n) =
	if n < 0 orelse n > V.maxLen
	  then raise Size
	  else let
		 fun first (is as In {pos, buf as Buf {inp, next, ...}, ...}, n) =
		   if pos + n <= V.length inp
		     then let
			    val inp' = V.extract(inp, pos, SOME n)
			  in
			    (inp', updatePos (is, pos + n))
			  end
		     else let
			    val inp' = V.extract(inp, pos, NONE)
			  in
			    loop (buf, [inp'], n - (V.length inp - pos))
			  end
		 and loop (buf' as Buf {next, ...}, inps, n) =
		   let
		     fun doit next =
		       case next of
			 Link {buf as Buf {inp, next, ...}} =>
			   if n <= V.length inp
			     then let
				    val inp' = V.extract(inp, 0, SOME n)
				    val inps = inp'::inps
				  in
				    finish (inps, update (is, n, buf))
				  end
			     else loop (buf, inp::inps, n - V.length inp)
		       | Eos {buf} => 
			   finish (inps, if n > 0
					   then updateBufBeg (is, buf)
					   else updateBufEnd (is, buf'))
		       | End => doit (extendB "inputN" is)
		       | _ => finish (inps, updateBufEnd (is, buf'))
		   in
		     doit (!next)
		   end
		 and finish (inps, is) =
		   let val inp = V.concat (List.rev inps)
		   in (inp, is)
		   end
	       in
		 first (is, n)
	       end

      (* input1' will move past a temporary end of stream *)
      fun input1' (is as In {pos, buf as Buf {inp, next, ...}, ...}) =
	let
	  val e = V.sub (inp, pos)
	  val is' = updatePos (is, pos + 1)
	in
	  (SOME e, is')
	end
        handle Subscript =>
	  let
	    fun doit next =
	      case next of
		Link {buf} => input1' (updateBufBeg (is, buf))
	      | Eos {buf} => (NONE, updateBufBeg (is, buf))
	      | End => doit (extendB "input1" is)
	      | _ => (NONE, is)
	  in
	    doit (!next)
	  end
		   
      (* input1 will never move past a temporary end of stream *)
      fun input1 is =
	case input1' is of
	  (SOME c, is') => SOME (c, is')
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
	    let
	      fun loop i =
		if i >= V.length v
		  then NONE
		  else if isLine (V.sub (v, i))
			 then SOME (i + 1)
			 else loop (i + 1)
	    in
	      loop i
	    end
	  fun first (is as In {pos, buf as Buf {inp, next, ...}, ...}) =
	    (case findLine (inp, pos) of
	       SOME i => let
			   val inp' = V.extract(inp, pos, SOME (i - pos))
			 in
			   SOME (inp', updatePos (is, i))
			 end
	     | NONE => if pos < V.length inp
			 then let
				val inp' = V.extract(inp, pos, NONE)
			      in
				loop (buf, [inp'])
			      end
			 else let
				fun doit next = 
				  case next of
				    Link {buf} => first (updateBufBeg (is, buf))
				  | Eos {buf} => NONE
				  | End => doit (extendB "inputLine" is)
				  | _ => NONE
			      in
				doit (!next)
			      end)
	  and loop (buf' as Buf {next, ...}, inps) = 
	    (* List.length inps > 0 *)
	    let
	      fun doit next =
		case next of
		  Link {buf as Buf {inp, next, ...}} =>
		    (case findLine (inp, 0) of
		       SOME i => let
				   val inp' = V.extract(inp, 0, SOME i)
				   val inps = inp'::inps
				 in
				   finish (inps, update (is, i + 1, buf), false)
				 end
		     | NONE => loop (buf, inp::inps))
		| End => doit (extendB "inputLine" is)
		| _ => finish (inps, updateBufEnd (is, buf'), true)
	    in
	      doit (!next)
	    end
	  and finish (inps, is, trail) =
	    let
	      val inps = if trail
			   then line::inps
			   else inps
	      val inp = V.concat (List.rev inps)
	    in
	       SOME (inp, is)
	    end
	in
	  first is
	end

      fun canInput (is as In {pos, buf as Buf {inp, next, ...}, ...}, n) =
	if n < 0 orelse n > V.maxLen
	  then raise Size
	else if n = 0
	  then SOME 0
	else let
	       fun start inp = 
		 add ([], inp, 0)
	       and add (inps, inp, k) =
		 let
		   val l = V.length inp
		   val inps = inp::inps
		 in
		   if k + l > n
		     then finish (inps, n)
		     else loop (inps, k + l)
		 end
	       and loop (inps, k) =
		 case extendNB "canInput" is of
		   NONE => finish (inps, k)
		 | SOME (Link {buf as Buf {inp, ...}}) =>
		     add (inps, inp, k)
		 | SOME (Eos {buf}) => finish (inps, k)
		 | _ => raise Fail "extendNB bug"
	       and finish (inps, k) =
		 let
		   val inp = V.concat (List.rev inps)
		 in
		   (inp, k)
		 end
	     in
	       if pos < V.length inp
		 then SOME (Int.min (V.length inp - pos, n))
		 else case !next of
		        End => 
			  (case extendNB "canInput" is of
			     NONE => NONE
			   | SOME (Link {buf as Buf {inp, base, ...}}) =>
			       let
				 val (inp, k) = start inp
				 val buf = Buf {inp = inp,
						base = base,
						next = ref End}
			       in
				 next := Link {buf = buf};
				 SOME k
			       end
			   | SOME (Eos {buf}) => SOME 0
			   | _ => raise Fail "extendNB bug")
		      | _ => SOME 0
	     end

      structure Close =
	 struct
	    datatype t = T of {close: unit -> unit,
			       name: string,
			       tail: state ref ref}

	    fun close (T {close, name, tail}) =
	       case !(!tail) of
		  End =>
		     (!tail := Closed
		      ; close () handle exn => liftExn name "closeIn" exn)
		| _ => ()
		     
	    fun equalsInstream (T {tail, ...}, is) = tail = instreamTail is

	    fun make (In {common = {reader = PIO.RD {close, name, ...},
				    tail, ...},
			  ...}): t =
	       T {close = close, name = name, tail = tail}
	 end

      val closeIn = Close.close o Close.make

      fun endOfStream is =
	let val (inp, _) = input is
	in V.length inp = 0
	end

      fun mkInstream' {reader, closed, buffer_contents} =
	let
	  val next = ref (if closed then Closed else End)
	  val base =
	    case readerSel (reader, #getPos) of
	      NONE => NONE
	    | SOME getPos => SOME (getPos ())
	  val buf = 
	    case buffer_contents of
	      NONE => Buf {inp = empty,
			   base = base,
			   next = next}
	    | SOME v => if V.length v = 0
			  then Buf {inp = empty,
				    base = base,
				    next = ref (Eos {buf = Buf {inp = empty,
								base = base,
								next = next}})}
			  else Buf {inp = v,
				    base = NONE,
				    next = next}
	in
	  In {common = {reader = reader,
			augmented_reader = PIO.augmentReader reader,
			tail = ref next},
	      pos = 0,
	      buf = buf}
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
	case !(!tail) of
	  End => (!tail := Truncated;
		  let val (inp, _) = inputAll is
		  in (reader, inp)
		  end)
	| _ => liftExn (instreamName is) "getReader" IO.ClosedStream

      fun filePosIn (is as In {pos, buf as Buf {base, ...}, ...}) =
	case base of
	  SOME b => Position.+ (Position.fromInt pos, b)
	| NONE => liftExn (instreamName is) "filePosIn" IO.RandomAccessNotSupported
   end

signature STREAM_IO_ARG = 
   sig
      structure PrimIO: PRIM_IO where type pos = Position.int
      structure Array: MONO_ARRAY
      structure ArraySlice: MONO_ARRAY_SLICE
      structure Vector: MONO_VECTOR
      structure VectorSlice: MONO_VECTOR_SLICE
      sharing type Array.array = ArraySlice.array = PrimIO.array
      sharing type Array.elem = ArraySlice.elem = PrimIO.elem = Vector.elem
	 = VectorSlice.elem
      sharing type Array.vector = ArraySlice.vector = PrimIO.vector
	 = Vector.vector = VectorSlice.vector
      sharing type ArraySlice.slice = PrimIO.array_slice
      sharing type ArraySlice.vector_slice = PrimIO.vector_slice
	 = VectorSlice.slice

      val someElem: PrimIO.elem
   end

functor StreamIO (S: STREAM_IO_ARG): STREAM_IO = 
  StreamIOExtra(open S
		structure Vector =
		  struct
		    open Vector
		    fun extract (v, i, sz) =
		      if i = 0 andalso sz = NONE
			then v
			else let
			       val l = 
				 case sz of
				   SOME sz => sz
				 | NONE => length v - i
			     in
			       tabulate
			       (l, fn j => 
				sub (v, i + j))
			     end
		  end
		val lineElem = someElem
		fun isLine _ = raise (Fail "<isLine>"))

signature STREAM_IO_EXTRA_FILE_ARG =
   sig
      include STREAM_IO_EXTRA_ARG

      structure Cleaner: CLEANER
   end

functor StreamIOExtraFile (S: STREAM_IO_EXTRA_FILE_ARG): STREAM_IO_EXTRA_FILE =
   struct
      open S

      structure PIO = PrimIO
      structure V = Vector

      structure StreamIO = StreamIOExtra (S)
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

      val closeAtExits: Close.t list ref = ref []
      val mkInstream'' =
	let
	   val _ = Cleaner.addNew (Cleaner.atExit, fn () =>
				   List.app Close.close (!closeAtExits))
	in
	  fn {reader, closed, buffer_contents, atExit = {close = closeAtExit}} =>
	  let
	    val is =
	       mkInstream' {reader = reader,
			    closed = closed,
			    buffer_contents = buffer_contents}
	    val _ =
	       if closed orelse not closeAtExit
		  then ()
	       else closeAtExits := Close.make is :: (!closeAtExits)
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
	    val _ =
	       closeAtExits :=
	       List.filter (fn c => Close.equalsInstream (c, is)) (!closeAtExits)
	 in
	    closeIn is
	 end
   end
