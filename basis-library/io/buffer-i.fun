signature BUFFER_I_EXTRA_ARG =
   sig
      structure PrimIO: PRIM_IO
      structure StreamIO: STREAM_IO_EXTRA
      structure Array: MONO_ARRAY
      structure Vector: MONO_VECTOR
      sharing type PrimIO.elem = StreamIO.elem = Vector.elem = Array.elem
      sharing type PrimIO.vector = StreamIO.vector = Vector.vector = Array.vector
      sharing type PrimIO.array = Array.array
      sharing type PrimIO.reader = StreamIO.reader
      sharing type PrimIO.pos = StreamIO.pos
      val someElem: PrimIO.elem

      val lineElem: Vector.elem
      val isLine: Vector.elem -> bool
      val hasLine: Vector.vector -> bool
   end

functor BufferIExtra
        (S: BUFFER_I_EXTRA_ARG): BUFFER_I_EXTRA =
   struct
      open S

      structure PIO = PrimIO
      structure SIO = StreamIO
      structure V = Vector
      structure A = Array

      type elem = PrimIO.elem
      type vector = PrimIO.vector
      type reader = PIO.reader
      type pos = PIO.pos
      type instream = SIO.instream

      fun liftExn name function cause = raise IO.Io {name = name,
						     function = function,
						     cause = cause}

      (*---------------*)
      (*   inbuffer    *)
      (*---------------*)

      datatype state = Open of {eos: bool} | Closed
      (* Inv: if !first < !last then !state = Open {eos = false} 
              if !state = Closed then !first = !last
	      if !state = Open {eos = true} then !first = !last
       *)
      datatype inbuffer = In of {reader: PIO.reader,
				 augmented_reader: PIO.reader,
				 state: state ref,
				 first: int ref, (* index of first character *)
				 last: int ref, (* one past the index of the last char *)
				 buf: A.array}

      fun equalsIn (is1 as In {state = state1, ...},
		    is2 as In {state = state2, ...}) = state1 = state2

      fun inbufferSel (In v, sel) = sel v
      fun inbufferReader ib = inbufferSel (ib, #reader)
      fun readerSel (PIO.RD v, sel) = sel v
      fun inbufferName ib = readerSel (inbufferReader ib, #name)

      val empty = V.tabulate (0, fn _ => someElem)
      val line = V.tabulate (1, fn _ => lineElem)
      fun lastElem v = V.sub (v, V.length v - 1)

      fun update function (ib as In {augmented_reader, state,
				     first, last, buf, ...}) blocking =
	case !state of
	  Closed => SOME false
	| Open {eos = true} => SOME false
	| Open {eos = false} =>
	    if !first < !last
	      then SOME true
	      else if blocking
		     then case readerSel (augmented_reader, #readArr) of
		            NONE => liftExn (inbufferName ib) 
			                    function 
					    IO.BlockingNotSupported
			  | SOME readArr =>
			      let
				val i = readArr {buf = buf, i = 0, sz = NONE}
				        handle exn =>
					liftExn (inbufferName ib) function exn
			      in
				if i = 0
				  then (state := Open {eos = true};
					SOME false)
				  else (first := 0;
					last := i;
					SOME true)
			      end
		     else case readerSel (augmented_reader, #readArrNB) of
		            NONE => liftExn (inbufferName ib) 
		                            function 
				            IO.NonblockingNotSupported
			  | SOME readArrNB =>
			      let
				val i = readArrNB {buf = buf, i = 0, sz = NONE}
			                handle exn =>
					liftExn (inbufferName ib) function exn
			      in
				case i of
				  NONE => NONE
				| SOME i =>
				    if i = 0
				      then (state := Open {eos = true};
					    SOME false)
				      else (first := 0;
					    last := i;
					    SOME true)
			      end

      fun updateB function ib = valOf (update function ib true)
      fun updateNB function ib = update function ib false

      fun input (ib as In {augmented_reader, state, 
			   first, last, buf, ...}) =
	case !state of
	  Closed => empty
	| Open {eos = true} => (state := Open {eos = false};
				empty)
	| Open {eos = false} =>
	    let
	      val f = !first
	      val l = !last
	    in
	      if f < l
		then (first := l;
		      V.tabulate (l - f, fn i => A.sub (buf, f + i)))
		else case readerSel (augmented_reader, #readVec) of
		       NONE => liftExn (inbufferName ib) "input" IO.BlockingNotSupported 
		     | SOME readVec => readVec (readerSel (augmented_reader, #chunkSize))
			               handle exn => 
				       liftExn (inbufferName ib) "input" exn
	    end

      (* input1 will move past a temporary end of stream *)
      fun input1 (ib as In {state, first, last, buf, ...}) =
	case !state of
	  Closed => NONE
	| Open {eos = true} => (state := Open {eos = false}; 
				NONE)
	| Open {eos = false} =>
	    let
	      val f = !first
	      val l = !last
	    in
	      if f < l
		then (first := f + 1;
		      SOME (A.sub (buf, f)))
		else if updateB "input1" ib
		       then (first := 1;
			     SOME (A.sub (buf, 0)))
		       else NONE
	    end
	  
      fun inputN (ib as In {augmented_reader, state, 
			    first, last, buf, ...}, n) =
	if n < 0 orelse n > V.maxLen
	  then raise Size
	  else case !state of
	    Closed => empty
	  | Open {eos = true} => (state := Open {eos = false};
				  empty)
	  | Open {eos = false} =>
	      let
		val f = !first
		val l = !last
		val size = l - f
	      in
		if size >= n
		  then (first := f + n;
			V.tabulate (n, fn k => A.sub (buf, f + k)))
		  else case readerSel (augmented_reader, #readArr) of
		         NONE => liftExn (inbufferName ib)
			         "inputN" 
				 IO.BlockingNotSupported 
		       | SOME readArr => 
			   let
			     val inp = A.array (n, someElem)
			     fun fill k =
			       if k >= size
				 then ()
				 else (A.update (inp, k, A.sub (buf, f + k));
				       fill (k + 1))
			     val _ = fill 0
			     val _ = first := l
			     fun loop i =
			       if i = n
				 then i
				 else let
					val j = readArr
					        {buf = inp,
						 i = i,
						 sz = SOME (n - i)}
						handle exn => 
						liftExn (inbufferName ib) "inputN" exn
				      in
					if j = 0
					  then (state := Open {eos = true}; i)
					  else loop (i + j)
				      end
			     val i = loop size
			   in
			     V.tabulate (i, fn k => A.sub (inp, k))
			   end
	      end
	    
      fun inputAll (ib as In {augmented_reader, state, 
			      first, last, buf, ...}) =
	case !state of
	  Closed => empty
	| Open {eos = true} => (state := Open {eos = false};
				empty)
	| Open {eos = false} =>
	    (case readerSel (augmented_reader, #readVec) of
	       NONE => liftExn (inbufferName ib) "inputAll" IO.BlockingNotSupported
	     | SOME readVec =>
		 let
		   val f = !first
		   val l = !last
		   val inp = V.tabulate (l - f, fn i => A.sub (buf, f + i))
		   val inps = [inp]
		   fun loop inps =
		     let
		       val inp = readVec (readerSel (augmented_reader, #chunkSize))
			         handle exn =>
				 liftExn "inputAll" (inbufferName ib) exn
		     in
		       if V.length inp = 0
			 then V.concat (List.rev inps)
			 else loop (inp :: inps)
		     end
		 in
		   loop inps
		 end)

      fun inputLine (ib as In {first, last, buf, ...}) = 
	let
	  fun finish (inps, trail) =
	    let
	      val inps = if trail
			   then line::inps
			   else inps
	      val inp = V.concat (List.rev inps)
	    in
	      inp
	    end
	  fun loop inps =
	    if updateB "inputLine" ib
	      then let
		     (* !first < !last *) 
		     fun loop' i = (* pre: !first <= i <= !last *)
		       let
			 val f = !first
			 val l = !last
			 fun done j = (* pre: !first < j <= !last *)
			   let
			     val inp = V.tabulate(j - f, fn k => A.sub (buf, f + k))
			   in
			     first := j;
			     inp::inps
			   end
		       in
			 if i >= l
			   then loop (done i)
			   else if isLine (A.sub (buf, i))
				  then finish (done (i + 1), false)
				  else loop' (i + 1)
		       end
		   in
		     loop' (!first)
		   end
	      else finish (inps, List.length inps > 0)
	in
	  loop []
	end

      (* Not entirely correct; spec requires:
       *
       * Implementations of canInput should attempt to return as large
       * a k as possible. For example, if the buffer contains 10
       * characters and the user calls canInput (f, 15), canInput
       * should call readVecNB 5 to see if an additional 5 characters
       * are available.
       *)
      fun canInput (ib as In {state, first, last, buf, ...}, n) =
	if n < 0 orelse n > V.maxLen
	  then raise Size
	  else case !state of
	    Closed => NONE
	  | Open {eos = true} => NONE
	  | Open {eos = false} => 
	      let
		val f = !first
		val l = !last
	      in
		if f < l
		  then SOME (Int.min (n, l - f))
		  else case updateNB "canInput" ib of
		         NONE => NONE
		       | SOME true => SOME (Int.min (n, !last))
		       | SOME false => SOME 0
	      end

      fun lookahead (ib as In {state, first, last, buf, ...}) =
	case !state of
	  Closed => NONE
	| Open {eos = true} => NONE
	| Open {eos = false} =>
	    let
	      val f = !first
	      val l = !last
	    in
	      if f < l
		then SOME (A.sub (buf, f))
		else if updateB "lookahead" ib
		       then SOME (A.sub (buf, 0))
		       else NONE
	    end

      fun closeIn (ib as In {first, last, state, ...}) =
	case !state of
	  Closed => ()
	| _ => (first := !last;
		state := Closed;
		(readerSel (inbufferReader ib, #close)) ())
	       handle exn => liftExn (inbufferName ib) "closeIn" exn

      fun endOfStream ib = not (updateB "endOfStream" ib)

      fun mkInbuffer' {reader, closed, buffer_contents} =
	let
	  val (state, first, last, buf) =
	    if closed
	      then (ref Closed, ref 0, ref 0, Array.array (0, someElem))
	      else let
		     val buf = Array.array (readerSel (reader, #chunkSize), someElem)
		     val first = ref 0
		     val (state, last) =
		       case buffer_contents of
			 NONE => (ref (Open {eos = false}), ref 0)
		       | SOME v => if V.length v = 0
				     then (ref (Open {eos = true}), ref 0)
				     else (V.appi (fn (i, c) => A.update (buf, i, c)) v;
					   (ref (Open {eos = false}), ref (V.length v)))
		   in
		     (state, first, last, buf)
		   end
	in
	  In {reader = reader,
	      augmented_reader = PIO.augmentReader reader,
	      state = state,
	      first = first,
	      last = last,
	      buf = buf}
	end
      fun mkInbuffer (reader, buffer_contents) = 
	mkInbuffer' {reader = reader, closed = false,
		     buffer_contents = if V.length buffer_contents = 0
					 then NONE
					 else SOME buffer_contents}
      fun openVector v = 
	mkInbuffer' {reader = PIO.openVector v,
		     closed = false,
		     buffer_contents = NONE}

      fun getInstream' mkInstream (ib as In {reader, state,
					     first, last, buf, ...}) =
	case !state of
	  Closed => mkInstream {reader = reader,
				closed = true,
				buffer_contents = NONE}
	| Open {eos = true} => mkInstream {reader = reader,
					   closed = false,
					   buffer_contents = SOME empty}
	| Open {eos = false} =>
	    let
	      val f = !first
	      val l = !last
	    in 
	      if f < l
		then let
		       val buffer_contents = 
			 V.tabulate (l - f, fn i => A.sub (buf, f + i))
		     in 
		       mkInstream {reader = reader,
				   closed = false,
				   buffer_contents = SOME buffer_contents}
		     end 
		else mkInstream {reader = reader,
				 closed = false,
				 buffer_contents = NONE}
	    end
      fun getInstream ib =
	getInstream' SIO.mkInstream' ib
   end

signature BUFFER_I_ARG = 
   sig
      structure PrimIO: PRIM_IO
      structure StreamIO: STREAM_IO
      structure Array: MONO_ARRAY
      structure Vector: MONO_VECTOR
      sharing type PrimIO.elem = StreamIO.elem = Vector.elem = Array.elem
      sharing type PrimIO.vector = StreamIO.vector = Vector.vector = Array.vector
      sharing type PrimIO.array = Array.array
      sharing type PrimIO.reader = StreamIO.reader
      sharing type PrimIO.pos = StreamIO.pos
      val someElem: PrimIO.elem
   end
functor BufferI
        (S: BUFFER_I_ARG): BUFFER_I = 
  BufferIExtra(open S
	       structure StreamIO =
		  struct
		     open StreamIO
		     fun equalsIn _ = raise (Fail "<equalsIn>")
		     fun instreamReader _ = raise (Fail "<instreamReader>")
		     fun mkInstream' _ = raise (Fail "<mkInstream>")
		     fun equalsOut _ = raise (Fail "<equalsOut>")
		     fun outstreamWriter _ = raise (Fail "<outstreamWriter>")
		     fun mkOutstream' _ = raise (Fail "<mkOutstream>")
		     fun openVector _ = raise (Fail "<openVector>")
		     fun inputLine _ = raise (Fail "<inputLine>")
		     fun outputSlice _ = raise (Fail "<outputSlice>")
		  end
	       val lineElem = someElem
	       fun isLine _ = raise (Fail "<isLine>")
	       fun hasLine _ = raise (Fail "<hasLine>"))

signature BUFFER_I_EXTRA_FILE_ARG =
   sig
      structure PrimIO: PRIM_IO
      structure StreamIO: STREAM_IO_EXTRA_FILE
      structure Array: MONO_ARRAY
      structure Vector: MONO_VECTOR
      sharing type PrimIO.elem = StreamIO.elem = Vector.elem = Array.elem
      sharing type PrimIO.vector = StreamIO.vector = Vector.vector = Array.vector
      sharing type PrimIO.array = Array.array
      sharing type PrimIO.reader = StreamIO.reader
      sharing type PrimIO.pos = StreamIO.pos
      val someElem: PrimIO.elem

      val lineElem: Vector.elem
      val isLine: Vector.elem -> bool
      val hasLine: Vector.vector -> bool

      structure Cleaner: CLEANER
   end

functor BufferIExtraFile
        (S: BUFFER_I_EXTRA_FILE_ARG): BUFFER_I_EXTRA_FILE =
   struct
      open S

      structure PIO = PrimIO
      structure SIO = StreamIO
      structure V = Vector
	
      structure BufferI = BufferIExtra(open S)
      open BufferI
	
      structure PFS = Posix.FileSys
	
      fun liftExn name function cause = raise IO.Io {name = name,
						     function = function,
						     cause = cause}

      (*---------------*)
      (*   inbuffer    *)
      (*---------------*)
	
      fun readerSel (PIO.RD v, sel) = sel v
      fun inbufferName ib = readerSel (inbufferReader ib, #name)

      fun inFd ib =
	case readerSel (inbufferReader ib, #ioDesc) of
	  SOME ioDesc => valOf (Posix.FileSys.iodToFD ioDesc)
	| NONE => liftExn (inbufferName ib) "inFd" (Fail "<no ioDesc>")

      val openInbuffers : (inbuffer * {close: bool}) list ref = ref []
      val mkInbuffer'' =
	let
	  val _ = Cleaner.addNew
	          (Cleaner.atExit, fn () =>
		   List.app (fn (ib, {close}) => 
			     if close
			       then closeIn ib
			       else ()) (!openInbuffers))
	in
	  fn {reader, closed, buffer_contents, atExit} =>
	  let
	    val ib = mkInbuffer' {reader = reader,
				  closed = closed,
				  buffer_contents = buffer_contents}
	    val _ = if closed
		      then ()
		      else openInbuffers := (ib,atExit) :: (!openInbuffers)
	  in
	    ib
	  end
	end
      fun mkInbuffer' {reader, closed, buffer_contents} =
	mkInbuffer'' {reader = reader, closed = closed, 
		      buffer_contents = buffer_contents,
		      atExit = {close = true}}
      fun mkInbuffer (reader, buffer_contents) =
	mkInbuffer' {reader = reader, closed = false, 
		     buffer_contents = if V.length buffer_contents = 0
					 then NONE
					 else SOME buffer_contents}
      fun getInstream'' mkInstream ib =
	let
	  val (ibs, openInbuffers') =
	    List.partition (fn (ib',_) => equalsIn (ib, ib')) (!openInbuffers)
	  val _ = openInbuffers := openInbuffers'
	  val atExit = 
	    List.foldr (fn ((_,{close = close'}), {close}) =>
			{close = close orelse close'})
                       {close = false}
                       ibs
	in
	  getInstream' (fn {reader, closed, buffer_contents} =>
			mkInstream {reader = reader,
				    closed = closed,
				    buffer_contents = buffer_contents,
				    atExit = atExit}) ib
	end
      fun getInstream ib = 
	getInstream'' SIO.mkInstream'' ib
      val closeIn = fn ib =>
	let
	  val _ = openInbuffers := List.filter (fn (ib',_) => 
						not (equalsIn (ib, ib'))) 
                                               (!openInbuffers)
	in
	  closeIn ib
	end
   end
