signature BUFFER_I_EXTRA_ARG =
   sig
      structure PrimIO: PRIM_IO
      structure StreamIO: STREAM_IO_EXTRA
      structure Vector: MONO_VECTOR
      structure Array: MONO_ARRAY
      sharing type PrimIO.reader = StreamIO.reader
      sharing type PrimIO.pos = StreamIO.pos
      sharing type PrimIO.elem = StreamIO.elem = Vector.elem = Array.elem
      sharing type PrimIO.vector = StreamIO.vector = Vector.vector = Array.vector
      sharing type PrimIO.array = Array.array
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

      type vector = PrimIO.vector
      type elem = PrimIO.elem

      fun liftExn name function cause = raise IO.Io {name = name,
						     function = function,
						     cause = cause}

      (*---------------*)
      (*   instream    *)
      (*---------------*)

      datatype state = Open of {eos: bool} | Closed
      (* Inv: if !first < !last then !state = Open {eos = false} 
              if !state = Closed then !first = !last
	      if !state = Open {eos = true} then !first = !last
       *)
      datatype instream = In of {reader: PIO.reader,
				 augmented_reader: PIO.reader,
				 state: state ref,
				 first: int ref, (* index of first character *)
				 last: int ref, (* one past the index of the last char *)
				 buf: A.array}

      fun equalsIn (is1 as In {state = state1, ...},
		    is2 as In {state = state2, ...}) = state1 = state2

      fun instreamSel (In v, sel) = sel v
      fun instreamReader is = instreamSel (is, #reader)
      fun readerSel (PIO.RD v, sel) = sel v
      fun instreamName is = readerSel (instreamReader is, #name)

      val empty = V.tabulate (0, fn _ => someElem)
      val line = V.tabulate (1, fn _ => lineElem)

      fun update function (is as In {augmented_reader, state,
				     first, last, buf, ...}) =
	case !state of
	  Closed => false
	| Open {eos = true} => false
	| Open {eos = false} =>
	    if !first < !last
	      then true
	      else case readerSel (augmented_reader, #readArr) of
		     NONE => liftExn (instreamName is) function IO.BlockingNotSupported
		   | SOME readArr =>
		       let
			 val i = readArr {buf = buf, i = 0, sz = NONE}
			         handle exn =>
				 liftExn (instreamName is) function exn
		       in
			 if i = 0
			   then (state := Open {eos = true};
				 false)
			   else (first := 0;
				 last := i;
				 true)
		       end
		     
      fun input (is as In {augmented_reader, state, 
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
		       NONE => liftExn (instreamName is) "input" IO.BlockingNotSupported 
		     | SOME readVec => readVec (readerSel 
						(augmented_reader, 
						 #chunkSize))
			               handle exn => 
				       liftExn (instreamName is) "input" exn
	    end

      (* input1 will move past a temporary end of stream *)
      fun input1 (is as In {state, first, last, buf, ...}) =
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
		else if update "input1" is
		       then (first := 1;
			     SOME (A.sub (buf, 0)))
		       else NONE
	    end
	  
      fun inputN (is as In {augmented_reader, state, 
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
		  else case readerSel (instreamReader is, #readArr) of
		         NONE => liftExn (instreamName is)
			         "input" 
				 IO.BlockingNotSupported 
		       | SOME readArr => 
			   let
			     val inp = A.array (n, someElem)
			     fun fill k =
			       if k = size
				 then ()
				 else A.update (inp, k, A.sub (buf, f + k))
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
						liftExn (instreamName is) "input" exn
				      in
					if j = 0
					  then (state := Open {eos = false}; 
						i)
					  else loop (i + j)
				      end
			     val i = loop n
			   in
			     V.tabulate (i, fn k => A.sub (buf, k))
			   end
	      end
	    
      fun inputAll (is as In {augmented_reader, state, 
			      first, last, buf, ...}) =
	case !state of
	  Closed => empty
	| Open {eos = true} => (state := Open {eos = false};
				empty)
	| Open {eos = false} =>
	    (case readerSel (augmented_reader, #readVec) of
	       NONE => liftExn (instreamName is) "inputAll" IO.BlockingNotSupported
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
				 liftExn "inputAll" (instreamName is) exn
		     in
		       if V.length inp = 0
			 then V.concat (List.rev inps)
			 else loop (inp :: inps)
		     end
		 in
		   loop inps
		 end)
	       
      fun inputLine (is as In {...}) = raise (Fail "<inputLine>")
	
      fun canInput (is as In {state, ...}, n) =
	if n < 0 orelse n > V.maxLen
	  then raise Size
	  else case !state of
	    Closed => NONE
	  | Open {eos = true} => NONE
	  | Open {eos = false} => raise (Fail "<canInput>")

      fun lookahead (is as In {state, first, last, buf, ...}) =
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
		else if update "lookahead" is
		       then SOME (A.sub (buf, 0))
		       else NONE
	    end

      fun closeIn (is as In {first, last, state, ...}) =
	case !state of
	  Closed => ()
	| _ => (first := !last;
		state := Closed;
		(readerSel (instreamReader is, #close)) ())
	       handle exn => liftExn (instreamName is) "closeIn" exn

      fun endOfStream is = not (update "endOfStream" is)

      fun mkInstream' {reader, closed, buffer_contents} =
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
      fun mkInstream (reader, buffer_contents) = 
	mkInstream' {reader = reader, closed = false,
		     buffer_contents = if V.length buffer_contents = 0
					 then NONE
					 else SOME buffer_contents}
      fun openVector v = mkInstream' {reader = PIO.openVector v,
				      closed = false,
				      buffer_contents = NONE}

      fun getInstream' mkInstream 
	               (is as In {reader, state,
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
      fun getInstream is =
	getInstream' SIO.mkInstream' is
	  
   end

signature BUFFER_I_EXTRA_FILE_ARG =
   sig
      structure PrimIO: PRIM_IO
      structure StreamIO: STREAM_IO_EXTRA_FILE
      structure Vector: MONO_VECTOR
      structure Array: MONO_ARRAY
      sharing type PrimIO.reader = StreamIO.reader
      sharing type PrimIO.pos = StreamIO.pos
      sharing type PrimIO.elem = StreamIO.elem = Vector.elem = Array.elem
      sharing type PrimIO.vector = StreamIO.vector = Vector.vector = Array.vector
      sharing type PrimIO.array = Array.array
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
      val getInstream = fn is =>
	let
	  val (iss, openInstreams') =
	    List.partition (fn (is',_) => equalsIn (is, is')) (!openInstreams)
	  val _ = openInstreams := openInstreams'
	  val atExit = 
	    List.foldr (fn ((_,{close = close'}), {close}) =>
			{close = close orelse close'})
                       {close = false}
                       iss
	in
	  getInstream' (fn {reader, closed, buffer_contents} =>
			SIO.mkInstream'' {reader = reader,
					  closed = closed,
					  buffer_contents = buffer_contents,
					  atExit = atExit}) is
	end
      val closeIn = fn is =>
	let
	  val _ = openInstreams := List.filter (fn (is',_) => 
						not (equalsIn (is, is'))) 
                                               (!openInstreams)
	in
	  closeIn is
	end
   end