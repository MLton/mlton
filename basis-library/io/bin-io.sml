(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure BinIO: BIN_IO =
   struct
      structure FS = Posix.FileSys
      structure PIO = Posix.IO
	 
      structure Array = Word8Array
      structure Vector = Word8Vector
	 
      type vector = Vector.vector
      type elem = Vector.elem

      val bufSize = 4096

      (*---------------------------------------------------*)
      (*                     outstream                     *)
      (*---------------------------------------------------*)

      datatype buf =
	 Buf of {size: int ref,
		 array: Array.array}

      fun isFull(Buf{size, ...}) = !size = bufSize

      (* write out to fd size bytes of buf starting at index i *)
      fun flushGen(fd: FS.file_desc,
		   buf: 'a,
		   i: int,
		   size: int,
		   write: FS.file_desc * {buf: 'a,
					   i: int,
					   sz: int option} -> int): unit =
	 let
	    val max = i + size
	    fun loop i =
	       if i = max
		  then ()
	       else
		  loop(i + write(fd, {buf = buf,
				      i = i,
				      sz = SOME(max - i)}))
	 in loop i
	 end

      fun flush(fd, Buf{size, array}) =
	 (flushGen(fd, array, 0, !size, PIO.writeArr)
	  ; size := 0)
	 
      datatype bufStyle =
	 Unbuffered
       | Line of buf
       | Buffered of buf
	 
      datatype outstream' =
	 Out of {fd: FS.file_desc,
		 closed: bool ref,
		 bufStyle: bufStyle}
      type outstream = outstream' ref

      val mkOutstream = ref
      val getOutstream = !
      val setOutstream = op :=
	 
      fun flushOut(ref(Out{fd, bufStyle, closed, ...})): unit =
	 (case (!closed, bufStyle) of
	     (true, _) => ()
	   | (_,    Unbuffered) => ()
	   | (_,    Line b) => flush(fd, b)
	   | (_,    Buffered b) => flush(fd, b))
	 handle exn => raise IO.Io{name = "<unimplemented>",
				   function = "flushOut",
				   cause = exn}

      val openOuts: outstream list ref = ref []

      fun closeOut(out as ref(Out{fd, closed, ...})): unit =
	 if !closed then ()
	 else (flushOut out;
	       closed := true;
	       PIO.close fd;
	       openOuts := List.filter (fn out' => not(out = out')) (!openOuts))

      val newOut =
	 ((* These side-effect is here so that the dead code elimination won't
	   * get rid of it as long as newOut is used
	   *)
	  AtSaveWorld.addNewCleaner(fn () => List.app flushOut (!openOuts));
	  AtExit.addNewCleaner
	  (fn () =>
	   List.app
	   (fn out as ref(Out{fd,...}) =>
	    (flushOut out;
	     if fd = FS.stdout orelse fd = FS.stderr
		then ()
	     else closeOut out))
	   (!openOuts));
	  (* end stupidity *)
	  fn fd =>
	  let
	     val bufStyle =
		if fd = FS.stderr
		   then Unbuffered
		else (if Posix.ProcEnv.isatty fd then Line else Buffered)
		   (Buf{size = ref 0,
			array = Array.array(bufSize, 0w0)})
	     val out = ref(Out{fd = fd,
			       closed = ref false,
			       bufStyle = bufStyle})
	  in openOuts := out :: !openOuts;
	     out
	  end)

      local
	 val readWrite =
	    let open FS.S
	    in flags[irusr, iwusr, irgrp, iwgrp, iroth, iwoth]
	    end
      in
	 fun openOut path =
	    (newOut(FS.createf(path, FS.O_WRONLY, FS.O.flags[FS.O.trunc, FS.O.binary], readWrite)))
	    handle exn => raise IO.Io{name = "<unimplemented>",
				      function = "openOut",
				      cause = exn}
	       
	 fun openAppend path =
	    (newOut(FS.createf(path, FS.O_WRONLY, FS.O.flags[FS.O.append, FS.O.binary], readWrite)))
	    handle exn => raise IO.Io{name = "<unimplemented>",
				      function = "openAppend",
				      cause = exn}
      end
      
      fun output(out as ref(Out{fd, closed, bufStyle, ...}), v): unit =
	 let
	 in if !closed
	       then raise IO.Io{name = "<unimplemented>",
				function = "output1",
				cause = IO.ClosedStream}
	    else
	       let
		  val vecSize = Vector.length v
		  fun store(b as Buf{size, array}) =
		     let
			val curSize = !size
			val newSize = vecSize + curSize
		     in
			if newSize > bufSize
			   then
			      let
				 (* flush the current buffer + a prefix of the
				  * vector, if the current buffer is empty
				  *)
				 val veci =
				    if curSize = 0
				       then 0
				    else
				       let val fill = bufSize - curSize
				       in Array.copyVec{src = v,
							si = 0,
							len = SOME fill,
							dst = array,
							di = curSize} ;
					  size := bufSize ;
					  flush(fd, b) ;
					  fill
				       end
				 (* flush out as much of the vector as needed
				  * so that <= bufSize remains
				  *)
				 fun loop i =
				    let val remaining = vecSize - i
				    in if remaining <= bufSize
					  then
					     (Array.copyVec{src = v,
							    si = i,
							    len = SOME remaining,
							    dst = array,
							    di = 0} ;
					      size := remaining)
				       else
					  (flushGen(fd, v, i, bufSize, PIO.writeVec);
					   loop(i + bufSize))
				    end
			      in loop veci
			      end
			else (Array.copyVec{src = v, si = 0, len = NONE,
					    dst = array, di = curSize} ;
			      size := newSize)
		     end
	       in case bufStyle of
		  Unbuffered => flushGen(fd, v, 0, vecSize, PIO.writeVec)
		| Line b => (store b)
		| Buffered b => store b
	       end handle exn => raise IO.Io{name = "<unimplemented>",
					     function = "output1",
					     cause = exn}
	 end
      
      fun output1(out, c: elem): unit =
            output(out, Vector.fromList [c])
	    
      (*---------------------------------------------------*)
      (*                     instream                      *)
      (*---------------------------------------------------*)

      datatype instream =
	 In of {fd: FS.file_desc,
		closed: bool ref,
		eof: bool ref,
		first: int ref, (* index of first character *)
		last: int ref,  (* one past the index of the last char *)
		buf: Array.array}

      fun newIn fd = In{fd = fd,
			eof = ref false,
			closed = ref false,
			first = ref 0,
			last = ref 0,
			buf = Array.array(bufSize, 0w0)}

      fun openIn path =
	 newIn(FS.openf(path, FS.O_RDONLY, FS.O.flags[FS.O.binary]))
	 handle exn => raise IO.Io{name = "<unimplemented>",
				   function = "openIn",
				   cause = exn}

      fun updateIn(In{fd, closed, eof, first, last, buf, ...}): unit =
	 if !closed
	    then raise IO.Io{name = "<unimplemented>",
			     function = "<unknown>",
			     cause = IO.ClosedStream}
	 else if !eof
		 then ()
	      else 
		 if !first = !last
		    then (* need to read *)
		       let
			  val bytesRead =
			     PIO.readArr(fd, {buf = buf, i = 0, sz = NONE})
		       in if bytesRead = 0
			     then eof := true
			  else (first := 0; last := bytesRead)
		       end
		 else ()

      val empty = Word8Vector.tabulate (0, fn _ => 0w0)
	 
      fun input(ins as In{eof, buf, first, last, ...}): vector =
	 (updateIn ins
	  ; if !eof
	       then (eof := false; empty)
	    else
	       (
		(Array.extract(buf, !first, SOME(!last - !first)))
		before first := !last))
	 handle exn => raise IO.Io{name = "<unimplemented>",
				   function = "input",
				   cause = exn}

      fun lookahead(ins as In{eof, buf, first, ...}): elem option =
	 (if !eof
	     then NONE
	  else (updateIn ins
		; if !eof
		     then NONE
		  else SOME((Array.sub(buf, !first)))))
	 handle exn => raise IO.Io{name = "<unimplemented>",
				   function = "lookahead",
				   cause = exn}

      fun input1(ins as In{buf, first, ...}): elem option =
	 (case lookahead ins of
	     NONE => NONE
	   | res as SOME _ => (first := 1 + !first;
			       res))
	 handle exn => raise IO.Io{name = "<unimplemented>",
				   function = "input1",
				   cause = exn}

      fun inputN(ins as In{fd, eof, first, last, buf, ...},
		 bytesToRead: int): vector =
	 (if Int.geu(bytesToRead, Array.maxLen)
	     then raise Size
	  else
	     if !eof
		then (eof := false; empty)
	     else
		let val size = !last - !first
		in if size >= bytesToRead
		      then (
			    (Array.extract(buf, !first, SOME bytesToRead))
			    before first := bytesToRead + !first)
		   else
		      let val dst = Array.array(bytesToRead, 0w0)
			 val _ =
			    (Array.copy{src = buf, si = !first, len = SOME size,
					dst = dst, di = 0} ;
			     first := !last)
			 fun loop(bytesRead: int): int =
			    if bytesRead = bytesToRead
			       then bytesRead
			    else let
				    val bytesRead' =
				       PIO.readArr
				       (fd, {buf = dst, i = bytesRead,
					     sz = SOME(bytesToRead - bytesRead)})
				 in if bytesRead' = 0
				       then (eof := true ;
					     bytesRead)
				    else loop(bytesRead + bytesRead')
				 end
			 val bytesRead = loop size
		      in if bytesRead = bytesToRead
			    then
			         (Primitive.Vector.fromArray dst)
			 else (
			       (Array.extract(dst, 0, SOME bytesRead)))
		      end
		end)
	     handle exn => raise IO.Io{name = "<unimplemented>",
				       function = "inputN",
				       cause = exn}

      fun inputAll(ins as In{fd, eof, first, last, buf, ...}) =
	 if !eof
	    then (eof := false; empty)
	 else
	    (let val vs = [Array.extract(buf, !first, SOME(!last - !first))]
		 fun loop vs =
		    let val v = PIO.readVec(fd, bufSize)
		    in if Vector.length v = 0
			  then (Vector.concat(rev vs))
		       else loop(v :: vs)
		    end
	     in loop vs
	     end)
	    handle exn => raise IO.Io{name = "<unimplemented>",
				      function = "inputN",
				      cause = exn}

      (* not entirely correct - really needs to do non blocking lookahead *)
      fun canInput(ins as In{eof, first, last, ...}, n) =
	 (updateIn ins
	  ; if !eof
	       then SOME 0
	    else SOME(Int.min(n, !last - !first)))

      fun closeIn(In{fd, closed, ...}) =
	 (PIO.close fd; closed := true)
	    
      fun endOfStream(ins as In{eof, ...}) =
	 !eof orelse (updateIn ins; !eof)

      (* This is all just a hack so that I can emulate scanStream *)
      structure StreamIO =
	 struct
	    type outstream = outstream'

	    datatype state =
	       Uneval of instream
	     | Eval of (char * lazy) option
	    withtype lazy = state ref

	    type instream = lazy
(*
	    fun make ins = ref(Uneval ins)

	    fun input1' r =
	       case !r of
		  Eval v => v
		| Uneval ins => let val v = (case input1 ins of
						NONE => NONE
					      | SOME c => SOME(c, make ins))
				in r := Eval v; v
				end
	    val input1 = input1'*)
	 end
(*
      fun scanStream f ins =
	 case f StreamIO.input1 (StreamIO.make ins) of
	    NONE => NONE
	  | SOME(v, _) => SOME v	*)
   end
(*
structure TextIOGlobal: TEXT_IO_GLOBAL = TextIO
open TextIO
*)