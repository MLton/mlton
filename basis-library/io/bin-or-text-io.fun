functor BinOrTextIO
   (
    val fileTypeFlags: Posix.FileSys.O.flags list

    structure Cleaner:
       sig
	  type t

	  val addNew: t * (unit -> unit) -> unit
	  val atExit: t
       end

    structure Int:
       sig
	  val min: int * int -> int
       end

    structure NativeVector:
       sig
	  type elem
	  type vector

	  val concat: vector list -> vector
	  val empty: vector
	  val fromByte: Word8Vector.elem -> elem
	  val fromWord8Vector: Word8Vector.vector -> vector
	  val hasLine: vector -> bool
	  val isEmpty: vector -> bool
	  val isLine: elem -> bool
	  val toByte: elem -> Word8Vector.elem
	  val toWord8Vector: vector -> Word8Vector.vector
       end

    structure Primitive:
       sig
	  val safe: bool

	  structure Array:
	     sig
		val array: int -> 'a array
	     end
	  structure String:
	     sig
		val fromWord8Vector: Word8Vector.vector -> string
		val toWord8Vector: string -> Word8Vector.vector
	     end
	  structure TextIO:
	     sig
		val bufSize: int
	     end
	  structure Vector:
	     sig
		val fromArray: Word8Array.array -> Word8Vector.vector
	     end
       end
    structure String:
       sig
	  val extract: string * int * int option -> string
	  val size: string -> int
	  val sub: string * int -> char
       end) =
struct

structure FS = Posix.FileSys
structure PIO = Posix.IO
   
structure Array = Word8Array
structure Vector = Word8Vector

type vector = NativeVector.vector
type elem = NativeVector.elem

val bufSize: int = Primitive.TextIO.bufSize

(*---------------------------------------------------*)
(*                     outstream                     *)
(*---------------------------------------------------*)

datatype buf =
   Buf of {size: int ref,
	   array: Array.array}

fun isFull (Buf {size, ...}) = !size = bufSize

(* Write out to fd size bytes of buf starting at index i. *)
fun flushGen (fd: FS.file_desc,
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
	    loop (i + write (fd, {buf = buf,
				  i = i,
				  sz = SOME (max - i)}))
   in loop i
   end

fun flush (fd, Buf {size, array}) =
   (flushGen (fd, array, 0, !size, PIO.writeArr)
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

fun outFd (ref (Out {fd, ...})) = fd

val mkOutstream = ref
val getOutstream = !
val setOutstream = op :=
   
fun flushOut (ref (Out {fd, bufStyle, closed, ...})): unit =
   (case (!closed, bufStyle) of
       (true, _) => ()
     | (_,    Unbuffered) => ()
     | (_,    Line b) => flush (fd, b)
     | (_,    Buffered b) => flush (fd, b))
       handle exn => raise IO.Io {name = "<unimplemented>",
				  function = "flushOut",
				  cause = exn}

val openOuts: outstream list ref = ref []

fun closeOut (out as ref (Out {fd, closed, ...})): unit =
   if !closed
      then ()
   else
      let
	 fun clean () =
	    (closed := true
	     ; PIO.close fd
	     ; openOuts := List.filter (fn out' => out <> out') (!openOuts))
      in (* flushOut out must be before closed := true *)
	 (flushOut out; clean ())
	 handle exn => (clean (); raise IO.Io {name = "<unimplemented",
					       function = "closeOut",
					       cause = exn})
      end

fun getPosOut(out as ref (Out {fd, bufStyle, ...})) =
   let
      val streamPos = Posix.IO.lseek (fd, 0, Posix.IO.SEEK_CUR)
      val bufPos =
	 case bufStyle of
	    Unbuffered => 0
	  | Buffered (Buf {size, ...}) => !size
	  | Line (Buf {size, ...}) => !size
   in
      streamPos + bufPos
   end

val newOut =
   let
      val _ = 
	 Cleaner.addNew
	 (Cleaner.atExit, fn () =>
	  List.app (fn out as ref (Out {fd,...}) =>
		    if fd = FS.stdout orelse fd = FS.stderr
		       then flushOut out
		    else closeOut out) (!openOuts))
   in
      fn (fd, bufStyle) =>
      let
	 val out = ref (Out {fd = fd,
			     closed = ref false,
			     bufStyle = bufStyle})
      in openOuts := out :: !openOuts
	 ; out
      end
   end

val stdErr = newOut (FS.stderr, Unbuffered)

val newOut =
   fn fd =>
   let
      val b = Buf {size = ref 0,
		   array = Primitive.Array.array bufSize}
   in newOut (fd,
	      if Posix.ProcEnv.isatty fd
		 then Line b
	      else Buffered b)
   end

val stdOut = newOut FS.stdout

local
   val readWrite =
      let open FS.S
      in flags [irusr, iwusr, irgrp, iwgrp, iroth, iwoth]
      end
in
   fun openOut path =
      (newOut (FS.createf (path,
                           FS.O_WRONLY,
                           FS.O.flags (FS.O.trunc::fileTypeFlags),
                           readWrite)))
      handle exn => raise IO.Io {name = "<unimplemented>",
				 function = "openOut",
				 cause = exn}
	 
   fun openAppend path =
      (newOut (FS.createf (path,
                           FS.O_WRONLY,
                           FS.O.flags (FS.O.append::fileTypeFlags),
                           readWrite)))
      handle exn => raise IO.Io {name = "<unimplemented>",
				 function = "openAppend",
				 cause = exn}
end

fun output (out as ref (Out {fd, closed, bufStyle, ...}), s): unit =
   if !closed
      then raise IO.Io {name = "<unimplemented>",
			function = "output",
			cause = IO.ClosedStream}
   else
      let
	 val v = NativeVector.toWord8Vector s
	 fun put () = flushGen (fd, v, 0, Vector.length v, PIO.writeVec)
	 fun doit (b as Buf {size, array}, maybe) =
	    let
	       val curSize = !size
	       val newSize = curSize + Vector.length v
	    in
	       if newSize >= Array.length array orelse maybe ()
		  then (flush (fd, b); put ())
	       else
		  (Array.copyVec {src = v, si = 0, len = NONE,
				  dst = array, di = curSize}
		   ; size := newSize)
	    end
      in
	 case bufStyle of
	    Unbuffered => put ()
	  | Line b => doit (b, fn () => NativeVector.hasLine s)
	  | Buffered b => doit (b, fn () => false)
      end handle exn => raise IO.Io {name = "<unimplemented>",
				     function = "output",
				     cause = exn}

local
   val buf1 = Primitive.Array.array 1
in
   fun output1 (out as ref (Out {fd, closed, bufStyle, ...}), c: elem): unit =
      if !closed
	 then raise IO.Io {name = "<unimplemented>",
			   function = "output1",
			   cause = IO.ClosedStream}
      else
	 let
	    fun doit (b as Buf {size, array}, maybe) =
	       let
		  val _ =
		     if 1 + !size >= Array.length array
			then flush (fd, b)
		     else ()
		  val _ = Array.update (array, !size, NativeVector.toByte c)
		  val _ = size := 1 + !size
		  val _ =
		     if maybe
			then flush (fd, b)
		     else ()
	       in
		  ()
	       end
	 in
	    case bufStyle of
	       Unbuffered =>
		  (Array.update (buf1, 0, NativeVector.toByte c)
		   ; flushGen (fd, buf1, 0, 1, PIO.writeArr))
	     | Line b => doit (b, NativeVector.isLine c)
	     | Buffered b => doit (b, false)
	 end handle exn => raise IO.Io {name = "<unimplemented>",
					function = "output",
					cause = exn}
end

(* ------------------------------------------------- *)
(*                   input buffer                    *)
(* ------------------------------------------------- *)

structure Buf =
   struct
      (* Inv: if !first < !last then not (!closed) andalso not (!eof). *)
      datatype t = 
	 T of {buf: Array.array,
	       closed: bool ref,
	       eof: bool ref,
	       fd: FS.file_desc,
	       first: int ref, (* index of first character *)
	       last: int ref  (* one past the index of the last char *)
	       }

      local fun make f (T r) = f r
      in
	 val closed = make #closed
	 val fd = make #fd
      end

      val isClosed = ! o closed

      val openIns: t list ref = ref []

      fun reallyClose (closed: bool ref, fd): unit =
	 (closed := true
	  ; PIO.close fd
	  ; openIns := (List.filter (fn T {closed = c', ...} => c' <> closed)
			(!openIns)))
	 
      fun closeIn (T {closed, fd, first, last,...}) =
	 if !closed
	    then ()
	 else (first := !last
	       ; reallyClose (closed, fd))
	    
      val newIn =
	 let
	    val _ =
	       Cleaner.addNew
	       (Cleaner.atExit, fn () =>
		List.app
		(fn b as T {fd, ...} => if fd = FS.stdin
					   then ()
					else closeIn b)
		(!openIns))
	 in
	    fn fd =>
	    let val b = T {fd = fd,
			   eof = ref false,
			   closed = ref false,
			   first = ref 0,
			   last = ref 0,
			   buf = Primitive.Array.array bufSize}
	    in openIns := b :: !openIns
	       ; b
	    end
	 end

      (* update returns true iff there is a character now available.
       * Equivalently, it returns the value of not (!eof).
       *)
      fun update (T {buf, closed, eof, fd, first, last, ...},
		  function: string): bool =
	 if !closed
	    then raise IO.Io {name = "<unimplemented>",
			      function = function,
			      cause = IO.ClosedStream}
	 else if !eof
		 then false
	      else 
		 !first < !last
		 orelse
		 (* need to read *)
		 let
		    val bytesRead =
		       PIO.readArr (fd, {buf = buf, i = 0, sz = NONE})
		 in if bytesRead = 0
		       then (eof := true; false)
		    else (first := 0; last := bytesRead; true)
		 end

      fun input (b as T {buf, closed, eof, fd, first, last, ...}) =
	 let
	    val f = !first
	    val l = !last
	 in
	    if f < l
	       then
		  (first := l
		   ; (NativeVector.fromWord8Vector
		      (Array.extract (buf, f, SOME (l - f)))))
	    else
	       if !closed orelse !eof
		  then NativeVector.empty
	       else
		  let
		     val v = PIO.readVec (fd, bufSize)
		  in
		     if 0 = Word8Vector.length v
			then (eof := true; NativeVector.empty)
		     else NativeVector.fromWord8Vector  v
		  end
	 end

      fun lookahead (b as T {buf, eof, first, last, ...}): elem option =
	 let
	    val f = !first
	 in
	    if f < !last
	       then SOME (NativeVector.fromByte (Array.sub (buf, f)))
	    else
	       if update (b, "lookahead")
		  then SOME (NativeVector.fromByte (Array.sub (buf, 0)))
	       else NONE
	 end

      fun input1 (b as T {buf, first, last, ...}): elem option =
	 let
	    val f = !first
	 in
	    if f < !last
	       then (first := f + 1;
		     SOME (NativeVector.fromByte (Array.sub (buf, f))))
	    else
	       if update (b, "input1")
		  then (first := 1; SOME (NativeVector.fromByte (Array.sub (buf, 0))))
	       else NONE
	 end

      fun inputN (T {fd, eof, first, last, buf, ...}, bytesToRead: int): vector =
	 if !eof
	    then (eof := false; NativeVector.empty)
	 else
	    let val size = !last -? !first
	    in if size >= bytesToRead
		  then (NativeVector.fromWord8Vector
			(Array.extract (buf, !first, SOME bytesToRead))
			before first := bytesToRead +? !first)
	       else
		  let
		     val dst = Primitive.Array.array bytesToRead
		     val _ =
			(Array.copy {src = buf, si = !first,
				     len = SOME size, dst = dst, di = 0}
			 ; first := !last)
		     fun loop (bytesRead: int): int =
			if bytesRead = bytesToRead
			   then bytesRead
			else let
				val bytesRead' =
				   PIO.readArr
				   (fd, {buf = dst, i = bytesRead,
					 sz = SOME (bytesToRead
						    -? bytesRead)})
			     in if bytesRead' = 0
				   then (eof := true
					 ; first := !last
					 ; bytesRead)
				else loop (bytesRead +? bytesRead')
			     end
		     val bytesRead = loop size
		  in if bytesRead = bytesToRead
			then NativeVector.fromWord8Vector
			   (Primitive.Vector.fromArray dst)
		     else (NativeVector.fromWord8Vector
			   (Array.extract (dst, 0, SOME bytesRead)))
		  end
	    end
	 handle exn => raise IO.Io {name = "<unimplemented>",
				    function = "inputN",
				    cause = exn}

      (* not entirely correct - really needs to do non blocking lookahead *)
      fun canInput (b as T {eof, first, last, ...}, n) =
	 let
	    val f = !first
	    val l = !last
	 in if f < l
	       then SOME (Int.min (n, l -? f))
	    else if update (b, "canInput")
		    then SOME (Int.min (n, !last))
		 else SOME 0
	 end

      fun inputAll (T {fd, eof, first, last, buf, ...}) =
	 if !eof
	    then (eof := false; NativeVector.empty)
	 else
	    let val vs = [Array.extract (buf, !first, SOME (!last -? !first))]
	       fun loop vs =
		  let val v = PIO.readVec (fd, bufSize)
		  in if Vector.length v = 0
			then (NativeVector.fromWord8Vector
			      (Vector.concat (rev vs)))
		     else loop (v :: vs)
		  end
	    in loop vs
	    end handle exn => raise IO.Io {name = "<unimplemented>",
					   function = "inputN",
					   cause = exn}

      fun endOfStream (b as T {eof, ...}) =
	 !eof orelse not (update (b, "endOfStream"))
   end

(* ------------------------------------------------- *)
(*                     StreamIO                      *)
(* ------------------------------------------------- *)

structure StreamIO =
   struct
      type out_pos = int
      type pos = int
      fun filePosOut x = x
	 
      structure Chain =
	 struct
	    datatype t = T of {buf: Array.array,
			       closed: bool ref,
			       fd: FS.file_desc,
			       next: t option ref}
	    local
	       fun make f (T r) = f r
	    in
	       val buf = make #buf
	       val fd = make #fd
	    end

	    fun isEmpty (T {buf, ...}) = 0 = Array.length buf

	    val empty as T {next, ...} =
	       T {buf = Primitive.Array.array 0,
		  closed = ref true, (* doesn't matter *)
		  fd = FS.stdin, (* doesn't matter *)
		  next = ref NONE}
	    val _ = next := SOME empty
	       
	    fun fromBuf (Buf.T {buf, closed, fd, first, last, ...}) =
	       let val first = !first
		  val last = !last
	       in T {buf = Array.tabulate (last -? first, fn i =>
					   Array.sub (buf, i -? first)),
		     fd = fd,
		     closed = closed,
		     next = ref NONE}
	       end

	    fun closeIn (T {closed, fd, ...}) =
	       if !closed
		  then ()
	       else (closed := true
		     ; Buf.reallyClose (closed, fd))

	    fun next (T {buf, closed, fd, next}): t =
	       case !next of
		  SOME c => c
		| NONE =>
		     if !closed
			then empty
		     else
			let
			   val buf = Primitive.Array.array bufSize
			   val n =
			      PIO.readArr (fd, {buf = buf, i = 0, sz = NONE})
			   val buf =
			      if n < bufSize
				 then Array.tabulate (n, fn i =>
						      Array.sub (buf, i))
			      else buf
			   val c = T {buf = buf,
				      closed = closed,
				      fd = fd,
				      next = ref NONE}
			   val _ = next := SOME c
			in c
			end
	 end

      datatype t = T of {pos: int,
			 chain: Chain.t}
      type instream = t
      type outstream = outstream'
      type elem = elem
      type vector = vector

      local
	 fun make f (T r) = f r
      in
	 val chain = make #chain
      end

      val fd = Chain.fd o chain

      fun input1 (T {pos, chain}) =
	 SOME (NativeVector.fromByte (Array.sub (Chain.buf chain, pos)),
	       T {pos = pos + 1,
		  chain = chain})
	 handle Subscript =>
	    let val c = Chain.next chain
	    in if Chain.isEmpty c
		  then NONE
	       else input1 (T {pos = 0, chain = c})
	    end

      fun fromBuf (b as Buf.T {first, ...}): t =
	 T {pos = 0,
	    chain = Chain.fromBuf b}

      fun closeIn (T {chain, ...}) = Chain.closeIn chain

      fun input (T {pos, chain as Chain.T {buf, ...}}): vector * t =
	 let
	    val rest = T {pos = 0, chain = Chain.next chain}
	 in
	    if pos < Array.length buf orelse pos = 0
	       then (NativeVector.fromWord8Vector
		     (Array.extract (buf, pos, NONE)),
		     rest)
	    else input rest
	 end
      
      fun endOfStream (T {pos, chain as Chain.T {buf, ...}}): bool =
	 pos = Array.length buf
	 andalso (0 = Array.length buf
		  orelse endOfStream (T {pos = 0, chain = Chain.next chain}))

      fun inputN (s: t, n: int): vector * t =
	 if n = 0 then (NativeVector.empty, s)
	 else
	    let
	       (* Pre: need > 0 *)
	       fun loop (T {pos, chain as Chain.T {buf, ...}},
			 need: int, ac: Word8Vector.vector list)
		  : t * Word8Vector.vector list =
		  let val available = Array.length buf -? pos
		  in if need <= available
			then (T {pos = pos +? need, chain = chain},
			      Array.extract (buf, pos, SOME need) :: ac)
		     else loop (T {pos = 0, chain = Chain.next chain},
				need -? available,
				Array.extract (buf, pos, NONE) :: ac)
		  end
	       val (s, ac) = loop (s, n, [])
	    in (NativeVector.fromWord8Vector
		(Word8Vector.concat (rev ac)),
		s)
	    end
	      
      fun canInput (T {pos, chain = Chain.T {buf, closed, ...}},
		    n: int): int option =
	 let val available = Array.length buf -? pos
	 in if available > 0 orelse pos = 0
	       then SOME (Int.min (n, available))
	    else if !closed
		    then SOME 0
		 else NONE
	 end
      
      fun inputAll' (s: t): vector * t =
	 let
	    fun loop (s, ac) =
	       let val (v, s) = input s
	       in if NativeVector.isEmpty v
		     then (NativeVector.concat (rev ac), s)
		  else loop (s, v :: ac)
	       end
	 in loop (s, [])
	 end

      val inputAll = #1 o inputAll'
   end

datatype t' =
   Buf of Buf.t
  | Stream of StreamIO.t
datatype t = T of t' ref
type instream = t

fun inFd (T r) =
   case !r of
      Buf b => Buf.fd b
    | Stream s => StreamIO.fd s

fun closeIn (T r) = 
   case !r of
      Buf b => Buf.closeIn b
    | Stream s => StreamIO.closeIn s

fun newIn fd = T (ref (Buf (Buf.newIn fd)))
   
val stdIn = newIn FS.stdin

fun openIn path =
   newIn (FS.openf (path,
                    FS.O_RDONLY,
                    FS.O.flags fileTypeFlags))
   handle exn => raise IO.Io {name = "<unimplemented>",
			      function = "openIn",
			      cause = exn}

fun input (T r): vector =
   case !r of
      Buf b => Buf.input b
    | Stream s => let val (res, s) = StreamIO.input s
		  in r := Stream s; res
		  end

fun lookahead (T r): elem option =
   case !r of
      Buf b => Buf.lookahead b
    | Stream s => (case StreamIO.input1 s of
		      NONE => NONE
		    | SOME (c, _) => SOME c)
	 
fun input1 (T r): elem option =
   case !r of
      Buf b => Buf.input1 b
    | Stream s => (case StreamIO.input1 s of
		      NONE => NONE
		    | SOME (c, s) => (r := Stream s; SOME c))
	 
fun inputN (T r, n: int): vector =
   case !r of
      Buf b => Buf.inputN (b, n)
    | Stream s => let val (res, s) = StreamIO.inputN (s, n)
		  in r := Stream s; res
		  end

fun canInput (T r, i) =
   case !r of
      Buf b => Buf.canInput (b, i)
    | Stream s => StreamIO.canInput (s, i)

fun inputAll (T r) =
   case !r of
      Buf b => Buf.inputAll b
    | Stream s => let val (res, s) = StreamIO.inputAll' s
		  in r := Stream s; res
		  end

fun endOfStream (T r) =
   case !r of
      Buf b => Buf.endOfStream b
    | Stream s => StreamIO.endOfStream s

fun getInstream (T r) =
   case !r of
      Buf b => let val s = StreamIO.fromBuf b
		   val _ = r := Stream s
	       in s
	       end
    | Stream s => s

fun setInstream (T r, s) = r := Stream s
   
fun scanStream f ins =
   case f StreamIO.input1 (getInstream ins) of
      NONE => NONE
    | SOME (v, s) => (setInstream (ins, s); SOME v)

fun mkInstream (s: StreamIO.t): t =
   T (ref (Stream s))

end
