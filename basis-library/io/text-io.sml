(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor TextIO (
	       structure Int:
		  sig
		     val geu: int * int -> bool
		     val min: int * int -> int
		  end
	       structure Primitive:
		  sig
		     val safe: bool

		     structure Array:
			sig
			   val array: int -> 'a array
			end
		     structure Stdio:
			sig
			   val print: string -> unit
			end
		     structure String:
			sig
			   val fromWord8Vector: Word8Vector.vector -> string
			   val toWord8Vector: string -> Word8Vector.vector
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
		  end
	       structure Cleaner:
		  sig
		     type t

		     val addNew: t * (unit -> unit) -> unit
		     val atExit: t
		     val atLoadWorld: t
		     val atSaveWorld: t
		  end
	       ) =
struct

structure FS = Posix.FileSys
structure PIO = Posix.IO
   
structure Array = Word8Array
structure Vector = Word8Vector

structure String =
   struct
      open Primitive.String
      open String
   end
type vector = string
type elem = char

val bufSize: int = 4096

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

val newOut =
   let
      val _ =
	 Cleaner.addNew (Cleaner.atSaveWorld, fn () =>
			 List.app flushOut (!openOuts))
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
      (newOut (FS.creat (path, readWrite)))
      handle exn => raise IO.Io {name = "<unimplemented>",
				 function = "openOut",
				 cause = exn}
	 
   fun openAppend path =
      (newOut (FS.createf (path, FS.O_WRONLY, FS.O.append, readWrite)))
      handle exn => raise IO.Io {name = "<unimplemented>",
				 function = "openAppend",
				 cause = exn}
end

fun output (out as ref (Out {fd, closed, bufStyle, ...}), s: string): unit =
   let
      val v = String.toWord8Vector s
   in
      if !closed
	 then raise IO.Io {name = "<unimplemented>",
			   function = "output",
			   cause = IO.ClosedStream}
      else
	 let
	    fun put () =
	       flushGen (fd, v, 0, Vector.length v, PIO.writeVec)
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
	     | Line b => doit (b, fn () => Char.contains s #"\n")
	     | Buffered b => doit (b, fn () => false)
	 end handle exn => raise IO.Io {name = "<unimplemented>",
					function = "output",
					cause = exn}
   end

fun output1 (out, c: elem): unit = output (out, str c)

fun outputSubstr (out, ss): unit = output (out, Substring.string ss)
   
fun print (s: string) = (output (stdOut, s); flushOut stdOut)

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
			   buf = Array.array (bufSize, 0w0)}
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

      val empty = ""

      fun input (b as T {buf, closed, eof, fd, first, last, ...}) =
	 let
	    val f = !first
	    val l = !last
	 in
	    if f < l
	       then
		  (first := l
		   ; (String.fromWord8Vector
		      (Array.extract (buf, f, SOME (l - f)))))
	    else
	       if !closed orelse !eof
		  then empty
	       else
		  let
		     val v = PIO.readVec (fd, bufSize)
		  in
		     if 0 = Word8Vector.length v
			then (eof := true; empty)
		     else String.fromWord8Vector  v
		  end
	 end

      fun lastChar s = String.sub (s, String.size s - 1)

      fun inputLine (b as T {eof, buf, first, last, ...}) =
	 let
	    fun loop (ac: string list) =
	       if update (b, "inputLine")
		  then
		     let
			(* !first < !last *)
			fun loop' i = (* pre: !first <= i <= !last *)
			   let
			      val f = !first
			      fun done j = (* pre: !first < j <= !last *)
				 (first := j
				  ; (String.fromWord8Vector
				     (Array.extract (buf, f, SOME (j -? f)))
				     :: ac))
			   in if i >= !last
				 then loop (done i)
			      else (case Byte.byteToChar (Array.sub (buf, i)) of
				       #"\n" => done (i + 1)
				     | _ => loop' (i + 1))
			   end
		     in loop' (! first)
		     end
	       else (eof := false; ac)
	    val ac = loop []
	 in
	    case ac of
	       [] => ""
	     | s :: _ => concat (rev (case lastChar s of
					 #"\n" => ac
				       | _ => "\n" :: ac))
	 end

      fun lookahead (b as T {buf, eof, first, last, ...}): elem option =
	 let
	    val f = !first
	 in
	    if f < !last
	       then SOME (Byte.byteToChar (Array.sub (buf, f)))
	    else
	       if update (b, "lookahead")
		  then SOME (Byte.byteToChar (Array.sub (buf, 0)))
	       else NONE
	 end

      fun input1 (b as T {buf, first, last, ...}): elem option =
	 let
	    val f = !first
	 in
	    if f < !last
	       then (first := f + 1;
		     SOME (Byte.byteToChar (Array.sub (buf, f))))
	    else
	       if update (b, "input1")
		  then (first := 1; SOME (Byte.byteToChar (Array.sub (buf, 0))))
	       else NONE
	 end

      fun inputN (T {fd, eof, first, last, buf, ...}, bytesToRead: int): vector =
	 if !eof
	    then (eof := false; empty)
	 else
	    let val size = !last -? !first
	    in if size >= bytesToRead
		  then (String.fromWord8Vector
			(Array.extract (buf, !first, SOME bytesToRead))
			before first := bytesToRead +? !first)
	       else
		  let
		     val dst = Array.array (bytesToRead, 0w0)
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
			then String.fromWord8Vector
			   (Primitive.Vector.fromArray dst)
		     else (String.fromWord8Vector
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
	    then (eof := false; empty)
	 else
	    let val vs = [Array.extract (buf, !first, SOME (!last -? !first))]
	       fun loop vs =
		  let val v = PIO.readVec (fd, bufSize)
		  in if Vector.length v = 0
			then (String.fromWord8Vector
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
	       T {buf = Array.array (0, 0w0),
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
			   val buf = Array.array (bufSize, 0w0)
			   val n = PIO.readArr (fd, {buf = buf, i = 0, sz = NONE})
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
	 SOME (Byte.byteToChar (Array.sub (Chain.buf chain, pos)),
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
	 
      fun inputLine (s: t) =
	 let
	    fun loop (s: t, ac: char list): string * t =
	       case input1 s of
		  NONE =>
		     let fun done ac = (implode (rev ac), s)
		     in case ac of
			[] => ("", s)
		      | #"\n" :: _ => done ac
		      | _ => done (#"\n" :: ac)
		     end
		| SOME (c, s) =>
		     if c = #"\n"
			then (implode (rev (#"\n" :: ac)), s)
		     else loop (s, c :: ac)
	 in loop (s, [])
	 end

      fun input (T {pos, chain as Chain.T {buf, ...}}): vector * t =
	 let
	    val rest = T {pos = 0, chain = Chain.next chain}
	 in
	    if pos < Array.length buf orelse pos = 0
	       then (Primitive.String.fromWord8Vector
		     (Array.extract (buf, pos, NONE)),
		     rest)
	    else input rest
	 end
      
      fun endOfStream (T {pos, chain as Chain.T {buf, ...}}): bool =
	 pos = Array.length buf
	 andalso (0 = Array.length buf
		  orelse endOfStream (T {pos = 0, chain = Chain.next chain}))

      fun inputN (s: t, n: int): vector * t =
	 if n = 0 then ("", s)
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
	    in (Primitive.String.fromWord8Vector
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
	       in if v = ""
		     then (concat (rev ac), s)
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
   
fun openIn path =
   newIn (FS.openf (path, FS.O_RDONLY, FS.O.flags []))
   handle exn => raise IO.Io {name = "<unimplemented>",
			      function = "openIn",
			      cause = exn}

val stdIn = newIn FS.stdin

fun inputLine (T r): string =
   case !r of
      Buf b => Buf.inputLine b
    | Stream s => let val (res, s) = StreamIO.inputLine s
		  in r := Stream s; res
		  end

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

structure TextIO: TEXT_IO_EXTRA = TextIO (structure Int = Int
					  structure Primitive = Primitive
					  structure String = String
					  structure Cleaner = Cleaner)
structure TextIOGlobal: TEXT_IO_GLOBAL = TextIO
open TextIOGlobal
