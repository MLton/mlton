(* modified from SML/NJ sources by Stephen Weeks 1998-6-25 *)
(* modified by Matthew Fluet 2002-10-11 *)
(* modified by Matthew Fluet 2002-11-21 *)

(* os-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * NOTE: this interface has been proposed, but not yet adopted by the
 * Standard basis committee.
 *
 *)

structure OS_IO: OS_IO =
  struct

  (* an iodesc is an abstract descriptor for an OS object that
   * supports I/O (e.g., file, tty device, socket, ...).
   *)
    datatype iodesc = datatype PreOS.IO.iodesc

    datatype iodesc_kind = K of string

  (* return a hash value for the I/O descriptor. *)
    fun hash (FD fd) = Word.fromInt fd

  (* compare two I/O descriptors *)
    fun compare (FD fd1, FD fd2) = Int.compare(fd1, fd2)

    structure Kind =
      struct
	val file = K "FILE"
	val dir = K "DIR"
	val symlink = K "LINK"
	val tty = K "TTY"
	val pipe = K "PIPE"
	val socket = K "SOCK"
	val device = K "DEV"
      end

  (* return the kind of I/O descriptor *)
    fun kind (fd) = let
	  val stat = Posix.FileSys.fstat fd
	  in
	    if      (Posix.FileSys.ST.isReg stat) then Kind.file
	    else if (Posix.FileSys.ST.isDir stat) then Kind.dir
	    else if (Posix.FileSys.ST.isChr stat) then Kind.tty
	    else if (Posix.FileSys.ST.isBlk stat) then Kind.device (* ?? *)
	    else if (Posix.FileSys.ST.isLink stat) then Kind.symlink
	    else if (Posix.FileSys.ST.isFIFO stat) then Kind.pipe
	    else if (Posix.FileSys.ST.isSock stat) then Kind.socket
	    else K "UNKNOWN"
	  end

    type poll_flags = {rd: bool, wr: bool, pri: bool}
    datatype poll_desc = PollDesc of (iodesc * poll_flags)
    datatype poll_info = PollInfo of (iodesc * poll_flags)

  (* create a polling operation on the given descriptor; note that
   * not all I/O devices support polling, but for the time being, we
   * don't test for this.
   *)
    fun pollDesc iod = SOME(PollDesc(iod, {rd=false, wr=false, pri=false}))

  (* return the I/O descriptor that is being polled *)
    fun pollToIODesc (PollDesc(iod, _)) = iod

    exception Poll

  (* set polling events; if the polling operation is not appropriate
   * for the underlying I/O device, then the Poll exception is raised.
   *)
    fun pollIn (PollDesc(iod, {rd, wr, pri})) =
	  PollDesc(iod, {rd=true, wr=wr, pri=pri})
    fun pollOut (PollDesc(iod, {rd, wr, pri})) =
	  PollDesc(iod, {rd=rd, wr=true, pri=pri})
    fun pollPri (PollDesc(iod, {rd, wr, pri})) =
	  PollDesc(iod, {rd=rd, wr=wr, pri=true})

  (* polling function *)
    local
      structure Prim = Primitive.OS.IO
      fun join (false, _, w) = w
        | join (true, b, w) = Word.orb(w, b)
      fun test (w, b) = (Word.andb(w, b) <> 0w0)
      val rdBit : Word.word = Primitive.OS.IO.POLLIN
      and wrBit : Word.word = Primitive.OS.IO.POLLOUT
      and priBit : Word.word = Primitive.OS.IO.POLLPRI
      fun fromPollDesc (PollDesc(FD fd, {rd, wr, pri})) =
	    ( fd,
	      join (rd, rdBit, 
	      join (wr, wrBit, 
              join (pri, priBit, 0w0)))
	    )
      fun toPollInfo (fd, w) = PollInfo(FD fd, {
	      rd = test(w, rdBit), 
	      wr = test(w, wrBit), 
              pri = test(w, priBit)
	    })
    in
    fun poll (pds, timeOut) = let
	  val (fds, eventss) = ListPair.unzip (List.map fromPollDesc pds)
	  val fds = Vector.fromList fds
	  val n = Vector.length fds
	  val eventss = Vector.fromList eventss
          val timeOut =
	     case timeOut of
	        SOME t => Int.fromLarge (Time.toMilliseconds t)
	      | NONE => ~1
	  val reventss = Array.array (n, 0w0)
	  val _ = Posix.Error.checkResult 
                  (Prim.poll (fds, eventss, n, timeOut, reventss))
	  in
	    Array.foldri
	    (fn (i, w, l) => 
	     if w <> 0w0
	       then (toPollInfo (Vector.sub (fds, i), w))::l
	       else l)
	    []
	    reventss
	  end
    end (* local *)

  (* check for conditions *)
    fun isIn (PollInfo(_, flgs)) = #rd flgs
    fun isOut (PollInfo(_, flgs)) = #wr flgs
    fun isPri (PollInfo(_, flgs)) = #pri flgs
    fun infoToPollDesc (PollInfo arg) = PollDesc arg
  end (* OS_IO *)


(*
 * $Log: os-io.sml, v $
 * Revision 1.4  1997/07/31 17:25:26  jhr
 *   We are now using 32-bit ints to represent the seconds portion of a
 *   time value.  This was required to handle the change in the type of
 *   Time.{to, from}{Seconds, Milliseconds, Microseconds}.
 *
 * Revision 1.3  1997/06/07  15:27:51  jhr
 *   SML'97 Basis Library changes (phase 3; Posix changes)
 *
 * Revision 1.2  1997/06/02  19:16:19  jhr
 *   SML'97 Basis Library changes (phase 2)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:25  george
 *   Version 109.24
 *
 *)
