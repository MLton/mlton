(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure PosixIO: POSIX_IO =
   struct
      structure Prim = PosixPrimitive.IO
      open Prim
      structure Error = PosixError
      val checkResult = Error.checkResult
      val checkReturnResult = Error.checkReturnResult
      structure FS = PosixFileSys

      datatype file_desc = datatype Prim.file_desc
      type pid = Prim.pid

      local
	 val a: PosixPrimitive.fd array = Array.array (2, 0)
      in
	 fun pipe () =
	    (checkResult (Prim.pipe a);
	     {infd = FD (Array.sub (a, 0)),
	      outfd = FD (Array.sub (a, 1))})
      end

      fun dup (FD fd) = FD (checkReturnResult (Prim.dup fd))

      fun dup2 {old = FD old, new = FD new} =
	 checkResult (Prim.dup2 (old, new))

      fun close (FD fd) = checkResult (Prim.close fd)

      local
	fun make {read, write} =
	  let
	    fun readArr (FD fd, {buf, i, sz}): int =
	      let
		val max = Array.checkSlice (buf, i, sz)
	      in
		checkReturnResult (read (fd, buf, i, max -? i))
	      end
	    
	    fun readVec (fd, n) =
	      let
		val a = Primitive.Array.array n
		val bytesRead = readArr (fd, {buf = a, i = 0, sz = SOME n})
	      in 
		if n = bytesRead
		  then Vector.fromArray a
		  else Array.extract (a, 0, SOME bytesRead)
	      end
		
	    fun writeVec (FD fd, {buf, i, sz}) =
	      let
		val max = Vector.checkSlice (buf, i, sz)
	      in
		checkReturnResult (write (fd, buf, i, max -? i))
	      end
	    
	    fun writeArr (fd, {buf, i, sz}) =
	      writeVec (fd, {buf = Vector.fromArray buf, i = i, sz = sz})
	  in
	    {readArr = readArr, readVec = readVec,
	     writeVec = writeVec, writeArr = writeArr}
	  end
      in
	val rwChar = make {read = readChar, write = writeChar}
	val rwWord8 = make {read = readWord8, write = writeWord8}
      end
      val {readArr, readVec, writeVec, writeArr} = rwWord8
		      
      structure FD =
	 struct
	    open FD BitFlags
	 end

      structure O = PosixFileSys.O

      datatype open_mode = datatype PosixFileSys.open_mode
	 
      fun dupfd {old = FD old, base = FD base} =
	 FD (checkReturnResult (Prim.fcntl3 (old, F_DUPFD, base)))

      fun getfd (FD fd) =
	 Word.fromInt (checkReturnResult (Prim.fcntl2 (fd, F_GETFD)))

      fun setfd (FD fd, flags): unit =
	 checkResult (Prim.fcntl3 (fd, F_SETFD, Word.toInt flags))
			    
      fun getfl (FD fd): O.flags * open_mode =
	 let val n = Prim.fcntl2 (fd, F_GETFD)
	 in if n < 0
	       then Error.error ()
	    else let val w = Word.fromInt n
		     val flags = Word.andb (w, Word.notb O_ACCMODE)
		     val mode = Word.andb (w, O_ACCMODE)
		 in (flags, PosixFileSys.wordToOpenMode mode)
		 end
	 end
      
      fun setfl (FD fd, flags: O.flags): unit  =
	 checkResult (Prim.fcntl3 (fd, F_SETFL, Word.toInt flags))
	 
      datatype whence = SEEK_SET | SEEK_CUR | SEEK_END

      val whenceToInt =
	 fn SEEK_SET => Prim.SEEK_SET
	  | SEEK_CUR => Prim.SEEK_CUR
	  | SEEK_END => Prim.SEEK_END

      fun intToWhence n =
	 if n = Prim.SEEK_SET
	    then SEEK_SET
	 else if n = Prim.SEEK_CUR
		 then SEEK_CUR
	      else if n = Prim.SEEK_END
		      then SEEK_END
		   else raise Fail "Posix.IO.intToWhence"
		      
      fun lseek (FD fd, n: Position.int, w: whence): Position.int =
	 checkReturnResult (Prim.lseek (fd, n, whenceToInt w))
	 
      fun fsync (FD fd): unit = checkResult (Prim.fsync fd)
	 
      datatype lock_type =
	 F_RDLCK
       | F_WRLCK
       | F_UNLCK

      val lockTypeToInt =
	 fn F_RDLCK => Prim.F_RDLCK
	  | F_WRLCK => Prim.F_WRLCK
	  | F_UNLCK => Prim.F_UNLCK

      fun intToLockType n =
	 if n = Prim.F_RDLCK
	    then F_RDLCK
	 else if n = Prim.F_WRLCK
		 then F_WRLCK
	      else if n = Prim.F_UNLCK
		      then F_UNLCK
		   else raise Fail "Posix.IO.intToLockType"
	 
      structure FLock =
	 struct
	    type flock = {ltype: lock_type,
			  whence: whence,
			  start: Position.int,
			  len: Position.int,
			  pid: pid option}
			 
	    fun flock l = l
	    val ltype: flock -> lock_type = #ltype
	    val whence: flock -> whence = #whence
	    val start: flock -> Position.int = #start
	    val len: flock -> Position.int = #len
	    val pid: flock -> pid option = #pid
	 end

      local
	 structure P = Prim.FLock
	 fun make
	    (cmd, usepid)
	    (FD fd, {ltype, whence, start, len, ...}: FLock.flock)
	    : FLock.flock  =
	    (P.setType (lockTypeToInt ltype)
	     ; P.setWhence (whenceToInt whence)
	     ; P.setStart start
	     ; P.setLen len
	     ; checkResult (P.fcntl (fd, cmd))
	     ; {ltype = intToLockType (P.typ ()),
		whence = intToWhence (P.whence ()),
		start = P.start (),
		len = P.len (),
		pid = if usepid then SOME (P.pid ()) else NONE})
      in
	 val getlk = make (F_GETLK, true)
	 val setlk = make (F_SETLK, false)
	 val setlkw = make (F_SETLKW, false)
      end

      (* Adapted from SML/NJ sources. *)
      (* posix-bin-prim-io.sml
       *
       * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
       *
       * This implements the UNIX version of the OS specific binary primitive
       * IO structure.  The Text IO version is implemented by a trivial translation
       * of these operations (see posix-text-prim-io.sml).
       *
       *)
      local
	val pos0 = Position.fromInt 0
	fun isReg fd = FS.ST.isReg(FS.fstat fd)
	fun posFns (closed, fd) = 
	  if (isReg fd)
	    then let
		   val pos = ref pos0
		   fun getPos () = !pos
		   fun setPos p = (if !closed 
				     then raise IO.ClosedStream 
				     else ();
				   pos := lseek(fd,p,SEEK_SET))
		   fun endPos () = (if !closed 
				      then raise IO.ClosedStream 
				      else ();
				    FS.ST.size(FS.fstat fd))
		   fun verifyPos () = let
					val curPos = lseek(fd, pos0, SEEK_CUR)
				      in
					pos := curPos; curPos
				      end
		 in
		   verifyPos ();
		   {pos = pos,
		    getPos = SOME getPos,
		    setPos = SOME setPos,
		    endPos = SOME endPos,
		    verifyPos = SOME verifyPos}
		 end
	    else {pos = ref pos0,
		  getPos = NONE, 
		  setPos = NONE, 
		  endPos = NONE, 
		  verifyPos = NONE}

	fun make {readArr, readVec, writeVec, writeArr} (RD, WR) =
	  let
	    fun mkReader {fd, name, initBlkMode} =
	      let
		val closed = ref false
		val {pos, getPos, setPos, endPos, verifyPos} = posFns (closed, fd)
		val blocking = ref initBlkMode
		fun blockingOn () = 
		  (setfl(fd, O.flags[]); blocking := true)
		fun blockingOff () = 
		  (setfl(fd, O.nonblock); blocking := false)
		fun ensureOpen () = 
		  if !closed then raise IO.ClosedStream else ()
		fun incPos k = pos := Position.+ (!pos, Position.fromInt k)
		val readVec = fn n => 
		  let val v = readVec (fd, n)
		  in incPos (Vector.length v); v
		  end
		val readArr = fn x => 
		  let val k = readArr (fd, x)
		  in incPos k; k
		  end
		fun blockWrap f x =
		  (ensureOpen ();
		   if !blocking then () else blockingOn ();
		   f x)
		fun noBlockWrap f x =
		  (ensureOpen ();
		   if !blocking then blockingOff () else ();
		   (SOME (f x)
		    handle (e as PosixError.SysErr (_, SOME cause)) =>
		    if cause = PosixError.again then NONE else raise e))
		val close = 
		  fn () => if !closed then () else (closed := true; close fd)
		val avail = 
		  if isReg fd
		    then fn () => if !closed 
				    then SOME 0
				    else SOME(Position.-(FS.ST.size(FS.fstat fd), !pos))
		    else fn () => if !closed then SOME 0 else NONE
	      in
		RD {name = name,
		    chunkSize = Primitive.TextIO.bufSize,
		    readVec = SOME (blockWrap readVec),
		    readArr = SOME (blockWrap readArr),
		    readVecNB = SOME (noBlockWrap readVec),
		    readArrNB = SOME (noBlockWrap readArr),
		    block = NONE,
		    canInput = NONE,
		    avail = avail,
		    getPos = getPos,
		    setPos = setPos,
		    endPos = endPos,
		    verifyPos = verifyPos,
		    close = close,
		    ioDesc = SOME (FS.fdToIOD fd)}
	      end
	    fun mkWriter {fd, name, initBlkMode, appendMode, chunkSize} =
	      let
		val closed = ref false
		val {pos, getPos, setPos, endPos, verifyPos} = posFns (closed, fd)
		fun incPos k = (pos := Position.+ (!pos, Position.fromInt k); k)
		val blocking = ref initBlkMode
		val appendFlgs = O.flags(if appendMode then [O.append] else [])
		fun updateStatus () = 
		  let
		    val flgs = if !blocking
				 then appendFlgs
				 else O.flags [O.nonblock, appendFlgs]
		  in
		    setfl(fd, flgs)
		  end
		fun ensureOpen () = 
		  if !closed then raise IO.ClosedStream else ()
		fun ensureBlock x = 
		  if !blocking then () else (blocking := x; updateStatus ())
		fun putV x = incPos(writeVec x)
		fun putA x = incPos(writeArr x)
		fun write (put, block) arg = 
		  (ensureOpen (); ensureBlock block; put (fd, arg))
		fun handleBlock writer arg = 
		  SOME(writer arg)
		  handle (e as PosixError.SysErr (_, SOME cause)) =>
		    if cause = PosixError.again then NONE else raise e
		val close = 
		  fn () => if !closed then () else (closed := true; close fd)
	      in
		WR {name = name,
		    chunkSize = chunkSize,
		    writeVec = SOME (write (putV, true)),
		    writeArr = SOME (write (putA, true)),
		    writeVecNB = SOME (handleBlock (write (putV, false))),
		    writeArrNB = SOME (handleBlock (write (putA, false))),
		    block = NONE,
		    canOutput = NONE,
		    getPos = getPos,
		    setPos = setPos,
		    endPos = endPos,
		    verifyPos = verifyPos,
		    close = close,
		    ioDesc = SOME (FS.fdToIOD fd)}
	      end
	  in
	    {mkReader = mkReader, mkWriter = mkWriter}
	  end
      in
	val {mkReader = mkBinReader, mkWriter = mkBinWriter} =
	  make rwWord8 (BinPrimIO.RD, BinPrimIO.WR)
	val {mkReader = mkTextReader, mkWriter = mkTextWriter} =
	  make rwChar (TextPrimIO.RD, TextPrimIO.WR)
      end
   end
