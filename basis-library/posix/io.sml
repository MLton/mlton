(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure PosixIO: POSIX_IO =
struct

structure Prim = PosixPrimitive.IO
open Prim
structure Error = PosixError
structure SysCall = Error.SysCall
structure FS = PosixFileSys

type file_desc = Prim.file_desc
type pid = Pid.t

val FD = PosixPrimitive.FileDesc.fromInt
val unFD = PosixPrimitive.FileDesc.toInt
   
local
   val a: file_desc array = Array.array (2, FD 0)
in
   fun pipe () =
      SysCall.syscall
      (fn () =>
       (Prim.pipe a,
        fn () => {infd = Array.sub (a, 0),
                  outfd = Array.sub (a, 1)}))
end

fun dup fd = FD (SysCall.simpleResult (fn () => Prim.dup fd))

fun dup2 {new, old} = SysCall.simple (fn () => Prim.dup2 (old, new))

fun close fd = SysCall.simpleRestart (fn () => Prim.close fd)

structure FD =
   struct
      open FD BitFlags
   end

structure O = PosixFileSys.O

datatype open_mode = datatype PosixFileSys.open_mode
         
fun dupfd {base, old} =
   FD (SysCall.simpleResultRestart 
       (fn () => Prim.fcntl3 (old, F_DUPFD, unFD base)))

fun getfd fd =
   Word.fromInt (SysCall.simpleResultRestart 
                 (fn () => Prim.fcntl2 (fd, F_GETFD)))

fun setfd (fd, flags): unit =
   SysCall.simpleRestart
   (fn () => Prim.fcntl3 (fd, F_SETFD, Word.toIntX flags))
                            
fun getfl fd : O.flags * open_mode =
   let 
      val n =
         SysCall.simpleResultRestart (fn () => Prim.fcntl2 (fd, F_GETFL))
      val w = Word.fromInt n
      val flags = Word.andb (w, Word.notb O_ACCMODE)
      val mode = Word.andb (w, O_ACCMODE)
   in (flags, PosixFileSys.wordToOpenMode mode)
   end
      
fun setfl (fd, flags: O.flags): unit  =
   SysCall.simpleRestart
   (fn () => Prim.fcntl3 (fd, F_SETFL, Word.toIntX flags))
         
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
                      
fun lseek (fd, n: Position.int, w: whence): Position.int =
   SysCall.syscall
   (fn () =>
    let val n = Prim.lseek (fd, n, whenceToInt w)
    in (if n = ~1 then ~1 else 0, fn () => n)
    end)
         
fun fsync fd : unit = SysCall.simple (fn () => Prim.fsync fd)
         
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
      (fd, {ltype, whence, start, len, ...}: FLock.flock)
      : FLock.flock  =
      SysCall.syscallRestart
      (fn () =>
       ((P.setType (lockTypeToInt ltype)
         ; P.setWhence (whenceToInt whence)
         ; P.setStart start
         ; P.setLen len
         ; P.fcntl (fd, cmd)), fn () => 
        {ltype = intToLockType (P.typ ()),
         whence = intToWhence (P.whence ()),
         start = P.start (),
         len = P.len (),
         pid = if usepid then SOME (P.pid ()) else NONE}))
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
                 val _ = verifyPos ()
              in
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

   fun make {RD, WR, fromVector, read, setMode, toArraySlice, toVectorSlice,
             vectorLength, write, writeVec} =
      let
         val setMode =
            fn fd =>
            if let
                  open Primitive.MLton.Platform.OS
               in
                  case host of
                     MinGW => true
                   | _ => false
               end
               then setMode fd
            else ()
         fun readArr (fd, sl): int =
            let
               val (buf, i, sz) = ArraySlice.base (toArraySlice sl)
            in
               SysCall.simpleResultRestart (fn () => read (fd, buf, i, sz))
            end
         fun readVec (fd, n) =
            let
               val a = Primitive.Array.array n
               val bytesRead = 
                  SysCall.simpleResultRestart (fn () => read (fd, a, 0, n))
            in 
               fromVector
               (if n = bytesRead
                   then Vector.fromArray a
                else ArraySlice.vector (ArraySlice.slice
                                        (a, 0, SOME bytesRead)))
            end
         fun writeArr (fd, sl) =
            let
               val (buf, i, sz) = ArraySlice.base (toArraySlice sl)
            in
               SysCall.simpleResultRestart
               (fn () => write (fd, buf, i, sz))
            end
         val writeVec =
            fn (fd, sl) =>
            let
               val (buf, i, sz) = VectorSlice.base (toVectorSlice sl)
            in
               SysCall.simpleResultRestart
               (fn () => writeVec (fd, buf, i, sz))
            end
         fun mkReader {fd, name, initBlkMode} =
            let
               val closed = ref false
               val {pos, getPos, setPos, endPos, verifyPos} =
                  posFns (closed, fd)
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
                  in incPos (vectorLength v); v
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
                                   else SOME (Position.toInt
                                              (Position.-
                                               (FS.ST.size (FS.fstat fd),
                                                !pos)))
                  else fn () => if !closed then SOME 0 else NONE
               val () = setMode fd
            in
               RD {avail = avail,
                   block = NONE,
                   canInput = NONE,
                   chunkSize = Primitive.TextIO.bufSize,
                   close = close,
                   endPos = endPos,
                   getPos = getPos,
                   ioDesc = SOME (FS.fdToIOD fd),
                   name = name,
                   readArr = SOME (blockWrap readArr),
                   readArrNB = SOME (noBlockWrap readArr),
                   readVec = SOME (blockWrap readVec),
                   readVecNB = SOME (noBlockWrap readVec),
                   setPos = setPos,
                   verifyPos = verifyPos}
            end
         fun mkWriter {fd, name, initBlkMode, appendMode, chunkSize} =
            let
               val closed = ref false
               val {pos, getPos, setPos, endPos, verifyPos} =
                  posFns (closed, fd)
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
               fun putV x = incPos (writeVec x)
               fun putA x = incPos (writeArr x)
               fun write (put, block) arg = 
                  (ensureOpen (); ensureBlock block; put (fd, arg))
               fun handleBlock writer arg = 
                  SOME(writer arg)
                  handle (e as PosixError.SysErr (_, SOME cause)) =>
                     if cause = PosixError.again then NONE else raise e
               val close = 
                  fn () => if !closed then () else (closed := true; close fd)
               val () = setMode fd
            in
               WR {block = NONE,
                   canOutput = NONE,
                   chunkSize = chunkSize,
                   close = close,
                   endPos = endPos,
                   getPos = getPos,
                   ioDesc = SOME (FS.fdToIOD fd),
                   name = name,
                   setPos = setPos,
                   verifyPos = verifyPos,
                   writeArr = SOME (write (putA, true)),
                   writeArrNB = SOME (handleBlock (write (putA, false))),
                   writeVec = SOME (write (putV, true)),
                   writeVecNB = SOME (handleBlock (write (putV, false)))}
            end
      in
         {mkReader = mkReader,
          mkWriter = mkWriter,
          readArr = readArr,
          readVec = readVec,
          writeArr = writeArr,
          writeVec = writeVec}
      end
in
   val {mkReader = mkBinReader, mkWriter = mkBinWriter,
        readArr, readVec, writeArr, writeVec} =
      make {RD = BinPrimIO.RD,
            WR = BinPrimIO.WR,
            fromVector = Word8Vector.fromPoly,
            read = readWord8,
            setMode = Prim.setbin,
            toArraySlice = Word8ArraySlice.toPoly,
            toVectorSlice = Word8VectorSlice.toPoly,
            vectorLength = Word8Vector.length,
            write = writeWord8,
            writeVec = writeWord8Vec}
   val {mkReader = mkTextReader, mkWriter = mkTextWriter, ...} =
      make {RD = TextPrimIO.RD,
            WR = TextPrimIO.WR,
            fromVector = fn v => v,
            read = readChar,
            setMode = Prim.settext,
            toArraySlice = CharArraySlice.toPoly,
            toVectorSlice = CharVectorSlice.toPoly,
            vectorLength = CharVector.length,
            write = writeChar,
            writeVec = writeCharVec}
end

end
