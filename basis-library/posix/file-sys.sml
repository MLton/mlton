(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure PosixFileSys: POSIX_FILESYS_EXTRA =
   struct
      (* Patch to make Time look like it deals with Int.int
       * instead of LargeInt.int.
       *)
      structure Time =
	 struct
	    open Time
	    val toSeconds = LargeInt.toInt o toSeconds
	    val fromSeconds = fromSeconds o LargeInt.fromInt
	 end
      
      structure Error = PosixError
      structure Prim = PosixPrimitive.FileSys
      open Prim
      structure Stat = Prim.Stat
      structure Flags = PosixFlags

      val checkResult = Error.checkResult

      datatype file_desc = datatype Prim.file_desc
      type uid = Prim.uid
      type gid = Prim.gid

      fun fdToWord (FD n) = SysWord.fromInt n
      val wordToFD = FD o SysWord.toInt
      fun id x = x
      val fdToIOD = id
      val iodToFD = SOME

      (*------------------------------------*)
      (*             dirstream              *)
      (*------------------------------------*)

      local
	 structure Prim = Prim.Dirstream
	 datatype dirstream = DS of Prim.dirstream option ref

	 fun get (DS r) =
	    case !r of
	       NONE => Error.raiseSys Error.badf
	     | SOME d => d
      in
	 type dirstream = dirstream
	    
	 fun opendir s =
	    let val d = Prim.opendir (String.nullTerm s)
	    in if Primitive.Cpointer.isNull d
		  then Error.error ()
	       else DS (ref (SOME d))
	    end

	 fun readdir d =
	    let
	       val d = get d
	       fun loop () =
		  let
		     val _ = Error.clearErrno ()
		     val cs = Prim.readdir d
		  in if Primitive.Cpointer.isNull cs
			then if Error.getErrno () = 0
				then ""
			     else Error.error ()
		     else (case C.CS.toString cs of
			      "." => loop ()
			    | ".." => loop ()
			    | s => s)
		  end
	    in loop ()
	    end

	 fun rewinddir d =
	    let val d = get d
	    in Error.clearErrno ()
	       ; Prim.rewinddir d
	       ; if Error.getErrno () = 0 then () else Error.error ()
	    end

	 fun closedir (DS r) =
	    case !r of
	       NONE => ()
	     | SOME d => (checkResult (Prim.closedir d); r := NONE)
      end
	 
      fun chdir s =
	 checkResult (Prim.chdir (String.nullTerm s))

      local
	 val size: int ref = ref 1
	 fun make () = Array.array (!size, #"\000")
	 val buffer = ref (make ())
	    
	 fun extractToChar (a, c) =
	    let
	       val n = Array.length a
	       (* find the null terminator *)
	       fun loop i =
		  if i >= n
		     then raise Fail "String.extractFromC didn't find terminator"
		  else if c = Array.sub (a, i)
			  then i
		       else loop (i + 1)
	    in Array.extract (a, 0, SOME (loop 0))
	    end
	 
	 fun extract a = extractToChar (a, #"\000")
      in
	 fun getcwd () =
	    if Primitive.Cpointer.isNull (Prim.getcwd (!buffer, !size))
	       then (size := 2 * !size
		     ; buffer := make ()
		     ; getcwd ())
	    else Primitive.String.fromCharVector (extract (!buffer))
      end
	 
      val stdin = FD 0
      val stdout = FD 1
      val stderr = FD 2

      structure S =
	 struct
	    open S Flags
	 end

      structure O =
	 struct
	    open O Flags
	 end

      datatype open_mode = O_RDONLY | O_WRONLY | O_RDWR

      fun wordToOpenMode w =
	 if w = o_rdonly then O_RDONLY
	 else if w = o_wronly then O_WRONLY
	      else if w = o_rdwr then O_RDWR
		   else raise Fail "wordToOpenMode: unknown word"
		      
      val openModeToWord =
	 fn O_RDONLY => o_rdonly
	  | O_WRONLY => o_wronly
	  | O_RDWR => o_rdwr

      val error = PosixError.error

      fun createf (pathname, openMode, flags, mode) =
	 let
	    val fd =
	       Prim.openn (String.nullTerm pathname,
			  Flags.flags [openModeToWord openMode, flags, O.creat],
			  mode)
	 in if fd = ~1
	       then error ()
	    else FD fd
	 end

      fun openf (pathname, openMode, flags) =
	 let val fd = Prim.openn (String.nullTerm pathname,
				 Flags.flags [openModeToWord openMode, flags],
				 Flags.empty)
	 in if fd = ~1
	       then error ()
	    else FD fd
	 end
	 
      fun creat (s, m) = createf (s, O_WRONLY, O.trunc, m)

      val umask = Prim.umask

      local
	 fun wrap p arg = (checkResult (p arg); ())
	 fun wrapOldNew p =
	    wrap (fn {old,new} => p (String.nullTerm old, String.nullTerm new))
      in
	 val link = wrapOldNew Prim.link
	 val mkdir = wrap (fn (p, m) => Prim.mkdir (String.nullTerm p, m))
	 val mkfifo = wrap (fn (p, m) => Prim.mkfifo (String.nullTerm p, m))
	 val unlink = wrap (Prim.unlink o String.nullTerm)
	 val rmdir = wrap (Prim.rmdir o String.nullTerm)
	 val rename = wrapOldNew Prim.rename
	 val symlink = wrapOldNew Prim.symlink
	 val chmod = wrap (fn (p, m) => Prim.chmod (String.nullTerm p, m))
	 val fchmod = wrap (fn (FD n, m) => Prim.fchmod (n, m))
	 val chown = wrap (fn (s, u, g) => Prim.chown (String.nullTerm s, u, g))
	 val fchown = wrap (fn (FD n, u, g) => Prim.fchown (n, u, g))
	 val ftruncate = wrap (fn (FD n, pos) => Prim.ftruncate (n, pos))
      end	    

      local
	 val size: int = 1024
	 val buf = Word8Array.array (size, 0w0)
      in
	 fun readlink (path: string): string =
	    let val len = Prim.readlink (String.nullTerm path, buf, size)
	    in checkResult len
	       ; Byte.unpackString (buf, 0, SOME len)
	    end
      end

      type dev = Prim.dev
      val wordToDev = id
      val devToWord = id

      type ino = Prim.ino
      val wordToIno = SysWord.toInt
      val inoToWord = SysWord.fromInt

      structure ST =
	 struct
	    datatype stat =
	       T of {dev: dev,
		     ino: ino,
		     mode: S.mode,
		     nlink: int,
		     uid: uid,
		     gid: gid,
		     size: int,
		     atime: Time.time,
		     mtime: Time.time,
		     ctime: Time.time}

	    fun fromC (): stat =
	       T {dev = Stat.dev (),
		  ino = Stat.ino (),
		  mode = Stat.mode (),
		  nlink = Stat.nlink (),
		  uid = Stat.uid (),
		  gid = Stat.gid (),
		  size = Stat.size (),
		  atime = Time.fromSeconds (Stat.atime ()),
		  mtime = Time.fromSeconds (Stat.mtime ()),
		  ctime = Time.fromSeconds (Stat.ctime ())}

	    local
	       fun make sel (T r) = sel r
	    in
	       val mode = make #mode
	       val ino = make #ino
	       val dev = make #dev
	       val nlink = make #nlink
	       val uid = make #uid
	       val gid = make #gid
	       val size = make #size
	       val atime = make #atime
	       val mtime = make #mtime
	       val ctime = make #ctime
	    end

	    local
	       fun make prim s = prim (mode s)
	    in
	       val isDir = make Prim.ST.isDir
	       val isChr = make Prim.ST.isChr
	       val isBlk = make Prim.ST.isBlk
	       val isReg = make Prim.ST.isReg
	       val isFIFO = make Prim.ST.isFIFO
	       val isLink = make Prim.ST.isLink
	       val isSock = make Prim.ST.isSock
	    end
	 end

      local
	 fun make (prim, f) arg =
	    (checkResult (prim (f arg))
	     ; ST.fromC ())
      in
	 val stat = make (Prim.Stat.stat, String.nullTerm)
	 val lstat = make (Prim.Stat.lstat, String.nullTerm)
	 val fstat = make (Prim.Stat.fstat, fn FD fd => fd)
      end

      datatype access_mode = A_READ | A_WRITE | A_EXEC

      val conv_access_mode =
	 fn A_READ => R_OK
	  | A_WRITE => W_OK
	  | A_EXEC => X_OK

      fun access (path: string, mode: access_mode list): bool =
	 let val mode = Flags.flags (F_OK :: (map conv_access_mode mode))
	 in case Prim.access (String.nullTerm path, mode) of
	    ~1 => false
	  | _ => true
	 end

      local
	 structure U = Prim.Utimbuf
      in
	 fun utime (f: string, opt: {actime: Time.time,
				    modtime: Time.time} option): unit =
	    let
	       val (a, m) =
		  case opt of
		     NONE => let val t = Time.now ()
			     in (t, t)
			     end
		   | SOME {actime = a, modtime = m} => (a, m)
	       val a = Time.toSeconds a
	       val m = Time.toSeconds m
	    in U.setActime a
	       ; U.setModtime m
	       ; checkResult (U.utime (String.nullTerm f))
	    end
      end

      local
	 fun convertProperty s =
	    case List.find (fn (_, s') => s = s') properties of
	       NONE => Error.raiseSys Error.inval
	     | SOME (n, _) => n

	 fun make prim (f, s) =
	    let val n = prim (f, convertProperty s)
	    in if n < 0
		  then Error.error ()
	       else if n = 0
		       then NONE
		    else SOME (SysWord.fromInt n)
	    end
	       
      in
	 val pathconf = make (fn (path, s) =>
			      Prim.pathconf (String.nullTerm path, s))
	 val fpathconf = make (fn (FD n, s) => Prim.fpathconf (n, s))
      end
   end
