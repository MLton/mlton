signature POSIX_FILE_SYS =
   sig
      eqtype uid
      eqtype gid

      eqtype file_desc
      val fdToWord: file_desc -> SysWord.word
      val wordToFD: SysWord.word -> file_desc

      (* identity functions *)
      val fdToIOD: file_desc -> OS.IO.iodesc
      val iodToFD: OS.IO.iodesc -> file_desc option

      type dirstream
      val opendir: string -> dirstream
      val readdir: dirstream -> string option
      val rewinddir: dirstream -> unit
      val closedir: dirstream -> unit

      val chdir: string -> unit
      val getcwd: unit -> string

      val stdin: file_desc
      val stdout: file_desc
      val stderr: file_desc

      structure S: 
         sig
            eqtype mode
            include BIT_FLAGS where type flags = mode

            val irwxu: mode
            val irusr: mode
            val iwusr: mode
            val ixusr: mode
            val irwxg: mode
            val irgrp: mode
            val iwgrp: mode
            val ixgrp: mode
            val irwxo: mode
            val iroth: mode
            val iwoth: mode
            val ixoth: mode
            val isuid: mode
            val isgid: mode
         end

      structure O: 
         sig
           include BIT_FLAGS

           val append: flags
           val excl: flags
           val noctty: flags
           val nonblock: flags
           val sync: flags
           val trunc: flags
         end

      datatype open_mode = O_RDONLY | O_WRONLY | O_RDWR

      val openf: string * open_mode * O.flags -> file_desc
      val createf: string * open_mode * O.flags * S.mode -> file_desc
      val creat: string * S.mode -> file_desc
      val umask: S.mode -> S.mode
      val link: {old: string, new: string} -> unit
      val mkdir: string * S.mode -> unit
      val mkfifo: string * S.mode -> unit
      val unlink: string -> unit
      val rmdir: string -> unit
      val rename: {old: string, new: string} -> unit
      val symlink: {old: string, new: string} -> unit
      val readlink: string -> string

      eqtype dev
      val wordToDev: SysWord.word -> dev
      val devToWord: dev -> SysWord.word

      eqtype ino
      val wordToIno: SysWord.word -> ino
      val inoToWord: ino -> SysWord.word

      structure ST: 
         sig
            type stat

            val isDir: stat -> bool
            val isChr: stat -> bool
            val isBlk: stat -> bool
            val isReg: stat -> bool
            val isFIFO: stat -> bool
            val isLink: stat -> bool
            val isSock: stat -> bool
            val mode: stat -> S.mode
            val ino: stat -> ino
            val dev: stat -> dev
            val nlink: stat -> int
            val uid: stat -> uid
            val gid: stat -> gid
            val size: stat -> Position.int
            val atime: stat -> Time.time
            val mtime: stat -> Time.time
            val ctime: stat -> Time.time
         end

      val stat: string -> ST.stat
      val lstat: string -> ST.stat
      val fstat: file_desc -> ST.stat

      datatype access_mode = A_READ | A_WRITE | A_EXEC

      val access: string * access_mode list -> bool
      val chmod: string * S.mode -> unit
      val fchmod: file_desc * S.mode -> unit
      val chown: string * uid * gid -> unit
      val fchown: file_desc * uid * gid -> unit
      val utime: string * {actime: Time.time, modtime: Time.time} option -> unit
      val ftruncate: file_desc * Position.int -> unit
      val pathconf: string * string -> SysWord.word option
      val fpathconf: file_desc * string -> SysWord.word option
   end

signature POSIX_FILE_SYS_EXTRA =
   sig
      include POSIX_FILE_SYS

      val flagsToOpenMode: O.flags -> open_mode
   end
