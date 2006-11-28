signature POSIX_IO_1997 =
   sig
      eqtype file_desc
      eqtype pid

      val pipe: unit -> {infd: file_desc, outfd: file_desc} 
      val dup: file_desc -> file_desc 
      val dup2: {old: file_desc, new: file_desc} -> unit

      val close: file_desc -> unit 
      val readVec: file_desc * int -> Word8Vector.vector 
      val readArr: file_desc * {buf: Word8Array.array,
                                i: int,
                                sz: int option} -> int 
      val writeVec: file_desc * {buf: Word8Vector.vector,
                                 i: int,
                                 sz: int option} -> int 
      val writeArr: file_desc * {buf: Word8Array.array,
                                 i: int,
                                 sz: int option} -> int 

      datatype whence = SEEK_SET | SEEK_CUR | SEEK_END

      structure FD:
         sig
            include POSIX_FLAGS_1997

            val cloexec: flags 
         end

      structure O:
         sig
            include POSIX_FLAGS_1997

            val append: flags 
            val nonblock: flags 
            val sync: flags 
         end

      datatype open_mode = O_RDONLY | O_WRONLY | O_RDWR

      val dupfd: {old: file_desc, base: file_desc} -> file_desc 
      val getfd: file_desc -> FD.flags 
      val setfd: file_desc * FD.flags -> unit 
      val getfl: file_desc -> O.flags * open_mode
      val setfl: file_desc * O.flags -> unit 
      val lseek: file_desc * Position.int * whence -> Position.int 
      val fsync: file_desc -> unit

      datatype lock_type =
         F_RDLCK
       | F_WRLCK
       | F_UNLCK

      structure FLock:
         sig
            type flock
            val flock: {
                        ltype: lock_type,
                        whence: whence,
                        start: Position.int,
                        len: Position.int,
                        pid: pid option
                        } -> flock 
            val ltype: flock -> lock_type 
            val whence: flock -> whence 
            val start: flock -> Position.int 
            val len: flock -> Position.int 
            val pid: flock -> pid option 
         end

      val getlk: file_desc * FLock.flock -> FLock.flock 
      val setlk: file_desc * FLock.flock -> FLock.flock 
      val setlkw: file_desc * FLock.flock -> FLock.flock
   end
