signature POSIX_PROC_ENV =
   sig
      eqtype pid
      eqtype uid
      eqtype gid
      eqtype file_desc

      val uidToWord: uid -> SysWord.word
      val wordToUid: SysWord.word -> uid
      val gidToWord: gid -> SysWord.word
      val wordToGid: SysWord.word -> gid
      val getpid : unit -> pid
      val getppid: unit -> pid
      val getuid : unit -> uid
      val geteuid: unit -> uid
      val getgid : unit -> gid
      val getegid: unit -> gid
      val setuid: uid -> unit
      val setgid: gid -> unit
      val getgroups: unit -> gid list
      val getlogin: unit -> string
      val getpgrp: unit -> pid
      val setsid: unit -> pid
      val setpgid: {pid: pid option, pgid: pid option} -> unit
      val uname: unit -> (string * string) list
      val time: unit -> Time.time
      val times: unit -> {elapsed: Time.time,
                          utime: Time.time,
                          stime: Time.time,
                          cutime: Time.time,
                          cstime: Time.time}
      val getenv: string -> string option
      val environ: unit -> string list
      val ctermid: unit -> string
      val ttyname: file_desc -> string
      val isatty: file_desc -> bool
      val sysconf: string -> SysWord.word
   end
