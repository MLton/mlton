signature POSIX_PROC_ENV =
   sig
      eqtype file_desc
      eqtype gid
      eqtype pid
      eqtype uid

      val ctermid: unit -> string
      val environ: unit -> string list 
      val getegid: unit -> gid 
      val getenv: string -> string option 
      val geteuid: unit -> uid 
      val getgid: unit -> gid 
      val getgroups: unit -> gid list
      val getlogin: unit -> string
      val getpgrp: unit -> pid 
      val getpid: unit -> pid 
      val getppid: unit -> pid 
      val getuid: unit -> uid 
      val gidToWord: gid -> SysWord.word 
      val isatty: file_desc -> bool 
      val setgid: gid -> unit
      val setpgid: {pid: pid option, pgid: pid option} -> unit
      val setsid: unit -> pid
      val setuid: uid -> unit
      val sysconf: string -> SysWord.word
      val time: unit -> Time.time
      val times: unit -> {elapsed: Time.time,
			  utime: Time.time,
			  stime: Time.time,
			  cutime: Time.time,
			  cstime: Time.time}
      val ttyname: file_desc -> string
      val uidToWord: uid -> SysWord.word 
      val uname: unit -> (string * string) list
      val wordToGid: SysWord.word -> gid 
      val wordToUid: SysWord.word -> uid 
   end
