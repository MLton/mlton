(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure PosixPrimitive =
   struct
      type nullString = string
      type cstring = Primitive.C.CS.cs
      type cstringArray = Primitive.C.CSS.css

      type fd = int
      type pid = int
      type uid = word
      type gid = word
      type signal = int
      type size = int
      type ssize = int
      type mode = word
      type time = int
      datatype file_desc = FD of int
	 
      structure Error =
	 struct
	    type syserror = int

	    val getErrno = _ffi "Posix_Error_getErrno": unit -> int;
	    val clearErrno = _ffi "Posix_Error_clearErrno": unit -> unit;
	    val strerror = _ffi "Posix_Error_strerror": syserror -> cstring;

	    val acces = _const "Posix_Error_acces": syserror;
	    val again = _const "Posix_Error_again": syserror;
	    val badf = _const "Posix_Error_badf": syserror;
	    val badmsg = _const "Posix_Error_badmsg": syserror;
	    val busy = _const "Posix_Error_busy": syserror;
	    val canceled = _const "Posix_Error_canceled": syserror;
	    val child = _const "Posix_Error_child": syserror;
	    val deadlk = _const "Posix_Error_deadlk": syserror;
	    val dom = _const "Posix_Error_dom": syserror;
	    val exist = _const "Posix_Error_exist": syserror;
	    val fault = _const "Posix_Error_fault": syserror;
	    val fbig = _const "Posix_Error_fbig": syserror;
	    val inprogress = _const "Posix_Error_inprogress": syserror;
	    val intr = _const "Posix_Error_intr": syserror;
	    val inval = _const "Posix_Error_inval": syserror;
	    val io = _const "Posix_Error_io": syserror;
	    val isdir = _const "Posix_Error_isdir": syserror;
	    val loop = _const "Posix_Error_loop": syserror;
	    val mfile = _const "Posix_Error_mfile": syserror;
	    val mlink = _const "Posix_Error_mlink": syserror;
	    val msgsize = _const "Posix_Error_msgsize": syserror;
	    val nametoolong = _const "Posix_Error_nametoolong": syserror;
	    val nfile = _const "Posix_Error_nfile": syserror;
	    val nodev = _const "Posix_Error_nodev": syserror;
	    val noent = _const "Posix_Error_noent": syserror;
	    val noexec = _const "Posix_Error_noexec": syserror;
	    val nolck = _const "Posix_Error_nolck": syserror;
	    val nomem = _const "Posix_Error_nomem": syserror;
	    val nospc = _const "Posix_Error_nospc": syserror;
	    val nosys = _const "Posix_Error_nosys": syserror;
	    val notdir = _const "Posix_Error_notdir": syserror;
	    val notempty = _const "Posix_Error_notempty": syserror;
	    val notsup = _const "Posix_Error_notsup": syserror;
	    val notty = _const "Posix_Error_notty": syserror;
	    val nxio = _const "Posix_Error_nxio": syserror;
	    val perm = _const "Posix_Error_perm": syserror;
	    val pipe = _const "Posix_Error_pipe": syserror;
	    val range = _const "Posix_Error_range": syserror;
	    val rofs = _const "Posix_Error_rofs": syserror;
	    val spipe = _const "Posix_Error_spipe": syserror;
	    val srch = _const "Posix_Error_srch": syserror;
	    val toobig = _const "Posix_Error_toobig": syserror;
	    val xdev = _const "Posix_Error_xdev": syserror;

	    val errorNames =
	       [
		(acces, "acces"),
		(again, "again"),
		(badf, "badf"),
		(badmsg, "badmsg"),
		(busy, "busy"),
		(canceled, "canceled"),
		(child, "child"),
		(deadlk, "deadlk"),
		(dom, "dom"),
		(exist, "exist"),
		(fault, "fault"),
		(fbig, "fbig"),
		(inprogress, "inprogress"),
		(intr, "intr"),
		(inval, "inval"),
		(io, "io"),
		(isdir, "isdir"),
		(loop, "loop"),
		(mfile, "mfile"),
		(mlink, "mlink"),
		(msgsize, "msgsize"),
		(nametoolong, "nametoolong"),
		(nfile, "nfile"),
		(nodev, "nodev"),
		(noent, "noent"),
		(noexec, "noexec"),
		(nolck, "nolck"),
		(nomem, "nomem"),
		(nospc, "nospc"),
		(nosys, "nosys"),
		(notdir, "notdir"),
		(notempty, "notempty"),
		(notsup, "notsup"),
		(notty, "notty"),
		(nxio, "nxio"),
		(perm, "perm"),
		(pipe, "pipe"),
		(range, "range"),
		(rofs, "rofs"),
		(spipe, "spipe"),
		(srch, "srch"),
		(toobig, "toobig"),
		(xdev, "xdev")
		]
	 end
      
      structure Signal =
	 struct
	    type signal = signal
      	    type how = int

	    val abrt = _const "Posix_Signal_abrt": signal;
	    val alrm = _const "Posix_Signal_alrm": signal;
	    val bus = _const "Posix_Signal_bus": signal;
	    val chld = _const "Posix_Signal_chld": signal;
	    val cont = _const "Posix_Signal_cont": signal;
	    val fpe = _const "Posix_Signal_fpe": signal;
	    val hup = _const "Posix_Signal_hup": signal;
	    val ill = _const "Posix_Signal_ill": signal;
	    val int = _const "Posix_Signal_int": signal;
	    val kill = _const "Posix_Signal_kill": signal;
	    val pipe = _const "Posix_Signal_pipe": signal;
	    val prof = _const "Posix_Signal_prof": signal;
	    val quit = _const "Posix_Signal_quit": signal;
	    val segv = _const "Posix_Signal_segv": signal;
	    val stop = _const "Posix_Signal_stop": signal;
	    val term = _const "Posix_Signal_term": signal;
	    val tstp = _const "Posix_Signal_tstp": signal;
	    val ttin = _const "Posix_Signal_ttin": signal;
	    val ttou = _const "Posix_Signal_ttou": signal;
	    val usr1 = _const "Posix_Signal_usr1": signal;
	    val usr2 = _const "Posix_Signal_usr2": signal;
	    val vtalrm = _const "Posix_Signal_vtalrm": signal;

	    val block = _const "Posix_Signal_block": how;
	    val default = _ffi "Posix_Signal_default": signal -> int;
	    val handlee = _ffi "Posix_Signal_handle": signal -> int;
	    val ignore = _ffi "Posix_Signal_ignore": signal -> int;
	    val isDefault =
	       _ffi "Posix_Signal_isDefault": signal * bool ref -> int;
	    val isPending = _ffi "Posix_Signal_isPending": signal -> bool;
	    val numSignals = _const "Posix_Signal_numSignals": int;
	    val setmask = _const "Posix_Signal_setmask": how;
	    val sigaddset = _ffi "Posix_Signal_sigaddset": signal -> int;
	    val sigdelset = _ffi "Posix_Signal_sigdelset": signal -> int;
	    val sigemptyset = _ffi "Posix_Signal_sigemptyset": unit -> int;
	    val sigfillset = _ffi "Posix_Signal_sigfillset": unit -> int;
	    val sigprocmask = _ffi "Posix_Signal_sigprocmask": how -> int;
	    val suspend = _ffi "Posix_Signal_suspend": unit -> int;
	    val unblock = _const "Posix_Signal_unblock": how;
	 end
      
      structure Process =
	 struct
	    val wnohang = _const "Posix_Process_wnohang": word;
	    structure W =
	       struct
		  type flags = word
		  val untraced = _const "Posix_Process_W_untraced": flags;
	       end
	    
	    type pid = pid
	    type status = int

	    val alarm = _ffi "Posix_Process_alarm": int -> int;
	    val exece =
	       _ffi "Posix_Process_exece"
	       : nullString * nullString array * nullString array -> int;
	    val execp =
	       _ffi "Posix_Process_execp": nullString * nullString array -> int;
	    val exit = _ffi "Posix_Process_exit": int -> unit;
	    val exitStatus = _ffi "Posix_Process_exitStatus": status -> int;
	    val fork = _ffi "Posix_Process_fork": unit -> pid;
	    val ifExited = _ffi "Posix_Process_ifExited": status -> bool;
	    val ifSignaled = _ffi "Posix_Process_ifSignaled": status -> bool;
	    val ifStopped = _ffi "Posix_Process_ifStopped": status -> bool;
	    val kill = _ffi "Posix_Process_kill": pid * signal -> int;
	    val pause = _ffi "Posix_Process_pause": unit -> int;
	    val sleep = _ffi "Posix_Process_sleep": int -> int;
	    val stopSig = _ffi "Posix_Process_stopSig": status -> signal;
	    val termSig = _ffi "Posix_Process_termSig": status -> signal;
	    val waitpid =
	       _ffi "Posix_Process_waitpid": pid * status ref * int -> pid;
	 end

      structure ProcEnv =
	 struct
	    val numgroups = _const "Posix_ProcEnv_numgroups": int;
	    val sysconfNames =
	       [
		(* Required *)
		(_const "Posix_ProcEnv_ARG_MAX": int;, "ARG_MAX"),
		(_const "Posix_ProcEnv_CHILD_MAX": int;, "CHILD_MAX"),
		(_const "Posix_ProcEnv_CLK_TCK": int;, "CLK_TCK"),
		(_const "Posix_ProcEnv_NGROUPS_MAX": int;, "NGROUPS_MAX"),
		(_const "Posix_ProcEnv_OPEN_MAX": int;, "OPEN_MAX"),
		(_const "Posix_ProcEnv_STREAM_MAX": int;, "STREAM_MAX"),
		(_const "Posix_ProcEnv_TZNAME_MAX": int;, "TZNAME_MAX"),
		(_const "Posix_ProcEnv_JOB_CONTROL": int;, "JOB_CONTROL"),
		(_const "Posix_ProcEnv_SAVED_IDS": int;, "SAVED_IDS"),
		(_const "Posix_ProcEnv_VERSION": int;, "VERSION"),
		(* Optional *)
		(_const "Posix_ProcEnv_BC_BASE_MAX": int;, "BC_BASE_MAX"),
		(_const "Posix_ProcEnv_BC_DIM_MAX": int;, "BC_DIM_MAX"),
		(_const "Posix_ProcEnv_BC_SCALE_MAX": int;, "BC_SCALE_MAX"),
		(_const "Posix_ProcEnv_BC_STRING_MAX": int;, "BC_STRING_MAX"),
		(_const "Posix_ProcEnv_COLL_WEIGHTS_MAX": int;,
		 "COLL_WEIGHTS_MAX"),
		(_const "Posix_ProcEnv_EXPR_NEST_MAX": int;, "EXPR_NEST_MAX"),
		(_const "Posix_ProcEnv_LINE_MAX": int;, "LINE_MAX"),
		(_const "Posix_ProcEnv_RE_DUP_MAX": int;, "RE_DUP_MAX"),
		(_const "Posix_ProcEnv_2_VERSION": int;, "2_VERSION"),
		(_const "Posix_ProcEnv_2_FORT_DEV": int;, "2_FORT_DEV"),
		(_const "Posix_ProcEnv_2_FORT_RUN": int;, "2_FORT_RUN"),
		(_const "Posix_ProcEnv_2_SW_DEV": int;, "2_SW_DEV")
		]
	       
	    type pid = pid
	    type gid = gid
	    type uid = uid
	    datatype file_desc = datatype file_desc

	    val getegid = _ffi "Posix_ProcEnv_getegid": unit -> gid;
	    val geteuid = _ffi "Posix_ProcEnv_geteuid": unit -> uid;
	    val getgid = _ffi "Posix_ProcEnv_getgid": unit -> gid;
	    val getgroups = _ffi "Posix_ProcEnv_getgroups": gid array -> int;
	    val getlogin = _ffi "Posix_ProcEnv_getlogin": unit -> cstring;
	    val getpgrp = _ffi "Posix_ProcEnv_getpgrp": unit -> pid;
	    val getpid = _ffi "Posix_ProcEnv_getpid": unit -> pid;
	    val getppid = _ffi "Posix_ProcEnv_getppid": unit -> pid;
	    val getuid = _ffi "Posix_ProcEnv_getuid": unit -> uid;
	    val setenv =
	       _ffi "Posix_ProcEnv_setenv": nullString * nullString -> int;
	    val setgid = _ffi "Posix_ProcEnv_setgid": gid -> int;
	    val setpgid = _ffi "Posix_ProcEnv_setpgid": pid * pid -> int;
	    val setsid = _ffi "Posix_ProcEnv_setsid": unit -> pid;
	    val setuid = _ffi "Posix_ProcEnv_setuid": uid -> int;

	    structure Uname =
	       struct
		  type uname = pointer

		  val uname = _ffi "Posix_ProcEnv_Uname_uname": unit -> int;
		  val sysname =
		     _ffi "Posix_ProcEnv_Uname_sysname": unit -> cstring;
		  val nodename =
		     _ffi "Posix_ProcEnv_Uname_nodename": unit -> cstring;
		  val release =
		     _ffi "Posix_ProcEnv_Uname_release": unit -> cstring;
		  val version =
		     _ffi "Posix_ProcEnv_Uname_version": unit -> cstring;
		  val machine =
		     _ffi "Posix_ProcEnv_Uname_machine": unit -> cstring;
	       end

	    type clock_t = word
	       
	    structure Tms =
	       struct
		  val utime = _ffi "Posix_ProcEnv_Tms_utime": unit -> clock_t;
		  val stime = _ffi "Posix_ProcEnv_Tms_stime": unit -> clock_t;
		  val cutime = _ffi "Posix_ProcEnv_Tms_cutime": unit -> clock_t;
		  val cstime = _ffi "Posix_ProcEnv_Tms_cstime": unit -> clock_t;
	       end

	    val ctermid = _ffi "Posix_ProcEnv_ctermid" : unit -> cstring;
	    val environ = _ffi "Posix_ProcEnv_environ" : cstringArray;
	    val getenv = _ffi "Posix_ProcEnv_getenv" : nullString -> cstring;
	    val isatty = _ffi "Posix_ProcEnv_isatty" : fd -> bool;
	    val sysconf = _ffi "Posix_ProcEnv_sysconf" : int -> int;
	    val times = _ffi "Posix_ProcEnv_times" : unit -> clock_t;
	    val ttyname = _ffi "Posix_ProcEnv_ttyname" : fd -> cstring;
	 end 
      
      structure FileSys =
	 struct
	    datatype file_desc = datatype file_desc

	    type ino = int
	    type dev = word
	    type uid = uid
	    type gid = gid

	    structure S =
	       struct
		  type mode = word
		  val ifsock = _const "Posix_FileSys_S_ifsock": mode;
		  val iflnk = _const "Posix_FileSys_S_iflnk": mode;
		  val ifreg = _const "Posix_FileSys_S_ifreg": mode;
		  val ifblk = _const "Posix_FileSys_S_ifblk": mode;
		  val ifdir = _const "Posix_FileSys_S_ifdir": mode;
		  val ifchr = _const "Posix_FileSys_S_ifchr": mode;
		  val ififo = _const "Posix_FileSys_S_ififo": mode;
		  val irwxu = _const "Posix_FileSys_S_irwxu": mode;
		  val irusr = _const "Posix_FileSys_S_irusr": mode;
		  val iwusr = _const "Posix_FileSys_S_iwusr": mode;
		  val ixusr = _const "Posix_FileSys_S_ixusr": mode;
		  val irwxg = _const "Posix_FileSys_S_irwxg": mode;
		  val irgrp = _const "Posix_FileSys_S_irgrp": mode;
		  val iwgrp = _const "Posix_FileSys_S_iwgrp": mode;
		  val ixgrp = _const "Posix_FileSys_S_ixgrp": mode;
		  val irwxo = _const "Posix_FileSys_S_irwxo": mode;
		  val iroth = _const "Posix_FileSys_S_iroth": mode;
		  val iwoth = _const "Posix_FileSys_S_iwoth": mode;
		  val ixoth = _const "Posix_FileSys_S_ixoth": mode;
		  val isuid = _const "Posix_FileSys_S_isuid": mode;
		  val isgid = _const "Posix_FileSys_S_isgid": mode;
	       end

	    structure O =
	       struct
		  type flags = word
		  val append = _const "Posix_FileSys_O_append": flags;
		  val creat = _const "Posix_FileSys_O_creat": flags;
		  val excl = _const "Posix_FileSys_O_excl": flags;
		  val noctty = _const "Posix_FileSys_O_noctty": flags;
		  val nonblock = _const "Posix_FileSys_O_nonblock": flags;
		  val sync = _const "Posix_FileSys_O_sync": flags;
		  val trunc = _const "Posix_FileSys_O_trunc": flags;
                  val text = _const "Posix_FileSys_O_text": flags;
                  val binary = _const "Posix_FileSys_O_binary": flags;
	       end

	    val o_rdonly = _const "Posix_FileSys_o_rdonly": word;
	    val o_wronly = _const "Posix_FileSys_o_wronly": word;
	    val o_rdwr = _const "Posix_FileSys_o_rdwr": word;
	    val R_OK = _const "Posix_FileSys_R_OK": word;
	    val W_OK = _const "Posix_FileSys_W_OK": word;
	    val X_OK = _const "Posix_FileSys_X_OK": word;
	    val F_OK = _const "Posix_FileSys_F_OK": word;

	    val properties =
	       [
		(_const "Posix_FileSys_CHOWN_RESTRICTED": int;,
		 "CHOWN_RESTRICTED"),
		(_const "Posix_FileSys_LINK_MAX": int;, "LINK_MAX"),
		(_const "Posix_FileSys_MAX_CANON": int;, "MAX_CANON"),
		(_const "Posix_FileSys_MAX_INPUT": int;, "MAX_INPUT"),
		(_const "Posix_FileSys_NAME_MAX": int;, "NAME_MAX"),
		(_const "Posix_FileSys_NO_TRUNC": int;, "NO_TRUNC"),
		(_const "Posix_FileSys_PATH_MAX": int;, "PATH_MAX"),
		(_const "Posix_FileSys_PIPE_BUF": int;, "PIPE_BUF"),
		(_const "Posix_FileSys_VDISABLE": int;, "VDISABLE"),
		(_const "Posix_FileSys_ASYNC_IO": int;, "ASYNC_IO"), 
		(_const "Posix_FileSys_SYNC_IO": int;, "SYNC_IO"), 
		(_const "Posix_FileSys_PRIO_IO": int;, "PRIO_IO")
		]

	    structure Dirstream =
	       struct
		  type dirstream = pointer

		  val closedir =
		     _ffi "Posix_FileSys_Dirstream_closedir": dirstream -> int;
		  val opendir =
		     _ffi "Posix_FileSys_Dirstream_opendir"
		     : nullString -> dirstream;
		  val readdir =
		     _ffi "Posix_FileSys_Dirstream_readdir"
		     : dirstream -> cstring;
		  val rewinddir =
		     _ffi "Posix_FileSys_Dirstream_rewinddir"
		     : dirstream -> unit;
	       end

	    structure Stat =
	       struct
		  val dev = _ffi "Posix_FileSys_Stat_dev": unit -> dev;
		  val ino = _ffi "Posix_FileSys_Stat_ino": unit -> ino;
		  val mode = _ffi "Posix_FileSys_Stat_mode": unit -> word;
		  val nlink = _ffi "Posix_FileSys_Stat_nlink": unit -> int;
		  val uid = _ffi "Posix_FileSys_Stat_uid": unit -> uid;
		  val gid = _ffi "Posix_FileSys_Stat_gid": unit -> gid;
		  val size = _ffi "Posix_FileSys_Stat_size": unit -> int;
		  val atime =
		     _ffi "Posix_FileSys_Stat_atime": unit -> time;
		  val mtime =
		     _ffi "Posix_FileSys_Stat_mtime": unit -> time;
		  val ctime =
		     _ffi "Posix_FileSys_Stat_ctime": unit -> time;
		  val fstat = _ffi "Posix_FileSys_Stat_fstat": fd -> int;
		  val lstat =
		     _ffi "Posix_FileSys_Stat_lstat": nullString -> int;
		  val stat =
		     _ffi "Posix_FileSys_Stat_stat": nullString -> int;
	       end

	    structure Utimbuf =
	       struct
		  val setActime =
		     _ffi "Posix_FileSys_Utimbuf_setActime": time -> unit;
		  val setModtime =
		     _ffi "Posix_FileSys_Utimbuf_setModTime": time -> unit;
		  val utime =
		     _ffi "Posix_FileSys_Utimbuf_utime": nullString -> int;
	       end

	    val access =
	       _ffi "Posix_FileSys_access": nullString * word -> int;
	    val chdir = _ffi "Posix_FileSys_chdir": nullString -> int;
	    val chmod =
	       _ffi "Posix_FileSys_chmod": nullString * mode -> int;
	    val chown =
	       _ffi "Posix_FileSys_chown": nullString * uid * gid -> int;
	    val fchmod =
	       _ffi "Posix_FileSys_fchmod": fd * mode -> int;
	    val fchown =
	       _ffi "Posix_FileSys_fchown": fd * uid * gid -> int;
	    val fpathconf =
	       _ffi "Posix_FileSys_fpathconf": fd * int -> int;
	    val ftruncate =
	       _ffi "Posix_FileSys_ftruncate": fd * int -> int;
	    val getcwd =
	       _ffi "Posix_FileSys_getcwd": char array * size -> cstring;
	    val link =
	       _ffi "Posix_FileSys_link": nullString * nullString -> int;
	    val mkdir =
	       _ffi "Posix_FileSys_mkdir": nullString * word -> int;
	    val mkfifo =
	       _ffi "Posix_FileSys_mkfifo": nullString * word -> int;
	    val openn =
	       _ffi "Posix_FileSys_open": nullString * word * mode -> fd;
	    val pathconf =
	       _ffi "Posix_FileSys_pathconf": nullString * int -> int;
	    val readlink =
	       _ffi "Posix_FileSys_readlink"
	       : nullString * word8 array * int -> int;
	    val rename =
	       _ffi "Posix_FileSys_rename": nullString * nullString -> int;
	    val rmdir = _ffi "Posix_FileSys_rmdir": nullString -> int;
	    val symlink =
	       _ffi "Posix_FileSys_symlink"
	       : nullString * nullString -> int;
	    val umask = _ffi "Posix_FileSys_umask": word -> word;
	    val unlink = _ffi "Posix_FileSys_unlink": nullString -> int;

	    structure ST =
	       struct
		  val isDir = _ffi "Posix_FileSys_ST_isDir": word -> bool;
		  val isChr = _ffi "Posix_FileSys_ST_isChr": word -> bool;
		  val isBlk = _ffi "Posix_FileSys_ST_isBlk": word -> bool;
		  val isReg = _ffi "Posix_FileSys_ST_isReg": word -> bool;
		  val isFIFO =
		     _ffi "Posix_FileSys_ST_isFIFO": word -> bool;
		  val isLink =
		     _ffi "Posix_FileSys_ST_isLink": word -> bool;
		  val isSock =
		     _ffi "Posix_FileSys_ST_isSock": word -> bool;
	       end
	 end

      structure IO =
	 struct
	    val F_DUPFD = _const "Posix_IO_F_DUPFD": int;
	    val F_GETFD = _const "Posix_IO_F_GETFD": int;
	    val F_SETFD = _const "Posix_IO_F_SETFD": int;
	    val F_GETFL = _const "Posix_IO_F_GETFL": int;
	    val F_SETFL = _const "Posix_IO_F_SETFL": int;
	    val F_GETLK = _const "Posix_IO_F_GETLK": int;
	    val F_SETLK = _const "Posix_IO_F_SETLK": int;
	    val F_RDLCK = _const "Posix_IO_F_RDLCK": int;
	    val F_WRLCK = _const "Posix_IO_F_WRLCK": int;
	    val F_UNLCK = _const "Posix_IO_F_UNLCK": int;
	    val F_SETLKW = _const "Posix_IO_F_SETLKW": int;
	    val F_GETOWN = _const "Posix_IO_F_GETOWN": int;
	    val F_SETOWN = _const "Posix_IO_F_SETOWN": int;
	    val O_ACCMODE = _const "Posix_IO_O_ACCMODE": word;
	    val SEEK_SET = _const "Posix_IO_SEEK_SET": int;
	    val SEEK_CUR = _const "Posix_IO_SEEK_CUR": int;
	    val SEEK_END = _const "Posix_IO_SEEK_END": int;

	    structure FD =
	       struct
		  type flags = word
		  val cloexec = _const "Posix_IO_FD_cloexec": flags;
	       end
	    
	    datatype file_desc = datatype file_desc
	    type pid = pid

	    structure FLock =
	       struct
		  val fcntl = _ffi "Posix_IO_FLock_fcntl": fd * int -> int;
		  val typ = _ffi "Posix_IO_FLock_typ": unit -> int;
		  val whence = _ffi "Posix_IO_FLock_whence": unit -> int;
		  val start = _ffi "Posix_IO_FLock_start": unit -> int;
		  val len = _ffi "Posix_IO_FLock_len": unit -> int;
		  val pid = _ffi "Posix_IO_FLock_pid": unit -> int;
		  val setType = _ffi "Posix_IO_FLock_setType": int -> unit;
		  val setWhence =
		     _ffi "Posix_IO_FLock_setWhence": int -> unit;
		  val setStart =
		     _ffi "Posix_IO_FLock_setStart": int -> unit;
		  val setLen = _ffi "Posix_IO_FLock_setLen": int -> unit;
		  val setPid = _ffi "Posix_IO_FLock_setPid": int -> unit;
	       end
	    
	    val close = _ffi "Posix_IO_close": fd -> int;
	    val dup = _ffi "Posix_IO_dup": fd -> fd;
	    val dup2 = _ffi "Posix_IO_dup2": fd * fd -> fd;
	    val fcntl2 = _ffi "Posix_IO_fcntl2": fd * int -> int;
	    val fcntl3 = _ffi "Posix_IO_fcntl3": fd * int * int -> int;
	    val fsync = _ffi "Posix_IO_fsync": fd -> int;
	    val lseek = _ffi "Posix_IO_lseek": fd * int * int -> int;
	    val pipe = _ffi "Posix_IO_pipe": fd array -> int;
	    val readChar = _ffi "Posix_IO_read":
	       fd * char array * int * size -> ssize;
	    val writeChar = _ffi "Posix_IO_write":
	       fd * char vector * int * size -> ssize;
	    val readWord8 = _ffi "Posix_IO_read":
	       fd * word8 array * int * size -> ssize;
	    val writeWord8 = _ffi "Posix_IO_write":
	       fd * word8 vector * int * size -> ssize;
	 end	       

      structure SysDB =
	 struct
	    type gid = gid
	    type uid = uid

	    structure Passwd =
	       struct
		  val name = _ffi "Posix_SysDB_Passwd_name": unit -> cstring;
		  val uid = _ffi "Posix_SysDB_Passwd_uid": unit -> uid;
		  val gid = _ffi "Posix_SysDB_Passwd_gid": unit -> gid;
		  val dir = _ffi "Posix_SysDB_Passwd_dir": unit -> cstring;
		  val shell = _ffi "Posix_SysDB_Passwd_shell": unit -> cstring;
	       end

	    val getpwnam = _ffi "Posix_SysDB_getpwnam": nullString -> bool;
	    val getpwuid = _ffi "Posix_SysDB_getpwuid": uid -> bool;

	    structure Group =
	       struct
		  val name = _ffi "Posix_SysDB_Group_name": unit -> cstring;
		  val gid = _ffi "Posix_SysDB_Group_gid": unit -> gid;
		  val mem = _ffi "Posix_SysDB_Group_mem": unit -> cstringArray;
	       end

	    val getgrgid = _ffi "Posix_SysDB_getgrgid": gid -> bool;
	    val getgrnam = _ffi "Posix_SysDB_getgrnam": nullString -> bool;
	 end

      structure TTY =
	 struct
	    type speed = word
	    val b0 = _const "Posix_TTY_b0": speed;
	    val b110 = _const "Posix_TTY_b110": speed;
	    val b1200 = _const "Posix_TTY_b1200": speed;
	    val b134 = _const "Posix_TTY_b134": speed;
	    val b150 = _const "Posix_TTY_b150": speed;
	    val b1800 = _const "Posix_TTY_b1800": speed;
	    val b19200 = _const "Posix_TTY_b19200": speed;
	    val b200 = _const "Posix_TTY_b200": speed;
	    val b2400 = _const "Posix_TTY_b2400": speed;
	    val b300 = _const "Posix_TTY_b300": speed;
	    val b38400 = _const "Posix_TTY_b38400": speed;
	    val b4800 = _const "Posix_TTY_b4800": speed;
	    val b50 = _const "Posix_TTY_b50": speed;
	    val b600 = _const "Posix_TTY_b600": speed;
	    val b75 = _const "Posix_TTY_b75": speed;
	    val b9600 = _const "Posix_TTY_b9600": speed;
	       
	    type pid = pid
	    datatype file_desc = datatype file_desc

	    structure V =
	       struct
		  val eof = _const "Posix_TTY_V_eof": int;
		  val eol = _const "Posix_TTY_V_eol": int;
		  val erase = _const "Posix_TTY_V_erase": int;
		  val intr = _const "Posix_TTY_V_intr": int;
		  val kill = _const "Posix_TTY_V_kill": int;
		  val min = _const "Posix_TTY_V_min": int;
		  val nccs = _const "Posix_TTY_V_nccs": int;
		  val quit = _const "Posix_TTY_V_quit": int;
		  val start = _const "Posix_TTY_V_start": int;
		  val stop = _const "Posix_TTY_V_stop": int;
		  val susp = _const "Posix_TTY_V_susp": int;
		  val time = _const "Posix_TTY_V_time": int;
	       end

	    structure I =
	       struct
		  type flags = word
		  val brkint = _const "Posix_TTY_I_brkint": flags;
		  val icrnl = _const "Posix_TTY_I_icrnl": flags;
		  val ignbrk = _const "Posix_TTY_I_ignbrk": flags;
		  val igncr = _const "Posix_TTY_I_igncr": flags;
		  val ignpar = _const "Posix_TTY_I_ignpar": flags;
		  val inlcr = _const "Posix_TTY_I_inlcr": flags;
		  val inpck = _const "Posix_TTY_I_inpck": flags;
		  val istrip = _const "Posix_TTY_I_istrip": flags;
		  val ixoff = _const "Posix_TTY_I_ixoff": flags;
		  val ixon = _const "Posix_TTY_I_ixon": flags;
		  val parmrk = _const "Posix_TTY_I_parmrk": flags;
	       end

	    structure O =
	       struct
		  type flags = word
		  val opost = _const "Posix_TTY_O_opost": flags;
	       end

	    structure C =
	       struct
		  type flags = word
		  val clocal = _const "Posix_TTY_C_clocal": flags;
		  val cread = _const "Posix_TTY_C_cread": flags;
		  val cs5 = _const "Posix_TTY_C_cs5": flags;
		  val cs6 = _const "Posix_TTY_C_cs6": flags;
		  val cs7 = _const "Posix_TTY_C_cs7": flags;
		  val cs8 = _const "Posix_TTY_C_cs8": flags;
		  val csize = _const "Posix_TTY_C_csize": flags;
		  val cstopb = _const "Posix_TTY_C_cstopb": flags;
		  val hupcl = _const "Posix_TTY_C_hupcl": flags;
		  val parenb = _const "Posix_TTY_C_parenb": flags;
		  val parodd = _const "Posix_TTY_C_parodd": flags;
	       end

	    structure L =
	       struct
		  type flags = word
		  val echo = _const "Posix_TTY_L_echo": flags;
		  val echoe = _const "Posix_TTY_L_echoe": flags;
		  val echok = _const "Posix_TTY_L_echok": flags;
		  val echonl = _const "Posix_TTY_L_echonl": flags;
		  val icanon = _const "Posix_TTY_L_icanon": flags;
		  val iexten = _const "Posix_TTY_L_iexten": flags;
		  val isig = _const "Posix_TTY_L_isig": flags;
		  val noflsh = _const "Posix_TTY_L_noflsh": flags;
		  val tostop = _const "Posix_TTY_L_tostop": flags;
	       end

	    structure TC =
	       struct
		  type set_action = int

		  val sadrain = _const "Posix_TTY_TC_sadrain": set_action;
		  val saflush = _const "Posix_TTY_TC_saflush": set_action;
		  val sanow = _const "Posix_TTY_TC_sanow": set_action;

		  type flow_action = int

		  val ion = _const "Posix_TTY_TC_ion": flow_action;
		  val ioff = _const "Posix_TTY_TC_ioff": flow_action;
		  val ooff = _const "Posix_TTY_TC_ooff": flow_action;
		  val oon = _const "Posix_TTY_TC_oon": flow_action;

		  type queue_sel = int

		  val iflush = _const "Posix_TTY_TC_iflush": queue_sel;
		  val ioflush = _const "Posix_TTY_TC_ioflush": queue_sel;
		  val oflush = _const "Posix_TTY_TC_oflush": queue_sel;
	       end		  

	    structure Termios =
	       struct
		  type flag = word

		  val iflag = _ffi "Posix_TTY_Termios_iflag": unit -> flag;
		  val oflag = _ffi "Posix_TTY_Termios_oflag": unit -> flag;
		  val cflag = _ffi "Posix_TTY_Termios_cflag": unit -> flag;
		  val lflag = _ffi "Posix_TTY_Termios_lflag": unit -> flag;
		  val cc = _ffi "Posix_TTY_Termios_cc": unit -> cstring;
		  val ospeed =
		     _ffi "Posix_TTY_Termios_cfgetospeed": unit -> speed;
		  val ispeed =
		     _ffi "Posix_TTY_Termios_cfgetispeed": unit -> speed;
		  val setiflag =
		     _ffi "Posix_TTY_Termios_setiflag": flag -> unit;
		  val setoflag =
		     _ffi "Posix_TTY_Termios_setoflag": flag -> unit;
		  val setcflag =
		     _ffi "Posix_TTY_Termios_setcflag": flag -> unit;
		  val setlflag =
		     _ffi "Posix_TTY_Termios_setlflag": flag -> unit;
		  val setospeed =
		     _ffi "Posix_TTY_Termios_setospeed": speed -> int;
		  val setispeed =
		     _ffi "Posix_TTY_Termios_setispeed": speed -> int;
	       end

	    val getattr =
	       _ffi "Posix_TTY_getattr": fd -> int;
	    val setattr =
	       _ffi "Posix_TTY_setattr": fd * TC.set_action -> int;
	    val sendbreak = _ffi "Posix_TTY_sendbreak": fd * int -> int;
	    val drain = _ffi "Posix_TTY_drain": fd -> int;
	    val flush = _ffi "Posix_TTY_flush": fd * TC.queue_sel -> int;
	    val flow = _ffi "Posix_TTY_flow": fd * TC.flow_action -> int;
	    val getpgrp = _ffi "Posix_TTY_getpgrp": fd -> pid;
	    val setpgrp = _ffi "Posix_TTY_setpgrp": fd * pid -> int;
	 end
   end
