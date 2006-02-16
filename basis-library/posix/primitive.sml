(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure PosixPrimitive =
   struct
      type cstring = Pointer.t
      type cstringArray = Pointer.t

      type uid = word
      type gid = word
      type size = int
      type ssize = int
      type mode = word
      type time = int

      structure FileDesc = Primitive.FileDesc
      type file_desc = FileDesc.t
      type fd = file_desc
         
      structure Error =
         struct
            type syserror = int

            val getErrno = _import "Posix_Error_getErrno": unit -> int;
            val clearErrno = _import "Posix_Error_clearErrno": unit -> unit;
            val strerror = _import "Posix_Error_strerror": syserror -> cstring;

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
            open Primitive.Signal
               
            val abrt = _const "Posix_Signal_abrt": t;
            val alrm = _const "Posix_Signal_alrm": t;
            val bus = _const "Posix_Signal_bus": t;
            val chld = _const "Posix_Signal_chld": t;
            val cont = _const "Posix_Signal_cont": t;
            val fpe = _const "Posix_Signal_fpe": t;
            val hup = _const "Posix_Signal_hup": t;
            val ill = _const "Posix_Signal_ill": t;
            val int = _const "Posix_Signal_int": t;
            val kill = _const "Posix_Signal_kill": t;
            val pipe = _const "Posix_Signal_pipe": t;
            val prof = _const "Posix_Signal_prof": t;
            val quit = _const "Posix_Signal_quit": t;
            val segv = _const "Posix_Signal_segv": t;
            val stop = _const "Posix_Signal_stop": t;
            val term = _const "Posix_Signal_term": t;
            val tstp = _const "Posix_Signal_tstp": t;
            val ttin = _const "Posix_Signal_ttin": t;
            val ttou = _const "Posix_Signal_ttou": t;
            val usr1 = _const "Posix_Signal_usr1": t;
            val usr2 = _const "Posix_Signal_usr2": t;
            val vtalrm = _const "Posix_Signal_vtalrm": t;

            val block = _const "Posix_Signal_block": how;
            val default = _import "Posix_Signal_default": t -> int;
            val handleGC = _import "Posix_Signal_handleGC": unit -> unit;
            val handlee = _import "Posix_Signal_handle": t -> int;
            val ignore = _import "Posix_Signal_ignore": t -> int;
            val isDefault =
               _import "Posix_Signal_isDefault": t * bool ref -> int;
            val isGCPending = _import "Posix_Signal_isGCPending": unit -> bool;
            val isPending = _import "Posix_Signal_isPending": t -> bool;
            val numSignals = _const "Posix_Signal_numSignals": int;
            val resetPending = _import "Posix_Signal_resetPending": unit -> unit;
            val setmask = _const "Posix_Signal_setmask": how;
            val sigaddset = _import "Posix_Signal_sigaddset": t -> int;
            val sigdelset = _import "Posix_Signal_sigdelset": t -> int;
            val sigemptyset = _import "Posix_Signal_sigemptyset": unit -> int;
            val sigfillset = _import "Posix_Signal_sigfillset": unit -> int;
            val sigismember = _import "Posix_Signal_sigismember": t -> int;
            val sigprocmask = _import "Posix_Signal_sigprocmask": how -> int;
            val suspend = _import "Posix_Signal_suspend": unit -> unit;
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

            structure Status = Primitive.Status
            
            val alarm = _import "Posix_Process_alarm": int -> int;
            val exece =
               _import "Posix_Process_exece"
               : NullString.t * NullString.t array * NullString.t array -> int;
            val execp =
               _import "Posix_Process_execp"
               : NullString.t * NullString.t array -> int;
            val exit = _import "Posix_Process_exit": int -> unit;
            val exitStatus = _import "Posix_Process_exitStatus": Status.t -> int;
            val fork = _import "Posix_Process_fork": unit -> Pid.t;
            val ifExited = _import "Posix_Process_ifExited": Status.t -> bool;
            val ifSignaled = _import "Posix_Process_ifSignaled"
               : Status.t -> bool;
            val ifStopped = _import "Posix_Process_ifStopped": Status.t -> bool;
            val kill = _import "Posix_Process_kill": Pid.t * Signal.t -> int;
            val nanosleep =
               _import "Posix_Process_nanosleep": int ref * int ref -> int;
            val pause = _import "Posix_Process_pause": unit -> int;
(*          val sleep = _import "Posix_Process_sleep": int -> int; *)
            val stopSig = _import "Posix_Process_stopSig": Status.t -> Signal.t;
            val system =
               _import "Posix_Process_system": NullString.t -> Status.t;
            val termSig = _import "Posix_Process_termSig": Status.t -> Signal.t;
            val waitpid =
               _import "Posix_Process_waitpid"
               : Pid.t * Status.t ref * int -> Pid.t;
            val cwait =
               if let
                     open Primitive.MLton.Platform.OS
                  in
                     case host of
                        Cygwin => true
                      | MinGW => true
                      | _ => false
                  end
                  then _import "MLton_Process_cwait": Pid.t * Status.t ref -> Pid.t;
               else fn _ => raise Fail "cwait not defined"
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
               
            type gid = gid
            type uid = uid
            type file_desc = file_desc

            val getegid = _import "Posix_ProcEnv_getegid": unit -> gid;
            val geteuid = _import "Posix_ProcEnv_geteuid": unit -> uid;
            val getgid = _import "Posix_ProcEnv_getgid": unit -> gid;
            val getgroups = _import "Posix_ProcEnv_getgroups": gid array -> int;
            val getlogin = _import "Posix_ProcEnv_getlogin": unit -> cstring;
            val getpgrp = _import "Posix_ProcEnv_getpgrp": unit -> Pid.t;
            val getpid = _import "Posix_ProcEnv_getpid": unit -> Pid.t;
            val getppid = _import "Posix_ProcEnv_getppid": unit -> Pid.t;
            val getuid = _import "Posix_ProcEnv_getuid": unit -> uid;
            val setenv =
               _import "Posix_ProcEnv_setenv": NullString.t * NullString.t -> int;
            val setgid = _import "Posix_ProcEnv_setgid": gid -> int;
            val setgroups = _import "Posix_ProcEnv_setgroups": gid array -> int;
            val setpgid = _import "Posix_ProcEnv_setpgid": Pid.t * Pid.t -> int;
            val setsid = _import "Posix_ProcEnv_setsid": unit -> Pid.t;
            val setuid = _import "Posix_ProcEnv_setuid": uid -> int;

            structure Uname =
               struct
                  val uname = _import "Posix_ProcEnv_Uname_uname": unit -> int;
                  val sysname =
                     _import "Posix_ProcEnv_Uname_sysname": unit -> cstring;
                  val nodename =
                     _import "Posix_ProcEnv_Uname_nodename": unit -> cstring;
                  val release =
                     _import "Posix_ProcEnv_Uname_release": unit -> cstring;
                  val version =
                     _import "Posix_ProcEnv_Uname_version": unit -> cstring;
                  val machine =
                     _import "Posix_ProcEnv_Uname_machine": unit -> cstring;
               end

            type clock_t = word
               
            structure Tms =
               struct
                  val utime = _import "Posix_ProcEnv_Tms_utime": unit -> clock_t;
                  val stime = _import "Posix_ProcEnv_Tms_stime": unit -> clock_t;
                  val cutime = _import "Posix_ProcEnv_Tms_cutime": unit -> clock_t;
                  val cstime = _import "Posix_ProcEnv_Tms_cstime": unit -> clock_t;
               end

            val ctermid = _import "Posix_ProcEnv_ctermid": unit -> cstring;
            val environ = #1 _symbol "Posix_ProcEnv_environ": cstringArray GetSet.t; ()
            val getenv = _import "Posix_ProcEnv_getenv": NullString.t -> cstring;
            val isatty = _import "Posix_ProcEnv_isatty": fd -> bool;
            val sysconf = _import "Posix_ProcEnv_sysconf": int -> int;
            val times = _import "Posix_ProcEnv_times": unit -> clock_t;
            val ttyname = _import "Posix_ProcEnv_ttyname": fd -> cstring;
         end 
      
      structure FileSys =
         struct
            type file_desc = file_desc

            type ino = int
            type dev = word
            type uid = uid
            type gid = gid

            structure S =
               struct
                  type mode = word
(*                val ifsock = _const "Posix_FileSys_S_ifsock": mode; *)
(*                val iflnk = _const "Posix_FileSys_S_iflnk": mode; *)
(*                val ifreg = _const "Posix_FileSys_S_ifreg": mode; *)
(*                val ifblk = _const "Posix_FileSys_S_ifblk": mode; *)
(*                val ifdir = _const "Posix_FileSys_S_ifdir": mode; *)
(*                val ifchr = _const "Posix_FileSys_S_ifchr": mode; *)
(*                val ififo = _const "Posix_FileSys_S_ififo": mode; *)
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
                  type dirstream = Pointer.t

                  val closedir =
                     _import "Posix_FileSys_Dirstream_closedir": dirstream -> int;
                  val opendir =
                     _import "Posix_FileSys_Dirstream_opendir"
                     : NullString.t -> dirstream;
                  val readdir =
                     _import "Posix_FileSys_Dirstream_readdir"
                     : dirstream -> cstring;
                  val rewinddir =
                     _import "Posix_FileSys_Dirstream_rewinddir"
                     : dirstream -> unit;
               end

            structure Stat =
               struct
                  val dev = _import "Posix_FileSys_Stat_dev": unit -> dev;
                  val ino = _import "Posix_FileSys_Stat_ino": unit -> ino;
                  val mode = _import "Posix_FileSys_Stat_mode": unit -> word;
                  val nlink = _import "Posix_FileSys_Stat_nlink": unit -> int;
                  val uid = _import "Posix_FileSys_Stat_uid": unit -> uid;
                  val gid = _import "Posix_FileSys_Stat_gid": unit -> gid;
                  val size =
                     _import "Posix_FileSys_Stat_size": unit -> Position.int;
                  val atime =
                     _import "Posix_FileSys_Stat_atime": unit -> time;
                  val mtime =
                     _import "Posix_FileSys_Stat_mtime": unit -> time;
                  val ctime =
                     _import "Posix_FileSys_Stat_ctime": unit -> time;
                  val fstat = _import "Posix_FileSys_Stat_fstat": fd -> int;
                  val lstat =
                     _import "Posix_FileSys_Stat_lstat": NullString.t -> int;
                  val stat =
                     _import "Posix_FileSys_Stat_stat": NullString.t -> int;
               end

            structure Utimbuf =
               struct
                  val setActime =
                     _import "Posix_FileSys_Utimbuf_setActime": time -> unit;
                  val setModtime =
                     _import "Posix_FileSys_Utimbuf_setModTime": time -> unit;
                  val utime =
                     _import "Posix_FileSys_Utimbuf_utime": NullString.t -> int;
               end

            val access =
               _import "Posix_FileSys_access": NullString.t * word -> int;
            val chdir = _import "Posix_FileSys_chdir": NullString.t -> int;
            val chmod =
               _import "Posix_FileSys_chmod": NullString.t * mode -> int;
            val chown =
               _import "Posix_FileSys_chown": NullString.t * uid * gid -> int;
            val fchmod =
               _import "Posix_FileSys_fchmod": fd * mode -> int;
            val fchown =
               _import "Posix_FileSys_fchown": fd * uid * gid -> int;
            val fpathconf =
               _import "Posix_FileSys_fpathconf": fd * int -> int;
            val ftruncate =
               _import "Posix_FileSys_ftruncate": fd * Position.int -> int;
            val getcwd =
               _import "Posix_FileSys_getcwd": char array * size -> cstring;
            val link =
               _import "Posix_FileSys_link": NullString.t * NullString.t -> int;
            val mkdir =
               _import "Posix_FileSys_mkdir": NullString.t * word -> int;
            val mkfifo =
               _import "Posix_FileSys_mkfifo": NullString.t * word -> int;
            val openn =
               _import "Posix_FileSys_open": NullString.t * word * mode -> int;
            val pathconf =
               _import "Posix_FileSys_pathconf": NullString.t * int -> int;
            val readlink =
               _import "Posix_FileSys_readlink"
               : NullString.t * word8 array * int -> int;
            val rename =
               _import "Posix_FileSys_rename": NullString.t * NullString.t -> int;
            val rmdir = _import "Posix_FileSys_rmdir": NullString.t -> int;
            val symlink =
               _import "Posix_FileSys_symlink"
               : NullString.t * NullString.t -> int;
            val umask = _import "Posix_FileSys_umask": word -> word;
            val unlink = _import "Posix_FileSys_unlink": NullString.t -> int;

            structure ST =
               struct
                  val isDir = _import "Posix_FileSys_ST_isDir": word -> bool;
                  val isChr = _import "Posix_FileSys_ST_isChr": word -> bool;
                  val isBlk = _import "Posix_FileSys_ST_isBlk": word -> bool;
                  val isReg = _import "Posix_FileSys_ST_isReg": word -> bool;
                  val isFIFO =
                     _import "Posix_FileSys_ST_isFIFO": word -> bool;
                  val isLink =
                     _import "Posix_FileSys_ST_isLink": word -> bool;
                  val isSock =
                     _import "Posix_FileSys_ST_isSock": word -> bool;
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
(*          val F_GETOWN = _const "Posix_IO_F_GETOWN": int; *)
(*          val F_SETOWN = _const "Posix_IO_F_SETOWN": int; *)
            val O_ACCMODE = _const "Posix_IO_O_ACCMODE": word;
            val SEEK_SET = _const "Posix_IO_SEEK_SET": int;
            val SEEK_CUR = _const "Posix_IO_SEEK_CUR": int;
            val SEEK_END = _const "Posix_IO_SEEK_END": int;

            structure FD =
               struct
                  type flags = word
                  val cloexec = _const "Posix_IO_FD_cloexec": flags;
               end
            
            type file_desc = file_desc

            structure FLock =
               struct
                  val fcntl = _import "Posix_IO_FLock_fcntl": fd * int -> int;
                  val typ = _import "Posix_IO_FLock_type": unit -> int;
                  val whence = _import "Posix_IO_FLock_whence": unit -> int;
                  val start =
                     _import "Posix_IO_FLock_start": unit -> Position.int;
                  val len =
                     _import "Posix_IO_FLock_len": unit -> Position.int;
                  val pid = _import "Posix_IO_FLock_pid": unit -> Pid.t;
                  val setType = _import "Posix_IO_FLock_setType": int -> unit;
                  val setWhence =
                     _import "Posix_IO_FLock_setWhence": int -> unit;
                  val setStart =
                     _import "Posix_IO_FLock_setStart": Position.int -> unit;
                  val setLen =
                     _import "Posix_IO_FLock_setLen": Position.int -> unit;
(*                val setPid = _import "Posix_IO_FLock_setPid": Pid.t -> unit; *)
               end
            
            val close = _import "Posix_IO_close": fd -> int;
            val dup = _import "Posix_IO_dup": fd -> int;
            val dup2 = _import "Posix_IO_dup2": fd * fd -> int;
            val fcntl2 = _import "Posix_IO_fcntl2": fd * int -> int;
            val fcntl3 = _import "Posix_IO_fcntl3": fd * int * int -> int;
            val fsync = _import "Posix_IO_fsync": fd -> int;
            val lseek =
               _import "Posix_IO_lseek": fd * Position.int * int -> Position.int;
            val pipe = _import "Posix_IO_pipe": fd array -> int;
            val readChar =
               _import "Posix_IO_read": fd * char array * int * size -> ssize;
            val setbin = 
               if let
                     open Primitive.MLton.Platform.OS
                  in
                     case host of
                        MinGW => true
                      | _ => false
                  end
                  then _import "Posix_IO_setbin": fd -> unit;
               else fn _ => raise Fail "setbin not defined"
            val settext = 
               if let
                     open Primitive.MLton.Platform.OS
                  in
                     case host of
                        MinGW => true
                      | _ => false
                  end
                  then _import "Posix_IO_settext": fd -> unit;
               else fn _ => raise Fail "settext not defined"
            val writeChar =
               _import "Posix_IO_write": fd * char array * int * size -> ssize;
            val writeCharVec =
               _import "Posix_IO_write": fd * char vector * int * size -> ssize;
            val readWord8 =
               _import "Posix_IO_read": fd * word8 array * int * size -> ssize;
            val writeWord8 =
               _import "Posix_IO_write": fd * word8 array * int * size -> ssize;
            val writeWord8Vec =
               _import "Posix_IO_write": fd * word8 vector * int * size -> ssize;
         end           

      structure SysDB =
         struct
            type gid = gid
            type uid = uid

            structure Passwd =
               struct
                  val name = _import "Posix_SysDB_Passwd_name": unit -> cstring;
                  val uid = _import "Posix_SysDB_Passwd_uid": unit -> uid;
                  val gid = _import "Posix_SysDB_Passwd_gid": unit -> gid;
                  val dir = _import "Posix_SysDB_Passwd_dir": unit -> cstring;
                  val shell =
                     _import "Posix_SysDB_Passwd_shell": unit -> cstring;
               end

            val getpwnam = _import "Posix_SysDB_getpwnam": NullString.t -> bool;
            val getpwuid = _import "Posix_SysDB_getpwuid": uid -> bool;

            structure Group =
               struct
                  val name = _import "Posix_SysDB_Group_name": unit -> cstring;
                  val gid = _import "Posix_SysDB_Group_gid": unit -> gid;
                  val mem =
                     _import "Posix_SysDB_Group_mem": unit -> cstringArray;
               end

            val getgrgid = _import "Posix_SysDB_getgrgid": gid -> bool;
            val getgrnam = _import "Posix_SysDB_getgrnam": NullString.t -> bool;
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
               
            type file_desc = file_desc

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

                  val iflag = _import "Posix_TTY_Termios_iflag": unit -> flag;
                  val oflag = _import "Posix_TTY_Termios_oflag": unit -> flag;
                  val cflag = _import "Posix_TTY_Termios_cflag": unit -> flag;
                  val lflag = _import "Posix_TTY_Termios_lflag": unit -> flag;
                  val cc = _import "Posix_TTY_Termios_cc": unit -> cstring;
                  val ospeed =
                     _import "Posix_TTY_Termios_cfgetospeed": unit -> speed;
                  val ispeed =
                     _import "Posix_TTY_Termios_cfgetispeed": unit -> speed;
                  val setiflag =
                     _import "Posix_TTY_Termios_setiflag": flag -> unit;
                  val setoflag =
                     _import "Posix_TTY_Termios_setoflag": flag -> unit;
                  val setcflag =
                     _import "Posix_TTY_Termios_setcflag": flag -> unit;
                  val setlflag =
                     _import "Posix_TTY_Termios_setlflag": flag -> unit;
                  val setospeed =
                     _import "Posix_TTY_Termios_setospeed": speed -> int;
                  val setispeed =
                     _import "Posix_TTY_Termios_setispeed": speed -> int;
               end

            val drain = _import "Posix_TTY_drain": fd -> int;
            val flow = _import "Posix_TTY_flow": fd * TC.flow_action -> int;
            val flush = _import "Posix_TTY_flush": fd * TC.queue_sel -> int;
            val getattr = _import "Posix_TTY_getattr": fd -> int;
            val getpgrp = _import "Posix_TTY_getpgrp": fd -> Pid.t;
            val sendbreak = _import "Posix_TTY_sendbreak": fd * int -> int;
            val setattr = _import "Posix_TTY_setattr": fd * TC.set_action -> int;
            val setpgrp = _import "Posix_TTY_setpgrp": fd * Pid.t -> int;
         end
   end
