(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure PosixProcEnv: POSIX_PROC_ENV =
   struct
      structure Prim = PrimitiveFFI.Posix.ProcEnv
      structure Error = PosixError
      structure SysCall = Error.SysCall
      structure CS = CUtil.C_String
      structure CSS = CUtil.C_StringArray

      type pid = C_PId.t
      type uid = C_UId.t
      type gid = C_GId.t
      type file_desc = C_Fd.t

      local
         open Prim
      in
         val getpgrp = getpgrp (* No error checking required *)
         val getegid = getegid (* No error checking required *)
         val geteuid = geteuid (* No error checking required *)
         val getgid = getgid (* No error checking required *)
         val getpid = getpid (* No error checking required *)
         val getppid = getppid (* No error checking required *)
         val getuid = getuid (* No error checking required *)
         val setgid = fn gid => SysCall.simple (fn () => setgid gid)
         val setuid = fn uid => SysCall.simple (fn () => setuid uid)
      end

      fun setsid () = SysCall.simpleResult (Prim.setsid)

      val uidToWord = C_UId.castToSysWord
      val wordToUid = C_UId.castFromSysWord
      val gidToWord = C_GId.castToSysWord
      val wordToGid = C_GId.castFromSysWord

      fun getgroups () =
         SysCall.syscall
         (fn () =>
          let
             val n = Prim.getgroupsN ()
             val a: C_GId.t array = Array.arrayUninit (C_Int.toInt n)
          in
             (Prim.getgroups (n, a), fn n => 
              ArraySlice.toList (ArraySlice.slice (a, 0, SOME (C_Int.toInt n))))
          end)

      fun getlogin () =
         SysCall.syscall'
         ({errVal = CUtil.C_Pointer.null}, fn () =>
          (Prim.getlogin (), fn cs => 
           CS.toString cs))

      fun setpgid {pid, pgid} =
         let
            val pid = case pid of NONE => 0 | SOME pid => pid
            val pgid = case pgid of NONE => 0 | SOME pgid => pgid
         in
            SysCall.simple
            (fn () => Prim.setpgid (pid, pgid))
         end

      fun uname () =
         SysCall.syscall
         (fn () =>
          (Prim.uname (), fn _ =>
           [("sysname", CS.toString (Prim.Uname.getSysName ())),
            ("nodename", CS.toString (Prim.Uname.getNodeName ())),
            ("release", CS.toString (Prim.Uname.getRelease ())),
            ("version", CS.toString (Prim.Uname.getVersion ())),
            ("machine", CS.toString (Prim.Uname.getMachine ()))]))

      val time = Time.now

      local
         local
            infixr 5 ::?
            fun (n,s) ::? l =
               if n = C_Int.fromInt ~1
                  then l
                  else (n,s) :: l
         in
            val sysconfNames =
               (Prim.SC_2_CHAR_TERM,"2_CHAR_TERM") ::?
               (Prim.SC_2_C_BIND,"2_C_BIND") ::?
               (Prim.SC_2_C_DEV,"2_C_DEV") ::?
               (Prim.SC_2_FORT_DEV,"2_FORT_DEV") ::?
               (Prim.SC_2_FORT_RUN,"2_FORT_RUN") ::?
               (Prim.SC_2_LOCALEDEF,"2_LOCALEDEF") ::?
               (Prim.SC_2_PBS,"2_PBS") ::?
               (Prim.SC_2_PBS_ACCOUNTING,"2_PBS_ACCOUNTING") ::?
               (Prim.SC_2_PBS_CHECKPOINT,"2_PBS_CHECKPOINT") ::?
               (Prim.SC_2_PBS_LOCATE,"2_PBS_LOCATE") ::?
               (Prim.SC_2_PBS_MESSAGE,"2_PBS_MESSAGE") ::?
               (Prim.SC_2_PBS_TRACK,"2_PBS_TRACK") ::?
               (Prim.SC_2_SW_DEV,"2_SW_DEV") ::?
               (Prim.SC_2_UPE,"2_UPE") ::?
               (Prim.SC_2_VERSION,"2_VERSION") ::?
               (Prim.SC_ADVISORY_INFO,"ADVISORY_INFO") ::?
               (Prim.SC_AIO_LISTIO_MAX,"AIO_LISTIO_MAX") ::?
               (Prim.SC_AIO_MAX,"AIO_MAX") ::?
               (Prim.SC_AIO_PRIO_DELTA_MAX,"AIO_PRIO_DELTA_MAX") ::?
               (Prim.SC_ARG_MAX,"ARG_MAX") ::?
               (Prim.SC_ASYNCHRONOUS_IO,"ASYNCHRONOUS_IO") ::?
               (Prim.SC_ATEXIT_MAX,"ATEXIT_MAX") ::?
               (Prim.SC_BARRIERS,"BARRIERS") ::?
               (Prim.SC_BC_BASE_MAX,"BC_BASE_MAX") ::?
               (Prim.SC_BC_DIM_MAX,"BC_DIM_MAX") ::?
               (Prim.SC_BC_SCALE_MAX,"BC_SCALE_MAX") ::?
               (Prim.SC_BC_STRING_MAX,"BC_STRING_MAX") ::?
               (Prim.SC_CHILD_MAX,"CHILD_MAX") ::?
               (Prim.SC_CLK_TCK,"CLK_TCK") ::?
               (Prim.SC_CLOCK_SELECTION,"CLOCK_SELECTION") ::?
               (Prim.SC_COLL_WEIGHTS_MAX,"COLL_WEIGHTS_MAX") ::?
               (Prim.SC_CPUTIME,"CPUTIME") ::?
               (Prim.SC_DELAYTIMER_MAX,"DELAYTIMER_MAX") ::?
               (Prim.SC_EXPR_NEST_MAX,"EXPR_NEST_MAX") ::?
               (Prim.SC_FSYNC,"FSYNC") ::?
               (Prim.SC_GETGR_R_SIZE_MAX,"GETGR_R_SIZE_MAX") ::?
               (Prim.SC_GETPW_R_SIZE_MAX,"GETPW_R_SIZE_MAX") ::?
               (Prim.SC_HOST_NAME_MAX,"HOST_NAME_MAX") ::?
               (Prim.SC_IOV_MAX,"IOV_MAX") ::?
               (Prim.SC_IPV6,"IPV6") ::?
               (Prim.SC_JOB_CONTROL,"JOB_CONTROL") ::?
               (Prim.SC_LINE_MAX,"LINE_MAX") ::?
               (Prim.SC_LOGIN_NAME_MAX,"LOGIN_NAME_MAX") ::?
               (Prim.SC_MAPPED_FILES,"MAPPED_FILES") ::?
               (Prim.SC_MEMLOCK,"MEMLOCK") ::?
               (Prim.SC_MEMLOCK_RANGE,"MEMLOCK_RANGE") ::?
               (Prim.SC_MEMORY_PROTECTION,"MEMORY_PROTECTION") ::?
               (Prim.SC_MESSAGE_PASSING,"MESSAGE_PASSING") ::?
               (Prim.SC_MONOTONIC_CLOCK,"MONOTONIC_CLOCK") ::?
               (Prim.SC_MQ_OPEN_MAX,"MQ_OPEN_MAX") ::?
               (Prim.SC_MQ_PRIO_MAX,"MQ_PRIO_MAX") ::?
               (Prim.SC_NGROUPS_MAX,"NGROUPS_MAX") ::?
               (Prim.SC_OPEN_MAX,"OPEN_MAX") ::?
               (Prim.SC_PAGESIZE,"PAGESIZE") ::?
               (Prim.SC_PAGE_SIZE,"PAGE_SIZE") ::?
               (Prim.SC_PRIORITIZED_IO,"PRIORITIZED_IO") ::?
               (Prim.SC_PRIORITY_SCHEDULING,"PRIORITY_SCHEDULING") ::?
               (Prim.SC_RAW_SOCKETS,"RAW_SOCKETS") ::?
               (Prim.SC_READER_WRITER_LOCKS,"READER_WRITER_LOCKS") ::?
               (Prim.SC_REALTIME_SIGNALS,"REALTIME_SIGNALS") ::?
               (Prim.SC_REGEXP,"REGEXP") ::?
               (Prim.SC_RE_DUP_MAX,"RE_DUP_MAX") ::?
               (Prim.SC_RTSIG_MAX,"RTSIG_MAX") ::?
               (Prim.SC_SAVED_IDS,"SAVED_IDS") ::?
               (Prim.SC_SEMAPHORES,"SEMAPHORES") ::?
               (Prim.SC_SEM_NSEMS_MAX,"SEM_NSEMS_MAX") ::?
               (Prim.SC_SEM_VALUE_MAX,"SEM_VALUE_MAX") ::?
               (Prim.SC_SHARED_MEMORY_OBJECTS,"SHARED_MEMORY_OBJECTS") ::?
               (Prim.SC_SHELL,"SHELL") ::?
               (Prim.SC_SIGQUEUE_MAX,"SIGQUEUE_MAX") ::?
               (Prim.SC_SPAWN,"SPAWN") ::?
               (Prim.SC_SPIN_LOCKS,"SPIN_LOCKS") ::?
               (Prim.SC_SPORADIC_SERVER,"SPORADIC_SERVER") ::?
               (Prim.SC_SS_REPL_MAX,"SS_REPL_MAX") ::?
               (Prim.SC_STREAM_MAX,"STREAM_MAX") ::?
               (Prim.SC_SYMLOOP_MAX,"SYMLOOP_MAX") ::?
               (Prim.SC_SYNCHRONIZED_IO,"SYNCHRONIZED_IO") ::?
               (Prim.SC_THREADS,"THREADS") ::?
               (Prim.SC_THREAD_ATTR_STACKADDR,"THREAD_ATTR_STACKADDR") ::?
               (Prim.SC_THREAD_ATTR_STACKSIZE,"THREAD_ATTR_STACKSIZE") ::?
               (Prim.SC_THREAD_CPUTIME,"THREAD_CPUTIME") ::?
               (Prim.SC_THREAD_DESTRUCTOR_ITERATIONS,"THREAD_DESTRUCTOR_ITERATIONS") ::?
               (Prim.SC_THREAD_KEYS_MAX,"THREAD_KEYS_MAX") ::?
               (Prim.SC_THREAD_PRIORITY_SCHEDULING,"THREAD_PRIORITY_SCHEDULING") ::?
               (Prim.SC_THREAD_PRIO_INHERIT,"THREAD_PRIO_INHERIT") ::?
               (Prim.SC_THREAD_PRIO_PROTECT,"THREAD_PRIO_PROTECT") ::?
               (Prim.SC_THREAD_PROCESS_SHARED,"THREAD_PROCESS_SHARED") ::?
               (Prim.SC_THREAD_SAFE_FUNCTIONS,"THREAD_SAFE_FUNCTIONS") ::?
               (Prim.SC_THREAD_SPORADIC_SERVER,"THREAD_SPORADIC_SERVER") ::?
               (Prim.SC_THREAD_STACK_MIN,"THREAD_STACK_MIN") ::?
               (Prim.SC_THREAD_THREADS_MAX,"THREAD_THREADS_MAX") ::?
               (Prim.SC_TIMEOUTS,"TIMEOUTS") ::?
               (Prim.SC_TIMERS,"TIMERS") ::?
               (Prim.SC_TIMER_MAX,"TIMER_MAX") ::?
               (Prim.SC_TRACE,"TRACE") ::?
               (Prim.SC_TRACE_EVENT_FILTER,"TRACE_EVENT_FILTER") ::?
               (Prim.SC_TRACE_EVENT_NAME_MAX,"TRACE_EVENT_NAME_MAX") ::?
               (Prim.SC_TRACE_INHERIT,"TRACE_INHERIT") ::?
               (Prim.SC_TRACE_LOG,"TRACE_LOG") ::?
               (Prim.SC_TRACE_NAME_MAX,"TRACE_NAME_MAX") ::?
               (Prim.SC_TRACE_SYS_MAX,"TRACE_SYS_MAX") ::?
               (Prim.SC_TRACE_USER_EVENT_MAX,"TRACE_USER_EVENT_MAX") ::?
               (Prim.SC_TTY_NAME_MAX,"TTY_NAME_MAX") ::?
               (Prim.SC_TYPED_MEMORY_OBJECTS,"TYPED_MEMORY_OBJECTS") ::?
               (Prim.SC_TZNAME_MAX,"TZNAME_MAX") ::?
               (Prim.SC_V6_ILP32_OFF32,"V6_ILP32_OFF32") ::?
               (Prim.SC_V6_ILP32_OFFBIG,"V6_ILP32_OFFBIG") ::?
               (Prim.SC_V6_LP64_OFF64,"V6_LP64_OFF64") ::?
               (Prim.SC_V6_LPBIG_OFFBIG,"V6_LPBIG_OFFBIG") ::?
               (Prim.SC_VERSION,"VERSION") ::?
               (Prim.SC_XBS5_ILP32_OFF32,"XBS5_ILP32_OFF32") ::?
               (Prim.SC_XBS5_ILP32_OFFBIG,"XBS5_ILP32_OFFBIG") ::?
               (Prim.SC_XBS5_LP64_OFF64,"XBS5_LP64_OFF64") ::?
               (Prim.SC_XBS5_LPBIG_OFFBIG,"XBS5_LPBIG_OFFBIG") ::?
               (Prim.SC_XOPEN_CRYPT,"XOPEN_CRYPT") ::?
               (Prim.SC_XOPEN_ENH_I18N,"XOPEN_ENH_I18N") ::?
               (Prim.SC_XOPEN_LEGACY,"XOPEN_LEGACY") ::?
               (Prim.SC_XOPEN_REALTIME,"XOPEN_REALTIME") ::?
               (Prim.SC_XOPEN_REALTIME_THREADS,"XOPEN_REALTIME_THREADS") ::?
               (Prim.SC_XOPEN_SHM,"XOPEN_SHM") ::?
               (Prim.SC_XOPEN_STREAMS,"XOPEN_STREAMS") ::?
               (Prim.SC_XOPEN_UNIX,"XOPEN_UNIX") ::?
               (Prim.SC_XOPEN_VERSION,"XOPEN_VERSION") ::?
               []
         end
      in
         fun sysconf s =
            case List.find (fn (_, s') => s = s') sysconfNames of
               NONE => Error.raiseSys Error.inval
             | SOME (n, _) =>
                  (SysWord.fromLargeInt o C_Long.toLarge o SysCall.simpleResult')
                  ({errVal = C_Long.fromInt ~1}, fn () => Prim.sysconf n)
      end

      local
         structure Times = Prim.Times

         val clocksPerSec = 
            (* syconf is not implemented on MinGW; 
             * we don't want a SysErr during Basis Library initialization. 
             *)
            if (let open Primitive.MLton.Platform.OS in host = MinGW end)
               then LargeInt.zero
            else SysWord.toLargeIntX (sysconf "CLK_TCK")

         fun cvt (clocks: C_Clock.t) =
            Time.fromTicks (LargeInt.quot
                            (LargeInt.* (C_Clock.toLargeInt clocks,
                                         Time.ticksPerSecond),
                             clocksPerSec))
      in
         fun times () =
            SysCall.syscall'
            ({errVal = C_Clock.castFromFixedInt ~1}, fn () =>
             (Prim.times (), fn elapsed =>
              {elapsed = cvt elapsed,
               utime = cvt (Times.getUTime ()), 
               stime = cvt (Times.getSTime ()), 
               cutime = cvt (Times.getCUTime ()), 
               cstime = cvt (Times.getCSTime ())}))
      end

      fun environ () = CSS.toList (Prim.environGet ())

      fun getenv name =
         let
            val cs = Prim.getenv (NullString.nullTerm name)
         in
            if CUtil.C_Pointer.isNull cs
               then NONE
            else SOME (CS.toString cs)
         end

      fun ctermid () = CS.toString (Prim.ctermid ())

      fun isatty fd = (Prim.isatty fd) <> C_Int.zero

      fun ttyname fd =
         SysCall.syscall'
         ({errVal = CUtil.C_Pointer.null}, fn () =>
          (Prim.ttyname fd, fn cs => 
           CS.toString cs))
   end
