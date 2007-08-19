(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure PosixSignal: POSIX_SIGNAL_EXTRA =
   struct
      open PrimitiveFFI.Posix.Signal

      type signal = C_Int.t

      val abrt = SIGABRT
      val alrm = SIGALRM
      val bus = SIGBUS
      val chld = SIGCHLD
      val cont = SIGCONT
      val fpe = SIGFPE
      val hup = SIGHUP
      val ill = SIGILL
      val int = SIGINT
      val kill = SIGKILL
      val pipe = SIGPIPE
      val poll = SIGPOLL
      val prof = SIGPROF
      val quit = SIGQUIT
      val segv = SIGSEGV
      val stop = SIGSTOP
      val sys = SIGSYS
      val term = SIGTERM
      val trap = SIGTRAP
      val tstp = SIGTSTP
      val ttin = SIGTTIN
      val ttou = SIGTTOU
      val urg = SIGURG
      val usr1 = SIGUSR1
      val usr2 = SIGUSR2
      val vtalrm = SIGVTALRM
      val xcpu = SIGXCPU
      val xfsz = SIGXFSZ

      val toInt = C_Int.toInt
      val fromInt = C_Int.fromInt

      val toWord = SysWord.fromInt o toInt
      val fromWord = fromInt o SysWord.toInt
   end
