(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure PosixSignal: POSIX_SIGNAL_EXTRA =
   struct
      open PrimitiveFFI.Posix.Signal
      structure Signal = PrePosix.Signal

      type signal = Signal.t

      val abrt = Signal.fromRep SIGABRT
      val alrm = Signal.fromRep SIGALRM
      val bus = Signal.fromRep SIGBUS
      val chld = Signal.fromRep SIGCHLD
      val cont = Signal.fromRep SIGCONT
      val fpe = Signal.fromRep SIGFPE
      val hup = Signal.fromRep SIGHUP
      val ill = Signal.fromRep SIGILL
      val int = Signal.fromRep SIGINT
      val kill = Signal.fromRep SIGKILL
      val pipe = Signal.fromRep SIGPIPE
      val poll = Signal.fromRep SIGPOLL
      val prof = Signal.fromRep SIGPROF
      val quit = Signal.fromRep SIGQUIT
      val segv = Signal.fromRep SIGSEGV
      val stop = Signal.fromRep SIGSTOP
      val sys = Signal.fromRep SIGSYS
      val term = Signal.fromRep SIGTERM
      val trap = Signal.fromRep SIGTRAP
      val tstp = Signal.fromRep SIGTSTP
      val ttin = Signal.fromRep SIGTTIN
      val ttou = Signal.fromRep SIGTTOU
      val urg = Signal.fromRep SIGURG
      val usr1 = Signal.fromRep SIGUSR1
      val usr2 = Signal.fromRep SIGUSR2
      val vtalrm = Signal.fromRep SIGVTALRM
      val xcpu = Signal.fromRep SIGXCPU
      val xfsz = Signal.fromRep SIGXFSZ

      val fromRep = Signal.fromRep
      val toRep = Signal.toRep

      val repToInt = C_Int.toInt
      val repFromInt = C_Int.fromInt

      val toInt = repToInt o toRep
      val fromInt = fromRep o repFromInt

      val repToWord = C_Int.castToSysWord
      val repFromWord = C_Int.castFromSysWord

      val toWord = repToWord o toRep
      val fromWord = fromRep o repFromWord
   end
