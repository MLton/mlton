(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonRlimit: MLTON_RLIMIT =
   struct
      open PrimitiveFFI.MLton.Rlimit
      structure RLim = C_RLim
      type t = C_Int.t

      val get =
         fn (r: t) =>
         PosixError.SysCall.syscall
         (fn () =>
          (get r, fn _ => 
           {hard = getHard (),
            soft = getSoft ()}))

      val set =
         fn (r: t, {hard, soft}) =>
         PosixError.SysCall.simple
         (fn () => set (r, hard, soft))

      val infinity = INFINITY

      val coreFileSize = CORE
      val cpuTime = CPU
      val dataSize = DATA
      val fileSize = FSIZE
      val numFiles = NOFILE
      val stackSize = STACK
      val virtualMemorySize = AS

(* NOT STANDARD *)
      val lockedInMemorySize = MEMLOCK
      val numProcesses = NPROC
      val residentSetSize = RSS
(* *)

   end
