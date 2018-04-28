(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonRlimit: MLTON_RLIMIT =
   struct
      open PrimitiveFFI.MLton.Rlimit
      structure RLim =
         struct
            type t = C_RLim.t
            val castFromSysWord = C_RLim.castFromSysWord
            val castToSysWord = C_RLim.castToSysWord
         end
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
