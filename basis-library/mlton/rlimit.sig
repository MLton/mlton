(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_RLIMIT =
   sig
      structure RLim : sig
                          type t
                          val castFromSysWord: SysWord.word -> t
                          val castToSysWord: t -> SysWord.word
                       end

      val infinity: RLim.t

      type t

      val coreFileSize: t        (* CORE    max core file size *)
      val cpuTime: t             (* CPU     CPU time in seconds *)
      val dataSize: t            (* DATA    max data size *)
      val fileSize: t            (* FSIZE   Maximum filesize *)
      val numFiles: t            (* NOFILE  max number of open files *)  
      val stackSize: t           (* STACK   max stack size *)
      val virtualMemorySize: t   (* AS      virtual memory limit *)

(* NOT STANDARD *)
      val lockedInMemorySize: t  (* MEMLOCK max locked address space *)
      val numProcesses: t        (* NPROC   max number of processes *)
      val residentSetSize: t     (* RSS     max resident set size *)
(* *)

      val get: t -> {hard: RLim.t, soft: RLim.t}
      val set: t * {hard: RLim.t, soft: RLim.t} -> unit
   end
