(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON =
   sig
(*      val cleanAtExit: unit -> unit *)
      val debug: bool
(*      val deserialize: Word8Vector.vector -> 'a *)
      (* Pointer equality.  The usual caveats about lack of a well-defined
       * semantics.
       *)
      val eq: 'a * 'a -> bool
(*      val errno: unit -> int *) (* the value of the C errno global *)
      val isMLton: bool
      val safe: bool
(*      val serialize: 'a -> Word8Vector.vector *)
      val share: 'a -> unit
      val shareAll: unit -> unit
      val size: 'a -> int

      structure Array: MLTON_ARRAY
      structure BinIO: MLTON_BIN_IO
      structure CallStack: MLTON_CALL_STACK
      structure Cont: MLTON_CONT
      structure Exn: MLTON_EXN
      structure Finalizable: MLTON_FINALIZABLE
      structure GC: MLTON_GC
      structure IntInf: MLTON_INT_INF
      structure Itimer: MLTON_ITIMER
      structure Platform: MLTON_PLATFORM
      structure Pointer: MLTON_POINTER
      structure ProcEnv: MLTON_PROC_ENV
      structure Process: MLTON_PROCESS
      structure Profile: MLTON_PROFILE
(*      structure Ptrace: MLTON_PTRACE *)
      structure Random: MLTON_RANDOM
      structure Rlimit: MLTON_RLIMIT
      structure Rusage: MLTON_RUSAGE
      structure Signal: MLTON_SIGNAL
      structure Socket: MLTON_SOCKET
      structure Syslog: MLTON_SYSLOG
      structure TextIO: MLTON_TEXT_IO
      structure Thread: MLTON_THREAD
      structure Vector: MLTON_VECTOR
      structure Weak: MLTON_WEAK
      structure Word: MLTON_WORD
      structure Word8: MLTON_WORD
      structure World: MLTON_WORLD
   end
