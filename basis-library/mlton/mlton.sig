(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.int
type word = Word.word
   
signature MLTON =
   sig
      val cleanAtExit: unit -> unit
(*      val deserialize: Word8Vector.vector -> 'a *)
      (* Pointer equality.  The usual caveats about lack of a well-defined
       * semantics.
       *)
      val eq: 'a * 'a -> bool
      val errno: unit -> int (* the value of the C errno global *)
      val isMLton: bool
      val safe: bool
(*      val serialize: 'a -> Word8Vector.vector *)
      val size: 'a -> int

      structure Array: MLTON_ARRAY
      structure BinIO: MLTON_BIN_IO
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
      structure Word:
	 sig
	    include MLTON_WORD
	    val addCheck: word * Word.word -> word (* may raise Overflow *)
	    val mulCheck: word * Word.word -> word (* may raise Overflow *)
	 end where type word = Word.word
      structure Word8: MLTON_WORD where type word = Word8.word
      structure World: MLTON_WORLD
   end
