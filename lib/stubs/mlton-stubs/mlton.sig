(* Copyright (C) 2009,2013 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON =
   sig
      val debug: bool
      (* Pointer equality.  The usual caveats about lack of a well-defined
       * semantics.
       *)
      val eq: 'a * 'a -> bool
      (* Structural equality.  Equivalent to SML's polymorphic
       * equality on equality types and a conservative approximation
       * of equivalence other types.
       *)
      val equal: 'a * 'a -> bool
      (* Structural hash. *)
      val hash: 'a -> Word32.word
      val isMLton: bool
      val safe: bool
      val share: 'a -> unit
      val shareAll: unit -> unit
      val size: 'a -> int

      structure Array: MLTON_ARRAY
(*       structure BinIO: MLTON_BIN_IO *)
(*       structure Cont: MLTON_CONT *)
      structure Exn: MLTON_EXN
(*       structure Finalizable: MLTON_FINALIZABLE *)
      structure GC: MLTON_GC
      structure Itimer: MLTON_ITIMER
      structure Platform: MLTON_PLATFORM
      structure ProcEnv: MLTON_PROC_ENV
      structure Process: MLTON_PROCESS
      structure Profile: MLTON_PROFILE
      structure Random: MLTON_RANDOM
      structure Rusage: MLTON_RUSAGE
      structure Signal: MLTON_SIGNAL
      structure TextIO: MLTON_TEXT_IO
      structure Thread: MLTON_THREAD
      structure Vector: MLTON_VECTOR
(*       structure Weak: MLTON_WEAK *)
(*       structure Word: MLTON_WORD *)
(*       structure Word8: MLTON_WORD *)
(*       structure World: MLTON_WORLD *)
   end
