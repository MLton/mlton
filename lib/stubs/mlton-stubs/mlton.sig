(* Copyright (C) 2009,2013,2019,2022 Matthew Fluet.
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
      val size: 'a -> IntInf.int
      val sizeAll: 'a -> IntInf.int

      structure Array: MLTON_ARRAY
      structure Exn: MLTON_EXN
      structure GC: MLTON_GC
      structure Platform: MLTON_PLATFORM
      structure Process: MLTON_PROCESS
      structure Profile: MLTON_PROFILE
      structure Random: MLTON_RANDOM
      structure Rusage: MLTON_RUSAGE
      structure TextIO: MLTON_TEXT_IO
      structure Vector: MLTON_VECTOR
   end
