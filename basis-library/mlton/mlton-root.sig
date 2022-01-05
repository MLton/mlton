(* Copyright (C) 2013,2019,2022 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_ROOT =
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
      val sizeAll: unit -> IntInf.int
   end
