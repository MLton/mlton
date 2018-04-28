(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature INT_INF =
   sig
      include INTEGER

      val andb: t * t -> t
      val hash: t -> word
      val log2: t -> Int.t
      val maxPow2ThatDivides: t -> word
      val notb: t -> t
      val orb: t * t -> t
      val xorb: t * t -> t
      val << : t * Pervasive.Word.word -> t
      val ~>> : t * Pervasive.Word.word -> t
   end
