(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_REAL =
   sig
      type t

      val fromWord: word -> t
      val fromLargeWord: LargeWord.word -> t
      val toWord: IEEEReal.rounding_mode -> t -> word
      val toLargeWord: IEEEReal.rounding_mode -> t -> LargeWord.word
   end
