(* Copyright (C) 2003-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure LargeWord: WORD =
   struct
      open Pervasive.LargeWord

      structure Z = FixWord (Pervasive.LargeWord)
      open Z

      val equals: t * t -> bool = op =
   end
