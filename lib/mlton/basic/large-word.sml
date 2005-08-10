(* Copyright (C) 2003-2003 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

structure LargeWord: WORD =
   struct
      open Pervasive.LargeWord

      structure Z = FixWord (Pervasive.LargeWord)
      open Z

      val equals: t * t -> bool = op =
   end
