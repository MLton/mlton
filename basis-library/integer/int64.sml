(* Copyright (C) 1999-2003 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Int64:
   sig
      include INTEGER_EXTRA

      val fromWord: word -> int
      val toWord: int -> word
   end =
   struct
      structure P = Primitive.Int64
      structure I = Integer (P)
      open I
      val fromWord = P.fromWord
      val toWord = P.toWord
   end
      

