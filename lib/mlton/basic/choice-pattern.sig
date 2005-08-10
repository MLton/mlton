(* Copyright (C) 2002-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.t
   
signature CHOICE_PATTERN =
   sig
      (* expand "ab{c{d,e},f{gh}}{i,j}" =
       * ["abcdi", "abcdj", "abcei", "abcej", "abfghi", "abfghj"]
       *)
      val expand: string -> string list Result.t
   end
