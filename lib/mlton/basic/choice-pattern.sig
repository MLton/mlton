(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.t
   
signature CHOICE_PATTERN =
   sig
      (* expand "ab{c{d,e},f{gh}}{i,j}" =
       * ["abcdi", "abcdj", "abcei", "abcej", "abfghi", "abfghj"]
       *)
      val expand: string -> string list Result.t
   end
