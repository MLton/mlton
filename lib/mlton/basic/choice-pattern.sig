(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CHOICE_PATTERN =
   sig
      (* expand "ab{c{d,e},f{gh}}{i,j}" =
       * ["abcdi", "abcdj", "abcei", "abcej", "abfghi", "abfghj"]
       *)
      val expand: string -> string list Result.t
   end
